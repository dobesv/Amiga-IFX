OPT MODULE
OPT EXPORT

->
-> sound.m - simplify playing sounds in E
->

-> Flag ID's
EXPORT SET 
	SNDF_NO_DATATYPES,		-> Attempt to use datatypes
	SNDF_PLAY_ASYNC,		-> Not implemented
	SNDF_DUMMY

MODULE 'devices/audio'
MODULE 'exec/ports', 'exec/lists', 'exec/nodes', 'exec/io', 'exec/execbase', 'exec'
MODULE 'exec/memory'
MODULE 'datatypes/datatypes', 'datatypes/datatypesclass', 'datatypes'
MODULE 'graphics', 'graphics/gfxbase'
MODULE 'dos', 'dos/dos'
MODULE 'utility/tagitem'
MODULE 'datatypes/soundclass'
MODULE 'amigalib/io'
MODULE 'tools/async'

	-> The sound object <-
EXPORT OBJECT sound
	filename	:PTR TO CHAR	-> Path of file to play
	type		:LONG
	data		:PTR TO CHAR
	length		:LONG
	period		:LONG
	volume		:INT
	cycles		:INT
ENDOBJECT

	-> Auxiliary Objects <-
OBJECT channel
	current	:PTR TO ioaudio
	sound	:PTR TO sound
ENDOBJECT

OBJECT chans
	left_a	:PTR TO channel
	left_b	:PTR TO channel
	right_a	:PTR TO channel
	right_b	:PTR TO channel
ENDOBJECT

-> Global objects and variables <-
EXPORT DEF clock
EXPORT DEF channels:PTR TO chans

-> Sound methods <-

EXPORT PROC load(fname:PTR TO CHAR, flags=NIL) OF sound HANDLE
	DEF d,l,p,v,c				-> We'll use these to dodge E's annoying incapabilities
	DEF obj=NIL					-> Datatypes stuff
	DEF file=NIL, 
		data=NIL:PTR TO INT		-> RAW/8SVX stuff
	DEF n
	
	IF fname
		->
		-> Check if the file exists
		->
		
		IF FileLength(fname)=-1 THEN Throw("FILE", "BAD")
		
		->
		-> Load it
		->
		
		IF (flags AND SNDF_NO_DATATYPES)
			IF datatypesbase THEN CloseLibrary(datatypesbase)
			datatypesbase := NIL
		ELSE
			datatypesbase := OpenLibrary('datatypes.library', 39)
		ENDIF
		IF datatypesbase
			self.type := "DTYP"
			-> We can use datatypes to import the data,
			-> but we'll play it ourself, to allow more
			-> flexibility.
			obj := NewDTObjectA(fname,
				[DTA_SOURCETYPE,DTST_FILE,
				DTA_GROUPID, 	GID_SOUND,	-> Only load a sound file
				SDTA_VOLUME, 	64,
				SDTA_CYCLES,	1,			-> Play only once
				NIL, 		NIL])
				
			-> Error checking
			IF obj=NIL THEN Throw("DTYP", "OBJ")
			
			-> Read the attributes
			IF GetDTAttrsA(obj,
				[SDTA_SAMPLE,		{d},
				SDTA_SAMPLELENGTH,	{l},
				SDTA_PERIOD,		{p},
				SDTA_VOLUME,		{v},
				SDTA_CYCLES,		{c},
				TAG_END])<5 THEN Throw("DTYP", "READ")
			
			-> Crash prevention
			IF l=NIL THEN Throw("FILE", "LEN")
			IF d=NIL THEN Throw("FILE", "LOAD")
			
			-> Copy the sample data
			self.data := NewM(l, MEMF_CHIP)
			CopyMem(d, self.data, l)
			
			-> Copy the other attributes
			self.length := l
			self.period := p
			self.volume := v
			self.cycles := c
			
			-> Now that we have all the stuff, we can just trash 
			-> the whole datatypes thing.
			DisposeDTObject(obj)
			obj := NIL
			CloseLibrary(datatypesbase)
			datatypesbase := NIL
		ELSE
			-> Without the datatypes library, we are left to
			-> using RAW or IFF (8svx) sounds, and figuring out
			-> which ourselves.
			
			-> Open the file, first
			file := as_Open(fname, MODE_OLDFILE)
			
			-> Check if it's an IFF
			as_Read(file, {data}, 4)  	-> Read the first four bytes
			IF data = "FORM"
				-> Import it as an IFF
				
				-> The next LONG is the length of the sample.
				Read(file, {l}, 4)		-> Read it
				self.length := l	-> Save it
				
				-> Then we get the type
				Read(file, {data}, 4)	-> Read it
				
				IF data="8SVX"
					self.type := "8SVX"
					-> Now we need to go to the "VHDR" chunk
					REPEAT
						IF Read(file, {data}, 4) = -1 THEN Throw("FILE", "READ")
					UNTIL data="VHDR"
					
					-> Skip until the record rate
					Seek(file, 4, OFFSET_CURRENT)
					Read(file, {data}, 4)	-> Normal samples
					Read(file, {l}, 4)		-> Hi samples
					self.length := (l+data)
					Seek(file, 4, OFFSET_CURRENT) -> Some crap
					
						-> Get the play rate, set the period
					Read(file, data, 2)	-> Reading a WORD (so +2)
					self.period := rate2period(data[])
					
						-> Go the data section
					Seek(file, 0, OFFSET_BEGINNING)
					REPEAT
						-> Read with error checking
						IF Read(file, {data}, 4) = -1 THEN Throw("FILE", "READ")
					UNTIL data="BODY"
					-> Skip next four bytes for fun (SIZE OF HUNK, I guess...)
					Read(file, {data}, 4)
					
					-> Allocate the data field and read the hunk
					self.data := NewM(self.length, (MEMF_CHIP OR MEMF_CLEAR))
					
						-> Save the size of the block that was ACTUALLY read,
						-> in case the number we got before was crap.
						-> Read it, too.
					self.length := Read(file, self.data, self.length)
					IF self.length=-1 THEN Throw("FILE", "READ")
					
					-> Set some defaults
					self.volume := 64
					self.cycles := 1
				ELSE
					Throw("FILE", "FORM")
				ENDIF
			ELSE
				self.type := "RAW"
				-> It's a RAW sample.
				
					-> Go to the end
				Seek(file, 0, OFFSET_END)	
				
					-> Return the beginning and save the filesize
				self.length := Seek(file, 0, OFFSET_BEGINNING)
				IF self.length=-1 THEN Throw("LOAD", "DOS")
				
					-> Allocate storage and read the data
				self.data	:= NewM(self.length, MEMF_CHIP)
				self.length := Read(file, self.data, self.length)
					
					-> Insert other data as defaults
				self.cycles := 1
				self.period := rate2period(10000)
				self.volume := 64
			ENDIF
			Close(file)
		ENDIF
	ELSE
		-> Check flags for if they want us to pop up a requester if
		-> no name is given.
		
		-> I'll insert this later, because it's useless.
	ENDIF
EXCEPT
	-> De-allocate everything.
	IF obj				THEN DisposeDTObject(obj)
	IF datatypesbase	THEN CloseLibrary(datatypesbase)
	IF self.data 		THEN END self.data
	IF file				THEN Close(file)
	self.length := NIL
	
	RETURN exception, exceptioninfo
ENDPROC NIL, NIL

->
-> NAME
->	play -- Play sound
->
-> SYNOPSIS
->	sound.play(channel, wait=0)
->
-> FUNCTION
->	This will play the sound that has previously been loaded or
-> put into the object.  It does not currently check to make sure
-> the data is OK, but if nothing's there, make sure length=0 and
-> nothing will go wrong.
->
-> INPUTS
->	channel -- channel to play in (0-3)
->	wait    -- If 1 (default), then wait for the sound to complete
->             before returning.  If zero, then begin io and return
->             immediately.
->
-> RESULTS
->	error, id -- Error code and an additional id.  This will be one of:
->               "MEM" , "OUT"  -- Out of memory
->               "INIT", "PORT" -- Failed to create message port
->               "INIT", "REQ"  -- Failed to create IORequest structure
->               "ADIO", "OPEN" -- Failed to open audio device
->               "ADIO", "ALOC" -- Failed to allocate channels
->               These values are copied striaght from the function's
->               exception and exceptioninfo return.
->

EXPORT PROC play(channel, wait=0) OF sound HANDLE
	DEF chan=NIL:PTR TO channel
	DEF port=NIL:PTR TO mp
	DEF req=NIL:PTR TO ioaudio
	DEF error
	
	-> Pick our channel
	IF channels=NIL 
		NEW channels
		IF channels=NIL THEN Throw("MEM", "OUT")
	ENDIF
	chan := channels.num(channel)
	
	-> Stop any sound that's playing.  If they want us to wait,
	-> then wait first.
	chan.stop(wait)	
	
	-> Create a replyport
	port := CreateMsgPort()
	IF port=NIL THEN Throw("INIT", "PORT")
	
	-> Create the request
	req := CreateIORequest(port, SIZEOF ioaudio)
	IF req=NIL THEN Throw("INIT", "REQ")
	
	-> Open the device
	req.io.command	:= ADCMD_ALLOCATE
	req.io.mn.ln.pri:= -60
	req.data		:= [Shl(1, channel)]:CHAR
	req.length 		:= 1
	IF (error := OpenDevice('audio.device', 0, req, 0))
		SELECT error
			CASE ADIOERR_ALLOCFAILED
				Throw("ADIO", "ALOC")
			DEFAULT
				Throw("ADIO", "OPEN")
		ENDSELECT
	ENDIF
	
	-> Initialize the request for playing
	req.io.command	:= CMD_WRITE
	req.io.flags	:= ADIOF_PERVOL
	req.volume 		:= self.volume
	req.period		:= self.period
	req.cycles		:= self.cycles
	req.data		:= self.data
	req.length		:= (IF self.length <= 131072 THEN self.length ELSE 131072)
	
	-> Play the sound
	beginio(req)
	
	-> Now we'll store the request in the channel.
	chan.current 	:= req
	chan.sound		:= self	
	
	-> Wait, if requested
	IF wait THEN self.wait(channel)
EXCEPT	
	IF port THEN DeleteMsgPort(port)
	IF req  THEN DeleteIORequest(req)
	RETURN exception, exceptioninfo
ENDPROC NIL, NIL

->
-> NAME
->	stop -- Stop the sound, if playing, in the selected channel
->
-> SYNOPSIS
->	sound.stop(channel)
->
-> INPUTS
->	channel -- channel to stop a sound from playing in
->

EXPORT PROC stop(chan) OF sound 
	DEF channel:PTR TO channel
	IF channels		-> Channels will only init'd if a sound has been
					-> played
		channel := channels.num(chan)
		IF channel.sound=self THEN channel.stop(0)
	ENDIF
ENDPROC

->
-> NAME
->	wait -- wait for sound to finish
->
-> SYNOPSIS
->	sound.wait(channel)
->
-> INPUTS
->	sound -- initialized sound structure
->	channel -- Channel no. to wait for.
->

EXPORT PROC wait(chan) OF sound
	DEF channel:PTR TO channel
	IF channels
		channel := channels.num(chan)
		IF channel.sound=self THEN channel.stop(1)
	ENDIF
ENDPROC

->
-> NAME
->	sigbit -- Return sigbit of sound for chanel
->
-> SYNOPSIS
->	sigbit := sound.sigbit(channel)
->
-> INPUTS
->	channel -- channel number to use
->	sound   -- initalized sound object
->
-> RESULT
->	sigbit -- sigbit usable by Wait()
->
-> SEE ALSO
->	exec.library/Wait()

EXPORT PROC sigbit(channel) OF sound
	DEF chan:PTR TO channel, bit=0
	IF channels
		chan := channels.num(channel)
		IF chan.sound=self
			bit := Shl(1, chan.current.io.mn.replyport.sigbit)
		ENDIF
	ENDIF
ENDPROC bit

->
-> NAME
->	sigbits -- 	Return sigbits for all channels in a form apropriate
->				for use with Wait(sigs)
->
-> SYNOPSIS
->	sigbits := sound.sigbits()
->
-> INPUTS
->	sound -- initalized sound structure
->
-> RESULT
->	sigbits -- sigbits usable in Wait()
->

EXPORT PROC sigbits() OF sound
	DEF n, bits=0
	IF channels
		FOR n := 0 TO 3
			bits := Or(bits, self.sigbit(n))
		ENDFOR
	ENDIF
ENDPROC bits

->
-> NAME
->	rate -- Get/set play rate for sound
->
-> SYNOPSIS
->	rate := sound.rate(rate=0)
->	rate := sound.rate()
->
-> INPUTS
->	sound -- Initialized sound structure
->	rate -- If non-zero, will set rate for sound play-back
->       -- If it's zero(not given), only the rate will be returned
->
-> RETURN
->	rate -- The current replay rate
->

EXPORT PROC rate(rate=0) OF sound
	IF rate
		self.period := rate2period(rate)
		RETURN rate
	ENDIF
ENDPROC period2rate(self.period)

->
-> NAME
->	pervol -- modify period & volume of playing channels
->
-> SYNOPSIS
->	sound.pervol()
->
-> FUNCTION
->	This will just send an ADCMD_PERVOL request to any
-> channels that the sound is playing in.
->

EXPORT PROC pervol() OF sound HANDLE
	DEF chan:PTR TO channel, n
	DEF bits=0
	DEF req:PTR TO ioaudio, port:PTR TO mp
	
	-> Find our channels
	FOR n := 0 TO 3
		chan := channels.num(n)
		IF chan.sound=self THEN bits := Or(bits, Shl(1, n))
	ENDFOR
	
	-> Create message port
	port := CreateMsgPort()
	IF port=NIL THEN Throw("INIT", "PORT")
	
	-> Create IO Request
	req := CreateIORequest(port, SIZEOF ioaudio)
	
	-> Open Device (sure do this alot, eh?)
	IF OpenDevice('audio.device', NIL, req, NIL) THEN Throw("DEV", "ADIO")
	
	-> Fill the object with lies
	req.io.command	:= ADCMD_PERVOL
	req.data 		:= bits
	req.period		:= self.period
	req.volume		:= self.volume
	
	-> Do it.
	DoIO(req)
	
EXCEPT DO
	IF port THEN DeleteMsgPort(port)
	IF req  THEN DeleteIORequest(req)
	
	RETURN exception, exceptioninfo
ENDPROC

->
-> NAME
->	end
->
-> FUNCTION
->	Aborts all sounds that are playing and de-allcoates
-> waveform data
->

EXPORT PROC end() OF sound
	DEF n=3
	
	IF channels			-> Channels will only be init'd if
						-> a sound has played/is playing
			-> Loop through all channels, stopping the sound
			-> if it's this sound they're playing.
		FOR n := 0 TO 3
			self.stop(n)
		ENDFOR
	ENDIF
		-> De-allocate sample data
	IF self.data THEN Dispose(self.data)
ENDPROC

-> Channel/channels Methods <-

->
-> Given channel number, returns channel object
->

PROC num(n) OF chans
	SELECT n
		CASE 0
			IF self.left_a=NIL THEN NEW self.left_a
			RETURN self.left_a
		CASE 1
			IF self.right_a=NIL THEN NEW self.right_a
			RETURN self.right_a
		CASE 2
			IF self.right_b=NIL THEN NEW self.right_b
			RETURN self.right_b
		CASE 3
			IF self.left_b=NIL THEN NEW self.left_b
			RETURN self.left_b
	ENDSELECT
ENDPROC

->
-> Stops all sounds
->

PROC silence() OF chans
	IF self.left_a THEN self.left_a.stop(0)
	IF self.left_b THEN self.left_b.stop(0)
	IF self.right_a THEN self.right_a.stop(0)
	IF self.right_b THEN self.right_b.stop(0)
ENDPROC

EXPORT PROC shutup() IS IF channels THEN channels.silence() ELSE -1

->
-> Remove all completed sounds
->

PROC getdone() OF chans
	DEF channel:PTR TO channel, n
	
	FOR n := 0 TO 3
		IF (channel := self.num(n))<>NIL
			IF channel.done() THEN channel.stop(0)
		ENDIF
	ENDFOR
ENDPROC

EXPORT PROC check_sounds() IS IF channels THEN channels.getdone() ELSE -1

->
-> Stop channel.  If wait=1 then wait for it to finish, otherwise
-> Abort playing.
->

PROC stop(wait) OF channel
	IF self.current
		-> If they want us to, abort previous sound
		IF wait=0 THEN AbortIO(self.current)
	
		-> Wait for any IO to finish
		WaitIO(self.current)
		
		-> Close the device for this request
		IF self.current.io.device THEN CloseDevice(self.current)
		
		-> Remove the message port
		IF self.current.io.mn.replyport THEN DeleteMsgPort(self.current.io.mn.replyport)
		
		-> Delete the whole IO thing
		DeleteIORequest(self.current)
		
		-> Inform the world that it's gone.
		self.current := NIL
		self.sound	 := NIL
	ENDIF
ENDPROC		

->
-> Check if a channel is still playing.
->

PROC done() OF channel IS IF self.current THEN CheckIO(self.current) ELSE NIL
	

-> Auxiliary PROC's <-

->
-> NAME
->	rate2period -- convert samples/sec to period
->
-> SYNOPSIS
->	period := rate2period(rate)
->
-> INPUTS
->	rate -- Play rate in samples/sec
->
-> RETURN
->	period -- Period value as required for use fo audio.device
->

EXPORT PROC rate2period(rate:LONG)
	-> If not previously set, set our global clock rate
	IF clock=NIL THEN setclock()
	
	-> If the rate is negative, then E just doesn't realize it's
	-> an UNSIGNED int.  That's okay, I guess.  We'll just make it
	-> positive

	IF rate>32769 THEN WriteF('Hmmm...\n')
	IF rate<0 THEN rate := (rate*-1)
	
	-> If zero rate, use default of 10000
	IF rate=0 THEN rate := 10000
	
	-> If rate is in MHz, convert it to Hz.
	IF rate<100 THEN rate := rate * 1000
ENDPROC (clock/rate)

EXPORT PROC period2rate(period)
	IF clock=NIL THEN setclock()
	IF period=NIL THEN RETURN 10000	-> If given value is 0, return default
ENDPROC (clock/period)

PROC setclock()
	DEF gb:PTR TO gfxbase
	IF (gb.displayflags AND PAL)
		clock := 3579545	/* PAL clock speed */
	ELSE 
		clock := 3546895	/* NTSC clock speed */
	ENDIF
ENDPROC

-> beginio <-

PROC beginio(req)
	MOVE.L req, A1
	MOVE.L A6, -(A7)
	MOVE.L $14(A1), A6
	JSR    -$1E(A6)
	MOVE.L (A7)+, A6
ENDPROC
