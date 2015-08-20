->
-> sound.m - simplify playing sounds in E
->

MODULE 'devices/audio'
MODULE 'exec/ports', 'exec/lists', 'exec/io', 'exec/execbase', 'exec'
MODULE 'datatypes/datatypes', 'datatypes/datatypesclass', 'datatypes'
MODULE 'dos', 'dos/dos'
MODULE 'utility/tagitem'
MODULE 'datatypes/soundclass'

DEF datatypesbase

PROC main()
	play_dt(arg)
ENDPROC

-> NAME
-> 	play_dt -- plays a sound file using datatypes.
->
-> SYNOPSIS
->	play_dt(file)
->	:LONG   :PTR TO CHAR
->
-> FUNCTION
->	This is a pretty simple-to-use function.  It is only an intermediate
-> to my planned sound object.
->
-> INPUTS
->	file (STRING)

PROC play_dt(file)
	DEF play:dttrigger, sound
	DEF num
	DEF length, cycles, vh:voiceheader, time
	DEF period, frequency
	DEF eb:PTR TO execbase
	DEF close
	
	play.methodid := DTM_TRIGGER
	play.function := STM_PLAY

		-> If they haven't opened the datatypes library for us, then
		-> we'll open it.
	IF (datatypesbase = NIL)
		datatypesbase := OpenLibrary('datatypes.library', 39)
	    close := 1
	ENDIF

	sound := NewDTObjectA(file,
		[
		DTA_GROUPID, 	GID_SOUND,	-> Only take sound files
		SDTA_VOLUME, 	64,			-> This might later be configurable
		SDTA_CYCLES,	1,
		TAG_END])

	IF sound
		-> We will return the time to Delay(), if they need it.
		GetDTAttrsA(sound,
			[SDTA_SAMPLELENGTH, {length},
			SDTA_PERIOD, 		{period},
			SDTA_CYCLES,		{cycles},
			TAG_END])
		
		eb := execbase
        frequency 	:= Mul(period, (357959465*2))
		WriteF('Frequency = \d\n', frequency)
		time		:= (((length * cycles) * 50) / frequency)
		WriteF('Time = \d\n', time)

		-> Play the sound.
		DoDTMethodA(sound, NIL, NIL, play)
	
		-> Wait for it to finish.
		Delay(time)
		
		-> Free the DT Object.
		DisposeDTObject(sound)

	ELSE -> Unable to create DT object!
		-> They're expecting a time, so zero is an error.  If they don't
		-> check for an error, then no sample will play, so nothing will
		-> will happen.
		time := NIL
	ENDIF
	IF close
		CloseLibrary(datatypesbase)
		datatypesbase := NIL
	ENDIF
ENDPROC time

