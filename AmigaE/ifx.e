->
-> IFX
->
-> © by Dobes Vandermeer, 1997
->
-> Events daemon
->

-> Standard modules
MODULE 'dos/dos',          'dos/dosextens',  'dos/dostags'
MODULE 'exec/ports',       'exec/libraries', 
      'exec/tasks',        'exec/nodes',     'exec/io',
      'exec/interrupts',      'exec/semaphores',   'exec/memory'
MODULE 'intuition/intuition'
MODULE 'utility'
MODULE 'libraries/commodities'
MODULE 'timer'
MODULE 'layers',           'graphics/clip'
MODULE 'devices/input',       'devices/inputevent', 'devices/timer'
MODULE 'workbench/startup',      'workbench/workbench'
MODULE 'icon'

-> Special modules
MODULE '*modules/action',     '*modules/ifx'
MODULE '*patch'
MODULE '*input/inputhandler'
MODULE 'tools/cli'
MODULE 'other/ecodex',        'other/split'
MODULE 'fabio/rxobj_oo'
MODULE 'fabio/cxobj_oo'
MODULE 'tools/sound'
MODULE 'amigalib/tasks'
MODULE 'dos/dostags'
MODULE '*speak/speak'
MODULE 'tools/detatch'


->
-> CONSTANTS
->

CONST IFX_VERSION=2, IFX_REVISION=5

CONST NAMESIZE=256

->
-> Global objects
->

   -> Ports and lists
DEF rx:PTR TO rxobj
DEF cx:PTR TO cxobj
DEF mp:PTR TO mp  
DEF ids:PTR TO ifx_id
DEF tasks:PTR TO ifx_task

   -> Input Handler
DEF inputhandler:PTR TO inputhandler
   
   -> Global flags
DEF enabled, on_hour, do_chime, program_done

   -> Sounds
DEF sounds:PTR TO LONG, curr_snd

   -> Signals
DEF sigs:LONG, soundsigs:LONG

   -> Current directory
DEF currentdir:LONG, olddir:LONG

   -> Patches
DEF p_ow:PTR TO patch, p_owt:PTR TO patch, p_cw:PTR TO patch,
   p_os:PTR TO patch, p_ost:PTR TO patch, p_cs:PTR TO patch,
   p_db:PTR TO patch, p_msz:PTR TO patch

   -> Semaphore
DEF ids_sem:PTR TO ss

->
-> MAIN
->

PROC main() HANDLE
   -> Error messages
   DEF err:PTR TO CHAR, more:PTR TO CHAR
   
   -> Signal processing
   DEF gotsigs
   
   -> Message port
   DEF msg:PTR TO ifx_msg
   
   -> Misc.
   DEF temp

   -> Clock chimes
   DEF timerio:PTR TO timerequest, timermp:PTR TO mp

   -> Detatch!
   detatch('IFXII')

   ->
   -> Initialize global variables
   ->
   -> This should be done FIRST to avoid errors!
   ->
   
   ids := NIL
   sigs := soundsigs := NIL
   enabled := 1      -> We are active by DEFault
   NEW sounds[2]
   currentdir := olddir := NIL
   curr_snd := 1
   
   ->
   -> Only add the port if it isn't already added
   ->
   
   ->>>
   ->>>>>>> FORBID
   Forbid()
   IF mp := FindPort('IFX')
      ->
      -> If it is, tell us to QUIT.
      ->
      
      ->
      -> We'll send a EXEC message
      ->
      
      NEW msg
      msg.cmd := IFX_QUIT
      msg.length := SIZEOF ifx_msg
      PutMsg(mp, msg)
         
      ->
      -> Wait long enough for (the other) IFX to quit.
      ->
      
      Delay(50)
      
      ->
      -> De-allocate the msg
      ->
      
      END msg
      msg := NIL
      mp := NIL
   ENDIF

   ->
   -> Prepare exec mp (NEEDED by patches)
   ->

   mp := CreateMsgPort()
   IF mp=NIL
      Permit()
      Throw("INIT", "MP")
   ENDIF
   mp.ln.pri := 50
   mp.ln.name:= 'IFX'
   sigs := sigs OR Shl(1, mp.sigbit)
   AddPort(mp)
   
   ->>>
   ->>>>>>> PERMIT
   Permit()
      ->->WriteF('mp: \h (\h)\n', sigs, Shl(1, mp.sigbit))
   
   ->>
   ->>>> LIBRARIES & DEVICES
   ->>

   ->
   -> Open Layers Library
   ->
   
   layersbase := OpenLibrary('layers.library', 36)
   IF layersbase=NIL THEN Throw("INIT", "LAYR")
   
   ->
   -> Open Utility Library
   ->
   
   utilitybase := OpenLibrary('utility.library', 36)
   IF utilitybase=NIL THEN Throw("INIT", "UTIL")
   
   ->
   -> Open input.device
   ->
   
   NEW inputhandler.init({inputhandlerfunc}, NIL, 25, 'IFX Input Handler')
      
   ->
   -> Open timer.device
   ->
   
   timermp := CreateMsgPort()
   IF timermp=NIL THEN Throw("INIT", "TiMP")
      
   timerio := CreateIORequest(timermp, SIZEOF timerequest)
   IF timerio=NIL THEN Throw("INIT", "TiIO")
      
   IF OpenDevice('timer.device', UNIT_WAITUNTIL, timerio, NIL) THEN Throw("INIT", "TiOp")
   timerbase := timerio.io.device
   timerio.io.mn.ln.type := NIL        -> This should prevent crashes
   sigs := sigs OR Shl(1, timermp.sigbit)
      ->->WriteF('timer: \h (\h)\n', sigs, Shl(1, timermp.sigbit))

   ->
   -> Get command line settings
   ->
   
   do_wbargs()
   
   -> 
   -> Load configuration data
   ->
   
   load_config('S:IFX.ids')

   ->
   -> Prepare patches
   ->
   
   NEW p_ow.init( intuitionbase, -204, {new_ow},  TRUE)
   NEW p_owt.init(intuitionbase, -606, {new_owt}, TRUE)
   NEW p_cw.init( intuitionbase, -72,  {new_cw},  TRUE)
   NEW p_os.init( intuitionbase, -198, {new_os},  TRUE)
   NEW p_ost.init(intuitionbase, -612, {new_ost}, TRUE)
   NEW p_cs.init( intuitionbase, -66,  {new_cs},  TRUE)
   NEW p_db.init( intuitionbase, -96,  {new_db},  TRUE)
   NEW p_msz.init(layersbase,    -180, {new_msz}, TRUE)

   ->>
   ->>>> FORBID
   Forbid()

   ->
   -> Install patches
   ->

   p_ow.install()
   p_owt.install()
   p_cw.install()
   p_os.install()
   p_ost.install()
   p_cs.install()
   p_db.install()
   p_msz.install()

   ->
   -> Set up first timer event
   ->
   -> The timer.device was opened earlier.
   ->
   
   settimer(timerio)

   Permit()
   ->>>> PERMIT
   ->>
   
   ->
   -> Install input handler
   ->
   -> The input.device was opened earlier
   ->
   
   inputhandler.install()
   
   ->
   -> Prepare commodities object
   ->
   
   NEW cx.cxobj()
   cx.create('IFX', 'IFX II  © by Dobes Vandermeer', 'Input effects (FX) Daemon')
   sigs := sigs OR cx.signal()
      ->->WriteF('cx: \h (\h)\n', sigs, cx.signal())
   
   ->
   -> Prepare AREXX object
   ->
   
   NEW rx.rxobj('PLAY')
   sigs := sigs OR rx.signal()
   
   ->
   -> Prepare Semaphore
   ->
   NEW ids_sem
   InitSemaphore(ids_sem)
   
   ->
   -> Break handling
   ->
   sigs := sigs OR SIGBREAKF_CTRL_C
      
   -> Main Loop
   program_done := FALSE
   gotsigs := CheckSignal(sigs)
   WHILE program_done=FALSE
      ->
      -> Check for a timer event
      ->
      
      IF (gotsigs AND Shl(1, timermp.sigbit))<>NIL
         WaitIO(timerio)               -> Get the event
         IF on_hour
            do_effect('hourly_chime')
         ELSE
            do_effect('quarterly_chime')
         ENDIF
         settimer(timerio)
      ENDIF
      
      ->
      -> Check for a message (probably from a patch)
      ->
      
      IF (gotsigs AND Shl(1, mp.sigbit))<>NIL
         WHILE (msg := GetMsg(mp))<>NIL
            temp := msg.cmd
            SELECT temp
               CASE -1     -> One of our patches
                  IF checktask(msg.data)=FALSE
                     do_effect(msg.id)
                  ENDIF
                  FreeMem(msg, msg.length)   -> Free it for them
                  
               CASE IFX_ID
                  IF msg.id THEN do_effect(msg.id)
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_EDIT_IDS
                  msg.data := ids
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_EDIT_TASKS
                  msg.data := tasks
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_EDIT_SEM
                  msg.data := ids_sem
                  IF msg.replyport THEN ReplyMsg(msg)

               CASE IFX_ACTION
                  IF msg.data      THEN do_action(msg.data)
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_EFFECT
                  IF msg.data      THEN do_effect(msg.data)
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_CHIMES
                  IF msg.data
                     do_chime := 1
                  ELSE
                     do_chime := 0
                  ENDIF
                  IF msg.replyport THEN ReplyMsg(msg)
               
               CASE IFX_PARSE
                  IF msg.data
                     ifx_parse(msg.data)
                  ENDIF
                  
               CASE IFX_QUIT
                  IF msg.replyport THEN ReplyMsg(msg)
                  do_effect('ifx_kill')
                  program_done := TRUE
                                 
               DEFAULT
                  IF msg.replyport THEN ReplyMsg(msg)
            ENDSELECT
         ENDWHILE
      ENDIF
      
      ->
      -> Check for completed sounds
      ->
      
      IF (gotsigs AND soundsigs)<>NIL THEN check_sounds()
      
      ->
      -> Did we get an order from Exchange?
      ->
      
      IF (gotsigs AND cx.signal())<>NIL
         temp := cx.get()
         SELECT temp
            CASE CXCMD_DISABLE
               IF enabled=TRUE
                  do_effect('ifx_disable')
                  IF p_ow  THEN p_ow.remove()
                  IF p_owt THEN p_owt.remove()
                  IF p_cw  THEN p_cw.remove()
                  IF p_os  THEN p_os.remove()
                  IF p_ost THEN p_ost.remove()
                  IF p_cs  THEN p_cs.remove()
                  IF p_db  THEN p_db.remove()
                  IF p_msz THEN p_msz.remove()
                  inputhandler.remove()
                  enabled := FALSE
               ENDIF
               
            CASE CXCMD_ENABLE
               IF enabled=FALSE
                  IF p_ow  THEN p_ow.install()
                  IF p_owt THEN p_owt.install()
                  IF p_cw  THEN p_cw.install()
                  IF p_os  THEN p_os.install()
                  IF p_ost THEN p_ost.install()
                  IF p_cs  THEN p_cs.install()
                  IF p_db  THEN p_db.install()
                  IF p_msz THEN p_msz.install()
                  inputhandler.install()
                  enabled := TRUE
                  do_effect('ifx_enable')
               ENDIF
               
            CASE CXCMD_KILL
               do_effect('ifx_kill')
               program_done := TRUE
               
            CASE CXCMD_APPEAR
               do_effect('ifx_appear')
               
            CASE CXCMD_DISAPPEAR
               do_effect('ifx_disappear')
               
         ENDSELECT
      ENDIF

      ->
      -> Break handling
      ->
      
      IF (gotsigs AND SIGBREAKF_CTRL_C)<>NIL
         do_effect('ifx_kill')
         program_done := TRUE
      ENDIF
         
      ->
      -> Check if we received a REXX message
      ->
      
      IF (gotsigs AND rx.signal())<>NIL
         rx.get({rexxhandle})
      ENDIF
      
      ->
      -> Wait for a signal!
      ->

      IF program_done=FALSE THEN gotsigs := Wait(sigs OR soundsigs)
   ENDWHILE
   
   -> Done!
EXCEPT DO
   ->
   -> Disable ourselves
   ->
   
   enabled := FALSE
   
   ->
   -> Unlock the current directory lock
   ->
   
   IF currentdir
      CurrentDir(olddir)
      UnLock(currentdir)
      currentdir := NIL
   ENDIF
   
   ->
   -> Delete the semaphore
   ->
   IF ids_sem THEN END ids_sem

   ->
   -> Delete AREXX and CX objects
   ->
   
   IF cx THEN END cx
   IF rx THEN END rx

   ->
   -> Close input.device
   ->
   
   IF inputhandler THEN END inputhandler
   
   ->
   -> Remove Patches
   ->
   
   IF p_ow;    p_ow.remove();    END p_ow;   ENDIF
   IF p_owt;   p_owt.remove(); END p_owt;    ENDIF
   IF p_cw;    p_cw.remove();    END p_cw;   ENDIF
   IF p_os;    p_os.remove();    END p_os;   ENDIF
   IF p_ost;   p_ost.remove(); END p_ost;    ENDIF
   IF p_cs;    p_cs.remove();    END p_cs;   ENDIF
   IF p_db;    p_db.remove();    END p_db;   ENDIF
   IF p_msz;   p_msz.remove(); END p_msz;    ENDIF
      
   ->
   -> Destroy sounds that may still be playing
   ->

   do_sound(NIL, 1) -> Destroy/stop first sound
   do_sound(NIL, 1) -> Destroy/stop second sound
   shutup()    -> This stops all other sounds and 
               -> de-allocates their channels (pre-loaded)

   ->
   -> Close timer.device
   ->
   
   IF timerio
      IF Not(CheckIO(timerio))
         AbortIO(timerio)
      ENDIF
      WaitIO(timerio)
      CloseDevice(timerio)
      DeleteIORequest(timerio)
   ENDIF
   IF timermp  -> Delete the message port
      DeleteMsgPort(timermp)
   ENDIF
   timerbase := NIL        -> Prevent E from closing the "library"
   
   
   ->
   -> Remove message port
   ->
   
   IF mp
      ->>>>>>>
      Forbid() -> We forbid so that we won't get new messages
      
      RemPort(mp)
      
      WHILE (msg := GetMsg(mp))
         IF msg.cmd=-1
            FreeMem(msg, SIZEOF ifx_msg)
         ELSE
            IF msg.replyport THEN ReplyMsg(msg)
         ENDIF
      ENDWHILE
      DeleteMsgPort(mp)
      mp := NIL
      
      Permit()
      ->>>>>>>
   ENDIF
   
   ->
   -> Close the utility.library
   ->
   
   IF utilitybase THEN CloseLibrary(utilitybase)
   utilitybase := NIL

   ->
   -> Close the layers.library
   ->
      
   IF layersbase THEN CloseLibrary(layersbase)
   layersbase := NIL
   
   ->
   -> Go back to original directory
   ->
   cdhome()
   
   -> Error messages
   err := NIL
   more := NIL
   
   -> Error messages
   SELECT exception
      CASE NIL
       err := 'Exiting...'
      CASE "INIT" 
         err := 'Initialisation error: '
         SELECT exceptioninfo
            CASE "US"
               more := 'Unable to find our task!'
            CASE "RX"
               more := 'Unable to create AREXX port!'
            CASE "CX"
               more := 'Unable to create commodity!'
            CASE "MP"
               more := 'Unable to create message port!'
            CASE "LIB"
               more := 'Unable to creat library!'
            CASE "LOAD"
               more := 'Unable to open config file!'
            CASE "UTIL"
               more := 'Unable to open utility.library v36+!'
            CASE "LAYR"
               more := 'Unable to open layers.library v36+!'
            CASE "InMP"
               more := 'Unable to create message port for input.device!'
            CASE "InIO"
               more := 'Unable to create IO Request for input.device!'
            CASE "InOp"
               more := 'Unable to open input.device!'
            CASE "TiMP"
               more := 'Unable to create message port for timer.device!'
            CASE "TiIO"
               more := 'Unable to create io request for timer.device!'
            CASE "TiOp"
               more := 'Unable to open timer.device!'
            DEFAULT
               more := 'No specific message available'
         ENDSELECT
      CASE "MEM"
         err := 'Out of memory!'
         IF exceptioninfo THEN more := exceptioninfo
      CASE "NEW"
         err := 'Out of memory!'
      CASE "NIL"
         err := 'NIL pointer used!'
         NEW more[1024]
         StringF(more, 'On line \d', exceptioninfo)
      DEFAULT
         NEW err[1024]
         StringF(err, 'Error #: \d \s[4]\n', exception, {exception})    
   ENDSELECT
   
   -> Output the error! (or nothing...)
   IF err OR 1
      IF more=NIL THEN more := ''
      NEW temp[1024]
      StringF(temp, '\s\n\s\n', err, more)
->   WriteF(temp)
      put_error(temp)
   ENDIF
ENDPROC

   ->
   -> settimer(timerio)
   ->
   -> Setup next timer event and randomized seed
   ->

PROC settimer(timerio:PTR TO timerequest)
   -> Variables for storing current time
   DEF timeval:PTR TO timeval, dest
   
   -> Get current time
   NEW timeval
   GetSysTime(timeval)

   -> "RANDOMIZE TIMER" (from ABASIC)
   Rnd(timeval.secs)
   
   -> Determine next quarter  (900 secs/15 mins)
   dest := Mul(Div(timeval.secs,900),900)+900
   on_hour := Mul(Div(timeval.secs,3600),3600)+3600
   IF on_hour=dest
      on_hour := Mod(Div(on_hour, 3600), 12)
      IF on_hour=0 THEN on_hour  := 12
      IF do_chime  THEN do_chime := 1
   ELSE
      on_hour := NIL
   ENDIF
      
   -> Write timing request
   AbortIO(timerio)  -> Abort any pending request
   timerio.io.command   := TR_ADDREQUEST
   timerio.time.secs := dest
   timerio.time.micro   := 0
   beginio(timerio)
   
   -> Free memory
   END timeval
ENDPROC

   ->
   -> do_action(action, ext)
   ->

PROC do_action(action:PTR TO ifx_action, ext=NIL) HANDLE
   DEF type, data, temp
   DEF snd:PTR TO sound
   DEF rexx:PTR TO rxobj
   
   -> Error checking
   IF action=NIL THEN RETURN
   
   -> Get vars
   data := action.data
   IF ext=NIL THEN ext  := action.ext
   type := action.type
   
   IF type=ACT_EXTEND
      IF action.nextaction=NIL THEN RETURN
      ext  := action.nextaction.data
      type := action.nextaction.type
   ENDIF
   
   SELECT type
      CASE ACT_NOTHING
         /* DO NOTHING */
      CASE ACT_AREXX
         -> ext=command, data=host
         NEW rexx.rxobj('IFX_ACTION')
         rexx.send(data, ext, NIL, NIL)
         END rexx
         
      CASE ACT_EXECUTE
         -> data=string
         temp := Open('NIL:', MODE_NEWFILE)
         Execute(data, temp, temp)
         Close(temp)    -> Close the file, stupid!
      
      CASE ACT_SPEECH
         -> data=stuff to "say"
         say(data)
         
      CASE ACT_SOUND
		  -> Data=sound to play, ext=cycles
         do_sound(data, 0, action.ext)
         
      CASE ACT_SOUNDFILE
		  -> data=Name of sound to play, ext=cycles
         NEW snd
         IF snd.load(data)=NIL
            do_sound(snd, 1, action.ext)
         ELSE
            -> Error! Remove the useless, unused data
            END snd
         ENDIF
         
      CASE ACT_IFX
         do_effect(data)
      
      CASE ACT_REQUESTER
         NEW temp[256]
         StringF(temp, '\n\n\s\n\n', data)
         put_message('IFX Message', temp)
         END temp
   ENDSELECT
EXCEPT
   ReThrow()
ENDPROC

PROC do_sound(snd:PTR TO sound, set=NIL, cycles=1)
   DEF tsnd:PTR TO sound, err
   DEF volstr[8]:STRING, volume
   
   curr_snd := (IF curr_snd=1 THEN 0 ELSE 1)

   ->
   -> First, we destroy any sound that was playing before.
   ->

   IF sounds[curr_snd]
      -> Get it
      tsnd := sounds[curr_snd]

      -> Remove signal bit
      soundsigs := soundsigs AND Not(tsnd.sigbits())

      -> Stop sound
      tsnd.stop(curr_snd*2)
      tsnd.stop((curr_snd*2)+1)

      -> Kill sound data
      Dispose(tsnd.data)
      tsnd.data := NIL
      
      -> Delete the sound
      END tsnd
      sounds[curr_snd] := NIL
   ENDIF

   ->
   -> Then we install ourselves into the system,
   -> if wanted.  If the sound is supposed to be preloaded,
   -> then you SHOULD NOT do this.
   ->

   IF set THEN sounds[curr_snd] := snd ELSE sounds[curr_snd] := NIL

   IF enabled -> Only play sounds when enabled
      ->
      -> The interesting thing is, if we want to stop
      -> and de-allocate the currently playing sound,
      -> We can just pass a NIL sound, and this function
      -> will comply nicely!
      ->

      IF snd
         ->
         -> Finally, we play the sound
         ->
       
       -> Read volume from ENV:
       IF GetVar('IFXVOLUME', volstr, 8, NIL)=-1 THEN
          IF GetVar('VOLUME', volstr, 8, NIL)=-1 THEN StrCopy(volstr, '64')
       volume := Val(volstr)
       snd.volume := Bounds(volume, 0, 64)
                
         -> Chiming handling
         snd.cycles := cycles -> Usually 1
         IF do_chime<>NIL AND on_hour<>NIL THEN snd.cycles := do_chime

         err := snd.play(curr_snd*2)
         err := snd.play((curr_snd*2)+1) OR err -> Add it on...

         ->
         -> We also add the sound to our signal mask
         ->

         soundsigs := soundsigs OR snd.sigbits()
      ENDIF
   ENDIF
ENDPROC err

   ->
   -> do_effect(id)
   ->

PROC do_effect(id:PTR TO CHAR)
   DEF ifx:PTR TO ifx_id
   DEF num:REG, n:REG, action:PTR TO ifx_action
   DEF ret
   
   -> If the pointer is junk, return
   IF id=NIL THEN RETURN
   
   -> If we're disabled, then don't do it
   IF enabled=FALSE THEN RETURN
   
   -> Handle chiming
   IF do_chime<>NIL AND on_hour<>NIL AND StrCmp(id, 'hourly_chime')<>NIL
      do_chime := on_hour
   ENDIF
      
   -> Find the id
   ifx := findid(id, ids)
   
   -> Randomly select an action
   IF ifx
      -> Check for problems
      IF ifx.action=NIL THEN RETURN
      
      -> Get our random number
      num := Rnd(ifx.num)  
      
      -> Find that number in the list
      action := ifx.action
      FOR n := 1 TO num
         IF action.nextaction<>NIL
            action := action.nextaction
         ENDIF
      ENDFOR
      
      -> Do the action
      IF action
         IF do_action(action)
            ret := NIL
         ELSE
            ret := action           -> non-zero return
         ENDIF
      ENDIF
   ELSE
      ret := NIL
   ENDIF
   
   -> Reset do_chime
   IF do_chime>1 THEN do_chime := 1
ENDPROC ret

->
-> Special functions
->

   ->
   -> rexxhandle(str)
   ->

PROC rexxhandle(str)
   DEF isrexx, rc, resultstr
   
   -> Only 
   IF enabled
      IF ifx_parse(str)=-1
         isrexx:=FALSE
         rc := 5
         resultstr := 'Incomplete'
      ELSE
         isrexx := TRUE
         rc := 0
         resultstr := 'Successful'
      ENDIF
   ELSE
      isrexx:=TRUE
      rc := 5
      resultstr := 'Disabled'
   ENDIF
      
ENDPROC isrexx, rc, resultstr

   ->
   -> load_config()
   ->

PROC load_config(filepath) HANDLE
   DEF file, buf, done=0
   
   -> Make sure filepath is OK
   IF filepath=NIL OR StrLen(filepath)=0
      filepath := 's:IFX.ids'
   ENDIF
   
   -> Open the file
   file := Open(filepath, MODE_OLDFILE)
   IF file=NIL THEN Throw("INIT", "LOAD")
   
   -> Create a buffer
   NEW buf[1024]
   IF buf=NIL THEN Throw("MEM", ' (for read buffer)')
      
   -> Read & Parse!
   WHILE done=0
      -> Read a line
      IF Fgets(file, buf, 1024)
         -> Parse it
         ifx_parse(buf)
      ELSE
         -> EOF! Return...
         done := 1
      ENDIF
   ENDWHILE
   
   -> Done!
EXCEPT DO
   IF file THEN Close(file)
   IF buf  THEN END buf
      
   RETURN exception, exceptioninfo
ENDPROC

PROC ifx_parse(str:PTR TO CHAR) HANDLE
   DEF list:PTR TO LONG, temp:PTR TO CHAR, snd:PTR TO sound
   DEF ifx:PTR TO ifx_id, action:PTR TO ifx_action, type=NIL
   DEF ti, ta, act:PTR TO ifx_action, task:PTR TO ifx_task

   -> Make sure we HAVE a string
   IF str=NIL THEN RETURN

   -> Set string to lower case
   LowerStr(str)

   -> Divide our arg into a more useful form
   list := argSplit(str)
   IF list=NIL THEN Throw("MEM", ' (for arg list)')

   ->
   -> All types/commands MUST be longer then 1 char, otherwise
   -> they will be considered comments
   ->
   
   IF StrLen(list[0])>1

      -> Find type
      type := NIL
      IF StrCmp(list[0], 'id')
         IF enabled
            do_effect(list[1])
         ELSE
            RETURN 1
         ENDIF
      
      ELSEIF StrCmp(list[0], 'file')
         NEW snd
         IF snd
            IF snd.load(list[1])=NIL
               do_sound(snd, 1)
            ELSE
               -> Error! Remove the useless, unused data
               END snd
            ENDIF
         ENDIF
         
      ELSEIF StrCmp(list[0], 'quit')
         do_effect('ifx_quit')
         program_done := TRUE
         
      ELSEIF StrCmp(list[0], 'prefs')     -> Load new prefs
         -> This is a KLUDGE!
         -> Without this, IFX crashes when you move a window,
         -> after receiving the prefs command.
         p_msz.remove()
         
		 -> Don't allow sound playing
		 enabled := FALSE

         -> Stop sounds
         do_sound(NIL)		-> Kill current channels
         do_sound(NIL)		-> Kill other channels
         shutup()
         
         -> Destroy ids list
         ifx := ids
         WHILE ifx<>NIL
            -> Save next
            ti := ifx.next
            
            -> Destroy action list
            act := ifx.action
            WHILE act<>NIL
               ta := act.nextaction
               IF act.data
                  IF act.type <> ACT_SOUND
                     -> It's a string!
                     END act.data
					 act.data := NIL
                  ELSE
                     -> It's a sound object!
                     snd := act.data
                     Dispose(snd.data)
                     snd.data := NIL
                     END snd
					 act.data := NIL
                  ENDIF
               ENDIF 
               END act
               act := ta
            ENDWHILE
            
            -> Destroy id data
            IF ifx.id THEN END ifx.id
            END ifx
            
            -> Goto next
            ifx := ti
         ENDWHILE
         
         task := tasks
         WHILE task<>NIL
            -> Save next
            ti := task.nexttask
            
            -> Destroy task data
            END task.pat
            END task
            
            -> Goto next
            task := ti
         ENDWHILE
         
         -> Prevent use of old, illegal pointers
         ids := NIL
         tasks := NIL
         
         -> Load new config   
         load_config('s:IFX.ids')
         
		 -> Play sounds again
		 enabled := TRUE
		 
         -> KLUDGE (see above)
         p_msz.install()
      ELSEIF StrCmp(list[0], 'sound')
         type := ACT_SOUNDFILE
      ELSEIF StrCmp(list[0], 'psound')
         type := ACT_SOUND
      ELSEIF StrCmp(list[0], 'speech')
         type := ACT_SPEECH
      ELSEIF StrCmp(list[0], 'exec')
         type := ACT_EXECUTE
      ELSEIF StrCmp(list[0], 'other')
         type := ACT_IFX
      ELSEIF StrCmp(list[0], 'exclude')
         newtask(list[1])
      ELSEIF StrCmp(list[0], 'message')
         type := ACT_REQUESTER
      ELSEIF StrCmp(list[0], 'chimes')
         do_chime := 1
      ELSEIF StrCmp(list[0], 'nochimes')
         do_chime := 0
      ELSEIF StrCmp(list[0], 'include') OR StrCmp(list[0], 'config')
         load_config(list[1])
      ELSEIF StrCmp(list[0], 'defprefsdir')
         -> Unlock previous dir, if there was one.
         cdup()
         
         -> CD to given dir
         cd(list[1])
         
      ELSEIF StrCmp(list[0], 'nothing')
         type := -1
      ELSE  -> Unknown command
         RETURN -1
      ENDIF

      IF type<>NIL
         -> Check if the ID already exists
         ifx := findid(list[1], ids)
         IF ifx=NIL THEN ifx := newid(list[1])

         -> Special case for type: ACT_NOTHING
         IF type=-1 THEN type := ACT_NOTHING    /* NIL */

         -> Add the entry
         IF list[2]
            -> Allocate the structure
            NEW action

            SELECT type
            CASE ACT_SOUND
               NEW snd
               IF snd.load(list[2])
                  END snd
                  RETURN ACT_SOUND
               ENDIF
               temp := snd
            
            CASE ACT_SOUNDFILE
               -> Get the data string
               NEW temp[NAMESIZE]
               GetCurrentDirName(temp, NAMESIZE)
               AddPart(temp, list[2], NAMESIZE)
            
            DEFAULT
               NEW temp[NAMESIZE]
               CopyMem(list[2], temp, NAMESIZE)
            ENDSELECT
            
            -> Fill in all the stuff
            action.type       := type
            action.data       := temp
			action.ext        := IF list[3] THEN Val(list[3]) ELSE 1
            action.nextaction := ifx.action
            ifx.action        := action
            ifx.num           := ifx.num+1
         ENDIF
      ENDIF
   ENDIF
EXCEPT DO
   -> Free the list
   IF list THEN DisposeLink(list)
      
   IF exception THEN ReThrow()
ENDPROC NIL

->
-> ID functions
->

   ->
   -> findid(name, ids)
   ->
   
PROC findid(id:PTR TO CHAR, list:PTR TO ifx_id)
   DEF next:REG, curr:REG PTR TO ifx_id
   
   -> Set name to lower case, for case-insensitive comparing
   LowerStr(id)
   
   -> Get first one
   curr := list
   
   -> Loop until a match is found
   WHILE curr
      -> Prepare
      next := curr.next
      
      -> Check it out
      IF StrCmp(curr.id, id) THEN RETURN curr
      
      -> Go to next
      curr := next
   ENDWHILE
ENDPROC NIL

   ->
   -> newid(name)
   ->
   
PROC newid(id:PTR TO CHAR)
   DEF ifx:REG PTR TO ifx_id
   DEF temp:REG PTR TO CHAR
   -> Create the object
   NEW ifx
   
   -> Copy the ID string
   NEW temp[StrLen(id)+1]
   StringF(temp, '\s', id)
   ifx.id    := temp
   
   -> Add new id to the list
   ifx.next := ids
   IF ids THEN ids.prev := ifx
   ifx.prev := NIL
   
   -> Initialize the other data
   ifx.num   := 0
   ifx.action   := NIL
   
   -> Set list head
   ids := ifx
ENDPROC ifx

->
-> TASK functions
->

   ->
   -> newtask(name)
   ->
   
PROC newtask(name:PTR TO CHAR)
   DEF task:REG PTR TO ifx_task, tokenized:REG PTR TO CHAR
   
   IF name=NIL THEN RETURN
      
   -> Create our new object
   NEW task
   
   -> Create our tokenized string buffer
   NEW tokenized[(StrLen(name)*3)]
      
   -> Tokenize the string
   ParsePatternNoCase(name, tokenized, (StrLen(name)*3))
   
   -> Put it into the object
   task.pat := tokenized
   
   -> Add it to the list
   task.nexttask := tasks
   tasks := task
ENDPROC task

   ->
   -> checktask(task *)
   ->
   -> This searches for a task in our "bad" list, and returns whether
   -> it exists.
   ->
   
PROC checktask(tc:PTR TO tc)
   DEF yes=0, name:REG PTR TO CHAR, task:REG PTR TO ifx_task
   
   -> Get the name string
   name := tc.ln.name
   
   IF tc=NIL THEN RETURN FALSE
   IF tasks=NIL THEN RETURN FALSE
   
   -> Start down the list
   task := tasks
   
   WHILE (yes=NIL) AND (task<>NIL)
      IF task.pat THEN yes := MatchPatternNoCase(task.pat, name)
      task := task.nexttask
   ENDWHILE
   
   -> Done!
ENDPROC yes

->
-> do_wbargs()
->
-> Read our tooltypes, if started from the WB, and
-> set the appropriate variables.
->

PROC do_wbargs() HANDLE
   DEF wb:PTR TO wbstartup, wbarg:PTR TO wbarg, old_dir
   DEF do:PTR TO diskobject
   DEF val
   
   IF wbmessage
      wb := wbmessage
      -> Open the icon.library
      iconbase := OpenLibrary('icon.library', 34)
      IF iconbase=NIL THEN RETURN
      
      -> Get first arg (ourself)
      wbarg := wb.arglist
      IF wbarg
         old_dir := CurrentDir(wbarg.lock)
         do := GetDiskObject(wbarg.name)
         IF do
            -> Chimes setting
            val := FindToolType(do.tooltypes, 'CHIMES')
            IF val
               UpperStr(val)
               do_chime := StrCmp(val, 'YES')
            ENDIF
            
            -> Defprefsdir
            val := FindToolType(do.tooltypes, 'SOUNDSDIR')
            IF val
               -> Get out of previous directory
               CurrentDir(NIL)
               IF currentdir THEN UnLock(currentdir)

               -> Get into new directory
               currentdir := Lock(val, ACCESS_READ)
               IF olddir=NIL
                  olddir := CurrentDir(currentdir)
               ELSE
                  CurrentDir(currentdir)
               ENDIF
            ENDIF
            
            -> Config file (S:IFX.ids is still loaded)
            val := FindToolType(do.tooltypes, 'PREFS')
            IF val
               load_config(val)
            ENDIF
            
            -> Done
            FreeDiskObject(do)
         ENDIF
         CurrentDir(old_dir)
      ENDIF
   ENDIF

EXCEPT DO
   -> Close the icon.library
   IF iconbase
      CloseLibrary(iconbase)
      iconbase := NIL
   ENDIF
ENDPROC

->
-> put_error (str, win)
->
-> output errors in an extensible way
->

PROC put_error(str, win=NIL) IS EasyRequestArgs(win, 
                        [SIZEOF easystruct, NIL, 'IFX ', str, 'Darn'],
                        NIL, NIL)

->
-> put_message (str, win)
->
-> output messages in an extensible way
->

PROC put_message(title, str, win=NIL) IS EasyRequestArgs(win, 
                        [SIZEOF easystruct, NIL, title, str, 'Okay'],
                        NIL, NIL)

->
-> Patch Functions
->

PROC put_msg(id, t=NIL) HANDLE
   DEF msg:PTR TO ifx_msg
   IF enabled
      msg := AllocMem(SIZEOF ifx_msg, MEMF_PUBLIC OR MEMF_CLEAR)
      IF msg
         IF mp
            msg.cmd     := -1
            msg.id      := id
            msg.data := t
            msg.length  := SIZEOF ifx_msg
            msg.replyport:=NIL
            PutMsg(mp, msg)
         ELSE
            FreeMem(msg, SIZEOF ifx_msg)
         ENDIF
      ENDIF
   ENDIF
EXCEPT
   IF msg THEN FreeMem(msg, SIZEOF ifx_msg)
ENDPROC

PROC new_ow()
   DEF nwin:PTR TO nw, tags, lib
   
   -> Save registers
   MOVE.L A0, nwin
   MOVE.L A1, tags
   MOVE.L A6, lib
   
   -> Send the message
   IF (nwin.flags AND WFLG_BACKDROP)=NIL
      put_msg('open_window', FindTask(NIL))
   ENDIF
   
   -> Restore Registers
   MOVE.L nwin, A0
   MOVE.L tags, A1
   MOVE.L lib, A6
ENDPROC

PROC new_os()
   DEF nscreen, tags, lib
   
   -> Save registers
   MOVE.L A0, nscreen
   MOVE.L A1, tags
   MOVE.L A6, lib                         
   
   -> Send the message
   put_msg('open_screen' , FindTask(NIL))
   
   -> Restore Registers
   MOVE.L nscreen, A0
   MOVE.L tags, A1
   MOVE.L lib, A6
ENDPROC

PROC new_cs()
   DEF screen, lib
   
   -> Save registers
   MOVE.L A0, screen
   MOVE.L A6, lib
   
   -> Send the message
   put_msg('close_screen' , FindTask(NIL))
   
   -> Restore Registers
   MOVE.L screen, A0
   MOVE.L lib, A6 
ENDPROC

PROC new_owt()
   DEF nwin:PTR TO nw, tags, tag:PTR TO LONG, lib
   
   -> Save registers
   MOVE.L A0, nwin
   MOVE.L A1, tags
   MOVE.L A6, lib
   
   -> Send the message
   IF tags
      tag := FindTagItem( WA_BACKDROP, tags)
      IF tag THEN IF tag[1] <> NIL THEN JUMP owt_exit
   ENDIF    
   
   IF nwin THEN IF (nwin.flags AND WFLG_BACKDROP)<>NIL THEN JUMP owt_exit
            
   -> We made it through the tests!  Put the message.
   put_msg('open_window' , FindTask(NIL))

owt_exit:
   -> Restore Registers
   MOVE.L nwin, A0
   MOVE.L tags, A1
   MOVE.L lib, A6 
ENDPROC

PROC new_ost()
   DEF nscreen, tags, lib
   
   -> Save registers
   MOVE.L A0, nscreen
   MOVE.L A1, tags
   MOVE.L A6, lib
   
   -> Send the message
   put_msg('open_screen' , FindTask(NIL))
   
   -> Restore Registers
   MOVE.L nscreen, A0
   MOVE.L tags, A1
   MOVE.L lib, A6 
ENDPROC

PROC new_db()
   DEF screen, lib
   
   -> Save registers
   MOVE.L A0, screen
   MOVE.L A6, lib
   
   -> Send the message
   put_msg('beep')
   
   -> Restore Registers
   MOVE.L screen, A0
   MOVE.L lib, A6 
ENDPROC

PROC new_cw()
   DEF win:PTR TO window, lib
   
   -> Save registers
   MOVE.L A0, win
   MOVE.L A6, lib
   
   -> Send the message
   IF (win.flags AND WFLG_BACKDROP)=NIL
      put_msg('close_window', FindTask(NIL))
   ENDIF
   
   -> Restore Registers
   MOVE.L win, A0
   MOVE.L lib, A6 
ENDPROC

PROC new_msz()
   DEF l,x,y,w,h,lib
   
   -> Save registers
   MOVE.L A6, lib
   MOVE.L A0, l
   MOVE.L D0, x
   MOVE.L D1, y
   MOVE.L D2, w
   MOVE.L D3, h
   
   IF (w OR h) AND (x OR y)
      put_msg('window_movesize' , FindTask(NIL))
   ELSEIF w OR h
      put_msg('window_resize' , FindTask(NIL))
   ELSEIF x OR y
      put_msg('window_move' , FindTask(NIL))
   ENDIF
   
   -> Restore registers
   MOVE.L lib, A6
   MOVE.L l, A0
   MOVE.L x, D0
   MOVE.L y, D1
   MOVE.L w, D2
   MOVE.L h, D3
ENDPROC x, y, w


   ->
   -> The input handler
   ->

PROC inputhandlerfunc(list:PTR TO inputevent)
   -> The variables
   DEF class, curr:PTR TO inputevent
   
   -> Get the first entry
   curr := list

   WHILE curr<>NIL
      class := curr.class
   
      SELECT class
      -> Perform processing
      CASE IECLASS_DISKINSERTED
         put_msg('disk_inserted')
            
      CASE IECLASS_DISKREMOVED
         put_msg('disk_removed')
            
      ENDSELECT
      
      -> Goto next
      curr := curr.nextevent
   ENDWHILE

   -> Safety's sake...
-> MOVE.L list, D0
ENDPROC list

->
-> Version info
->

CHAR '$VER: IFX 2.37 (12/13/97)'
LONG 0
CHAR '$INF: A sound-events daemon for the Amiga'
