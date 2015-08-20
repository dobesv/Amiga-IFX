->
-> IFXNote.e
->
-> Add a FileNote to all sounds that have an ID associated.
->

MODULE 'dos/dos'
MODULE 'other/split'
MODULE 'tools/cli'


   ->
   -> load_config()
   ->

PROC load_config(filepath) HANDLE
   DEF file, buf, done=0
   
   -> Write Info
   WriteF('FILE \s\n', filepath)

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
         comment_parse(buf)
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

PROC comment_parse(str:PTR TO CHAR) HANDLE
   DEF list:PTR TO LONG
   DEF comment[81]:STRING

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
      -> Check if it is a sound
      IF StrCmp(list[0], 'sound') OR StrCmp(list[0], 'psound')
          StringF(comment, 'used as IFX id "\s"', list[1])
		  IF SetComment(list[2], comment)
			  WriteF('\t\s \s \s\n', list[0], list[2], comment)
		  ELSE
		      WriteF('\t*** Error with \s (\s)\n', list[2], list[1])
		  ENDIF
	  ELSEIF StrCmp(list[0], 'defprefsdir')
		  cdup()
		  cd(list[1])
		  WriteF('DIR \s\n', list[1])
	  ELSEIF StrCmp(list[0], 'config') OR StrCmp(list[0], 'include')
		  load_config(list[1])
      ENDIF
	  IF CtrlC() THEN Raise(NIL)
   ENDIF
EXCEPT DO
   -> Free the list
   IF list THEN DisposeLink(list)
      
   IF exception THEN ReThrow()
ENDPROC NIL

->
-> MAIN
->

PROC main() HANDLE
	DEF currdir:PTR TO CHAR
	
	cli_lastdir := NIL
	currdir := getcd()
	load_config('S:IFX.ids')
EXCEPT DO
->	cdhome()
	cd(currdir)
	IF exception THEN WriteF('Error during commenting process.\n')
ENDPROC