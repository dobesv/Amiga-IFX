OPT MODULE

MODULE 'exec', 'exec/ports', 'exec/io'
MODULE 'devices/narrator', 'translator'

EXPORT PROC say(str:PTR TO CHAR, channels=%1100, rate=DEFRATE, pitch=DEFPITCH, mode=DEFMODE, sex=MALE, volume=64) HANDLE
	DEF ioreq:PTR TO ndi, port:PTR TO mp
	DEF phonetic:PTR TO CHAR, len
	
	NEW phonetic[1024]
	
	-> First open translator.library
	translatorbase := OpenLibrary('translator.library', 0)
	IF translatorbase=NIL THEN Raise("LIB")
	
	Translate(str, StrLen(str), phonetic, 1024)
	
	-> Create a message port for replies
	port := CreateMsgPort()
	IF port=NIL THEN Raise("PORT")
	
	-> Create an IORequest
	ioreq := CreateIORequest(port, SIZEOF ndi)
	IF ioreq=NIL THEN Raise("REQ")
	
	ioreq.flags := NIL
	-> Open the Device
	IF OpenDevice('narrator.device', 0, ioreq, 0) THEN Raise("DEV")
	
	-> Prepare to write
	ioreq.iostd.command := CMD_WRITE
	ioreq.iostd.data 	:= phonetic
	ioreq.iostd.length 	:= StrLen(phonetic)
	ioreq.chmasks 	:= [3, 5, 10, 12]:CHAR
	ioreq.nummasks	:= 4
	ioreq.rate 		:= rate
	ioreq.pitch 	:= pitch
	ioreq.mode 		:= mode
	ioreq.sex 		:= sex
	ioreq.volume 	:= volume
	
	-> Do it.
	DoIO(ioreq)
	
EXCEPT DO
	IF port 			THEN DeleteMsgPort(port)
	IF ioreq
		CloseDevice(ioreq)
		DeleteIORequest(ioreq)
	ENDIF
	IF translatorbase	THEN CloseLibrary(translatorbase)
	IF exception THEN RETURN exception
ENDPROC NIL
