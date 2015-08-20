->
-> testinput.e
->
-> Test the input handler routines
->

MODULE '*inputhandler'
MODULE 'dos/dos'
MODULE 'devices/inputevent'
    
-> The event counter
DEF inputcount:LONG	
	
->
PROC main() HANDLE
	DEF ih:PTR TO inputhandler
	
	-> Intilialize input handler
	NEW ih.init({testhandler}, NIL, 10, 'Test')
	
	-> Install it
	ih.install()
	
	-> Wait for a keypress from the CLI
	WriteF('Waiting for CTRL-C...')
	Wait(SIGBREAKF_CTRL_C)
	
	-> Remove handler
	ih.remove()
	
	-> Destroy input handler
	END ih
	
	-> Tell user count
	WriteF('\d\n', inputcount)
EXCEPT
	WriteF('Exception!!! Number = $\h\n', exception)
ENDPROC

PROC testhandler(list:PTR TO inputevent)
	DEF class
	class := list.class
	SELECT class
	CASE IECLASS_DISKINSERTED
		DisplayBeep(NIL)
	CASE IECLASS_DISKREMOVED
		DisplayBeep(NIL)
	DEFAULT
		INC inputcount
	ENDSELECT
		
	MOVE.L list, D0
ENDPROC list