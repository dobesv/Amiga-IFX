->
-> inputhandler.e
->
-> An object to simplify adding Input Handlers using the
-> input.device.
-> 
OPT MODULE

MODULE 	'exec/ports',		'exec/io',	   		'exec/interrupts',
		'exec/lists',		'exec/nodes'	
MODULE 	'devices/input'
MODULE 	'other/ecodex'
  
->
-> inputhandler
->
-> The object maintains its own device pointers, so each
-> new object opens the device again.  This should not be
-> a problem.
->

EXPORT OBJECT inputhandler
	io		:PTR TO iostd
	mp		:PTR TO mp
	is		:PTR TO is
	entry	:LONG
	data	:LONG
	flags	:INT
ENDOBJECT

->       <-
-> FLAGS <-
->       <-

SET IHO_INSTALLED,			-> The handler is currently installed
	IHO_INITIALIZED,		-> The handler has valid fields
	IHO_DUMMY

->         <-
-> METHODS <-
->         <-

->
-> init()
->
-> This opens the device and sets up the interrupt server.
-> If the handler was currently installed, this will uninstall
-> it.
->

PROC init(entry, data, pri, name) OF inputhandler HANDLE
	-> If we're already intialized, there's no need
	-> to re-open the device
	IF (self.flags AND IHO_INITIALIZED)=NIL
		-> Message Port
		self.mp := CreateMsgPort()
		IF self.mp=NIL THEN Throw("MP", "INIT")
		
		-> IO Request
		self.io := CreateIORequest(self.mp, SIZEOF iostd)
		IF self.io=NIL THEN Throw("IO", "INIT")
		
		-> Open the device
		IF OpenDevice('input.device', 0, self.io, 0) THEN Throw("DEV", "OPEN")
	
	ENDIF
	
	-> If the handler is currently installed, we should remove it now
	IF (self.flags AND IHO_INSTALLED)<>NIL THEN self.remove()
	
	-> Interrupt server
	IF self.is=NIL THEN NEW self.is
	self.is.code := eCodeInputHandler(entry)
	self.is.data := data
	self.is.ln.pri  := pri
	self.is.ln.name := name

	-> Save parameters
	self.entry 	:= entry
	self.data	:= data
	
	-> Set flags
	self.flags := IHO_INITIALIZED
EXCEPT
	IF exception="NEW" THEN ReThrow()
	RETURN exception					-> Some error happened
ENDPROC NIL

->
-> install()
->
-> This installs the input handler, using the previously defined
-> stuff.
->

PROC install() OF inputhandler
	IF (self.flags AND IHO_INSTALLED)=NIL AND 
	   			(self.flags AND IHO_INITIALIZED)<>NIL
		self.io.command := IND_ADDHANDLER
		self.io.data	:= self.is
		DoIO(self.io)
		
		-> Set flags
		self.flags := self.flags OR IHO_INSTALLED
	ENDIF
ENDPROC

->
-> remove()
->
-> This uninstalls the input handler that was previously
-> installed.
->

PROC remove() OF inputhandler
	IF (self.flags AND IHO_INSTALLED)<>NIL AND
				(self.flags AND IHO_INITIALIZED)<>NIL
		self.io.command := IND_REMHANDLER
		self.io.data	:= self.is
		DoIO(self.io)
		
		-> Set flags
		self.flags := self.flags AND Not(IHO_INSTALLED)
	ENDIF
ENDPROC

->
-> end()
->
-> This removes the handler, closes the device, and
-> de-allocates the interrupt server structure.
->

PROC end() OF inputhandler
	IF (self.flags AND IHO_INITIALIZED)<>NIL
		IF (self.flags AND IHO_INSTALLED) THEN self.remove()
		CloseDevice(self.io)
		DeleteIORequest(self.io)
		DeleteMsgPort(self.mp)
		END self.is
		
		-> Set flags
		self.flags := self.flags AND Not(IHO_INITIALIZED)
	ENDIF
ENDPROC