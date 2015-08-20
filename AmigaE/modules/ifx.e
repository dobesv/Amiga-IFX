OPT MODULE
OPT EXPORT
->
-> IFX.e
-> © 1997 by Dobes Vandermeer
->
-> OBJECTs and constants for remote controlling IFX.
-> 

MODULE 'exec/nodes',		'exec/ports'

->
-> ifx_msg - message types
->

ENUM IFX_ID=0,			-> Play effect (name in "id")
	 IFX_EFFECT,		-> Do effect (incl. as object in "data")
	 IFX_ACTION,		-> Do action (incl. as structure in "data")
	 IFX_CHIMES=333,	-> If "data" is set TRUE, then turn chimes ON
	 IFX_PARSE,			-> Parse the command line in "id"
	 IFX_QUIT=666,		-> Exit the program (no args)
	 IFX_EDIT_IDS,		-> Get "data" as pointer to top of ids list
	 IFX_EDIT_TASKS,	-> Get "data" as pointer to top of tasks list
	 IFX_EDIT_SEM,		-> Get "data" as pointer to editing semaphore
		 				-> Please, when editing the list, use the
						-> the semaphore I hav provided.  Lock it as
						-> shared for read-only access, or full lock
						-> for writing.  This will avoid crashes/bugs
						-> in programs.
	 IFX_DUMMY

->
-> The lists:
->
-> When editing IDs, please ObtainSemaphore() the public semaphore
-> that you can get using IFX_EDIT_SEM.  When reading IDs, use
-> ObtainSemaphoreShared().
->
-> Each list is given as the pointer to the FIRST entry in the
-> list.  The IDs list is doubly linked, but the last entry is not
-> available; the double-link is just for removing/adding nodes.
->
-> The task exclusions are singly linked, from top to bottom, using
-> a Last-in, First-out scheme.  These are more trouble to edit,
-> but they are less useful anyway.
->
-> The pointers you get are NOT copies.  They are the actual memory.
-> Any changes you make will change IFX's behaviour real-time.
->
-> The ID list IS compatible with a normal list.
->

->
-> ifx_id:  This is the object I use in my internal ids list.
-> The fields are described.
->

OBJECT ifx_id
	next:PTR TO ifx_id			-> Next entry in list
	prev:PTR TO ifx_id			-> Previous entry in list
	num	:INT					-> Number of actions added
	id	:PTR TO CHAR			-> ID name
	action	:PTR TO ifx_action	-> Action list
ENDOBJECT

OBJECT ifx_msg OF mn			-> Includes MN, so message compatible
	cmd	:INT					-> See above
	id	:PTR TO CHAR
	data:LONG
ENDOBJECT

OBJECT ifx_action
	type		:LONG					-> See action.e
	data		:PTR TO CHAR			-> See action.e
	nextaction	:PTR TO ifx_action
	ext			:INT					-> Cycles
ENDOBJECT

OBJECT ifx_task
	nexttask	:PTR TO ifx_task
	pat			:PTR TO CHAR		-> Create using ParsePatternNoCase()
ENDOBJECT
