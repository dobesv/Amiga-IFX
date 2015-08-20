OPT MODULE
OPT EXPORT
->
-> modules/action.e
->
-> CONSTs and definitions for actions
->
-> var in first set of quotes is "data", second set should be "ext"
->

ENUM ACT_NOTHING=0,		-> Use how you like
	 ACT_EXTEND,		-> Get type and ext from "nextaction"
	 ACT_SELECT,		-> Tell "gadget" it's been selected
	 ACT_VALUE,			-> Set "gadget" to new "value"
	 ACT_EXECUTE,		-> Run a "command"
	 ACT_AREXX,			-> Send "AREXX port" a "command"
	 ACT_IDCMP,			-> Send a dummy "intuimessage" to "window"'s port
	 ACT_PROCESS,		-> Create a process at "entry point" w/ "args"
	 ACT_TASK,			-> Create a task at "entry point"
	 ACT_FUNCTION,		-> Call the function at "entry point"
	 ACT_MESSAGE,		-> Send "message" to "port"
	 ACT_SOUND,			-> Play a sound (described by "sound")
	 ACT_SOUNDFILE,		-> Play a sound (load from "filename")
	 ACT_REQUESTER,		-> Use string in "data".  Buttons are "Okay|Cancel"
	 ACT_IFX,			-> Do another id
	 ACT_MESSAGE,		-> Pop a requester with a cute "note"
	 ACT_SPEECH,		-> "Say" some "stuff"
	 ACT_DUMMY			-> For crash testing?

