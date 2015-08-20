->
-> prefs.e
->
-> An attempt at using GadTools to do the IFX preferences 
-> program.
->

-> Modules
MODULE 'dos',		'dos/dos',		'dos/dosextens',	'dos/dostags'
MODULE 'asl',		'libraries/asl'
MODULE 'intuition', 'intuition/intuition', 'intuition/gadgetclass',
		'intuition/screens'
MODULE 'graphics',  'graphics/text'
MODULE 'gadtools',  'libraries/gadtools'
MODULE 'exec/ports'
MODULE 'utility/tagitem'

MODULE 'exec/obj/list', 	'exec/obj/node'
MODULE 'other/split'
MODULE '*/modules/action'
MODULE 'amigalib/tasks'
MODULE 'tools/sound'
MODULE 'fabio/rxobj_oo'

->
-> Object definitions
->

	-> ID
	
OBJECT ifx_id OF node
	list	:PTR TO list		-> Action list
ENDOBJECT

    -> Action

OBJECT ifx_action OF node
	type		:PTR TO CHAR
ENDOBJECT
	
-> Gadget ID's
ENUM LV_IDS,
	 LV_ACTS,
	 BUT_SAVE,
	 BUT_CANCEL,
	 BUT_ADDID,
	 BUT_REMID,
	 BUT_ADDACT,
	 BUT_REMACT,
	 BUT_OKAYID,
	 BUT_OKAYACT,
	 BUT_CANCELACT,
	 BUT_FILEREQ,
	 BUT_TESTACT,
	 STR_INFO,
	 STR_ID,
	 CY_TYPE,
	 ID_DUMMY

	-> Gadget objects
DEF	lv_ids		:PTR TO gadget,
	lv_acts		:PTR TO gadget,
	but_save	:PTR TO gadget,
	but_cancel	:PTR TO gadget,
	but_okayid	:PTR TO gadget,
	but_addid	:PTR TO gadget,
	but_remid	:PTR TO gadget,
	but_addact	:PTR TO gadget,
	but_remact	:PTR TO gadget,
	but_filereq	:PTR TO gadget,
	but_testact	:PTR TO gadget,
	str_info	:PTR TO gadget,
	str_id		:PTR TO gadget,
	cy_type		:PTR TO gadget,
	but_okayact	:PTR TO gadget
	
-> Global variables
	-> Font
DEF topaz80:PTR TO textattr

	-> Sounds
DEF sounds:PTR TO LONG, curr

	-> Ids
DEF ids:PTR TO list

	-> Port
DEF g_mp:PTR TO mp
	
	-> Current ID and Action
DEF id_curr:PTR TO ifx_id, act_curr:PTR TO ifx_action

	-> Last directory name
DEF last_dir:PTR TO CHAR

-> MAIN
PROC main() HANDLE
	DEF font
	
	-> Allocate sounds space
	NEW sounds[2]
	
	-> Create our ids list
	NEW ids.init()
	IF ids=NIL THEN Throw("MEM", ' (for IDS list)')
	
	-> Create our global message port
	g_mp := CreateMsgPort()
	IF g_mp=NIL THEN Throw("INIT", ' (message port)')
	
	-> Load the configuration
	IF load_config() THEN put_error('Unable to open prefs file.\n')
	
	-> Open ASL
	IF aslbase=NIL THEN aslbase := OpenLibrary('asl.library', 37)
	IF aslbase=NIL /* Still */ THEN RETURN FALSE
	
	-> Open Gadtools
	IF gadtoolsbase=NIL THEN gadtoolsbase := OpenLibrary('gadtools.library', 37)
	IF gadtoolsbase=NIL /* Still */ THEN RETURN FALSE
	
	-> Open Datatypes
	IF datatypesbase=NIL THEN datatypesbase := OpenLibrary('datatypes.library', 0)
	
	-> Initialize some stuff
	topaz80 := ['topaz.font', 8, 0, 0]:textattr			-> Default font
	
	-> Open the font, to make sure it's possible
	IF (font := OpenFont(topaz80))<>NIL
		-> Do the stuff
		dowindow({maingadgets})
		
		-> Close the font
		CloseFont(font)
	ELSE
		put_error('Unable to open topaz font!\n')
	ENDIF	

EXCEPT DO
	IF font			THEN CloseFont(font)
	IF gadtoolsbase THEN CloseLibrary(gadtoolsbase)
	gadtoolsbase := NIL
	IF aslbase		THEN CloseLibrary(aslbase)
	aslbase := NIL
	IF datatypesbase THEN CloseLibrary(datatypesbase)
	datatypesbase := NIL
	IF sounds[0] THEN END sounds[0]
	IF sounds[1] THEN END sounds[1]
ENDPROC

PROC dowindow(makegadgets)
	DEF myscr		:PTR TO screen,
		mywin		:PTR TO window,
		glist		:PTR TO gadget,
		gad			:LONG,
		userdata	:LONG,
		done=0		:LONG,
		vi			:LONG
		

	-> Lock the public screen
	IF (myscr := LockPubScreen(NIL))<>NIL
		-> Get visual info
	    IF (vi := GetVisualInfoA(myscr, [TAG_END, NIL]))<>NIL
	    	gad := makegadgets(myscr, vi, {glist})
	    	IF gad
		    	-> Open the window
		    	IF (mywin := OpenWindowTagList(NIL,
			    		[WA_TITLE,		'IFX Preferences V2.00',
		    			 WA_GADGETS,			glist,
	    				 WA_AUTOADJUST, 		TRUE,
	    				 WA_WIDTH,				480,
		    			 WA_INNERHEIGHT,		130,
		    			 WA_DRAGBAR,			1,
			    		 WA_CLOSEGADGET,		1,
		    			 WA_DEPTHGADGET,		1,
	    				 WA_ACTIVATE,			1,
	    				 WA_SIMPLEREFRESH,		1,
		    			 WA_IDCMP,			IDCMP_CLOSEWINDOW OR
		    			 					IDCMP_REFRESHWINDOW OR
		    		 						IDCMP_VANILLAKEY OR
	    			 						LISTVIEWIDCMP OR
	    			 						BUTTONIDCMP OR
	    			 						STRINGIDCMP OR
	    			 						CYCLEIDCMP,
		    			 WA_PUBSCREEN, 			myscr,
			    		 NIL, 					NIL]))<>NIL
			    	
			    	-> Set userdata
			    	mywin.userdata := makegadgets

			    	-> Refresh the gadgets
			    	Gt_RefreshWindow(mywin, NIL)
			    	
			    	WHILE done=NIL
				    	
				    	-> Do that gadget/message THANG..
				    	makegadgets := handlewindow(mywin, userdata)
				    	
				    	IF makegadgets
					    	-> Free up the old gadgets
					    	RemoveGList(mywin, glist, 0)
					    	FreeGadgets(glist)
				    		
					    	-> Make the new gadgets
					    	gad := makegadgets(myscr, vi, {glist})
					    	IF gad
					    		-> Add the gadgets
					    		AddGList( mywin, glist, -1, -1, NIL ) 
					    		
					    		-> Erase the old gadgets
					    		EraseRect(mywin.rport, mywin.borderleft, mywin.bordertop, mywin.width-mywin.borderright-1, mywin.height-mywin.borderbottom-1)
					    		
					    		-> Draw the new ones
					    		RefreshGadgets(glist, mywin, NIL)
					    		
					    		-> Refresh'em for fun!
					    		Gt_RefreshWindow(mywin, NIL)
					    	ELSE
					    		done := 1
					    	ENDIF
					    ELSE
					    	done := 1
					    ENDIF
				    ENDWHILE
			    	
			    	-> Close the window
			    	CloseWindow(mywin)
	    		 ELSE
	    		 	put_error('Unable to open window!\n')
	    		 ENDIF
	    	ELSE
	    		put_error('Unable to create gadgets!\n')
	    	ENDIF
		    	FreeGadgets(glist)
		    	FreeVisualInfo(vi)
		ELSE
			put_error('Unable to get visual info!\n')
			ENDIF
			
		UnlockPubScreen(NIL, myscr)
	ELSE
		put_error('Unable to lock public screen!\n')
	ENDIF
ENDPROC


PROC handlewindow(win:PTR TO window, userdata)
	DEF imsg:PTR TO intuimessage,
		class, code, id, object, gad:PTR TO gadget,
		ifx:PTR TO ifx_id, act:PTR TO ifx_action,
		rx:PTR TO rxobj,
		obj:PTR TO node,
		thwin:PTR TO window,
		asl:PTR TO filerequester,
		temp:PTR TO CHAR,
		done=NIL
	
	-> Set ourselves to global status
	done
	
	WHILE done=NIL
		WaitPort(win.userport)
		WHILE (imsg := Gt_GetIMsg(win.userport))<>NIL
			class  := imsg.class
			code   := imsg.code
			object := imsg.iaddress
			thwin  := imsg.idcmpwindow
			
			SELECT class
				CASE IDCMP_REFRESHWINDOW
					-> Refresh the window and its gadgets
					Gt_BeginRefresh(thwin)
					Gt_EndRefresh(thwin, 1)
				
				CASE IDCMP_GADGETDOWN
				CASE IDCMP_MOUSEMOVE
				CASE IDCMP_GADGETUP
					gad := object
					id := gad.gadgetid
					
					gadgethit:
					
					SELECT id
						CASE LV_IDS
							IF lv_ids THEN killlist(lv_ids, win)
							id_curr := entrynum(ids, code)
							done := {idgadgets}
							
						CASE LV_ACTS
							IF id_curr
								IF lv_acts THEN killlist(lv_acts, win)
								act_curr := entrynum(id_curr.list, code)
								done := {actgadgets}
							ENDIF
							
						CASE BUT_SAVE
							-> Save the config
							save_config()
							
							-> Inform IFX
							NEW rx.rxobj('IFXPREFS')
							rx.send('PLAY', 'prefs', NIL, NIL)
							END rx
							done := -1
 							
						CASE BUT_CANCEL
							IF ask('Are you sure you want\nto exit without saving?', win)
								-> Yes
								done := -1
							ELSE
								-> No
							ENDIF	
						
						CASE BUT_OKAYID
							IF id_curr AND str_id
								-> Read the new string
								END id_curr.name
								
								obj := str_id.specialinfo::stringinfo.buffer
								IF obj
									NEW id_curr.name[StrLen(obj)+1]
									StrCopy(id_curr.name, obj)
								ENDIF
								
								-> Nullify the id
								id_curr := NIL
							ELSE
								put_error('DEBUG: Error @ 289')
							ENDIF
							done := {maingadgets}

						CASE BUT_ADDID
							killlist(lv_ids, win)
							id_curr := newid('new_id')
							IF id_curr
								done := {idgadgets}
							ELSE
								put_error('Unable to create new ID!')
							ENDIF
							
						CASE BUT_REMID
							IF id_curr
								IF ask('Are you sure you want to\nremove this ID?', win)
									-> Yes
									END id_curr
									id_curr := NIL
									done := {maingadgets}
								ENDIF
							ELSE
								put_error('DEBUG: Error 274')
							ENDIF
							
						CASE BUT_ADDACT
							IF id_curr
								-> Clear old list (not destroy)
								IF lv_ids THEN killlist(lv_ids, win)
								
								-> Create new action
								NEW act_curr
								IF act_curr
									id_curr.list.addtail(act_curr)
									done := {actgadgets}
								ELSE
									put_error('Unable to create new Action.\n')
								ENDIF
							ELSE
								put_error('DEBUG: Error 286')
							ENDIF
							
						CASE BUT_REMACT
							IF act_curr
								act_curr.remove()
								END act_curr
								act_curr := NIL
								done := {idgadgets}
							ENDIF
						
						CASE BUT_OKAYACT
							IF act_curr AND str_info
								-> Read the new string
								END act_curr.name
								
								obj := str_info.specialinfo::stringinfo.buffer
								IF obj
									NEW act_curr.name[(StrLen(obj)+1)]
									IF act_curr.name
										CopyMem(obj, act_curr.name, StrLen(obj)+1)
									ENDIF
								ENDIF
								
								-> Nullify the action
								act_curr := NIL
								-> Go back to the ID window
								done := {idgadgets}
							ELSE
								put_error('DEBUG: Error @ 330')
							ENDIF
						
						CASE BUT_FILEREQ
							-> If there is a string in the gadget, we'll
							-> use it for our drawer
							obj := str_info.specialinfo::stringinfo.buffer
							IF obj
								IF StrLen(obj)>1
									-> Allocate memory
									NEW id[StrLen(obj)+2]
									
									-> Copy it
									StrCopy(id, obj)
									
									-> Slash the file part
									temp := PathPart(id)
									temp[] := "\0"
								ELSE
									id := last_dir
									temp := NIL
								ENDIF
							ELSE
								id := last_dir
								temp := NIL
							ENDIF
							
							-> Create the request
							asl := AllocAslRequest(ASL_FILEREQUEST,
									[ASL_HAIL,			'Select A File',
									 ASL_WINDOW,		win,
									 ASL_LEFTEDGE,		win.leftedge,
									 ASL_TOPEDGE,		win.topedge,
									 ASL_WIDTH,			win.width,
									 ASL_HEIGHT,		win.height,
									 ASL_DIR,			IF id THEN id ELSE '',
									 ASL_FILE,			IF temp THEN temp ELSE '',
									 NIL,				NIL])
								
							-> If successful
							IF asl
								IF RequestFile(asl)<>NIL
									-> asl.drawer and asl.file are the strings
									NEW temp[StrLen(asl.file) + StrLen(asl.drawer)+1]
									IF temp
										IF AddPart(temp, asl.drawer, ALL)
										IF AddPart(temp, asl.file, ALL)
										-> Save dir for later use
											IF asl.drawer
												END last_dir
												NEW last_dir[StrLen(asl.drawer)+1]
												IF last_dir
													StrCopy(last_dir, asl.drawer)
												ELSE
													put_error('Out of memory @ 443')
												ENDIF
											ENDIF
											
											-> Set gadget, where we'll read it from later
											Gt_SetGadgetAttrsA(str_info, win, NIL,
												[GTST_STRING, 		temp,
												 NIL,				NIL])
												 
										ENDIF
										ENDIF
										END temp
									ELSE
										put_error('Out of memory for string!')
									ENDIF
								ENDIF
								FreeAslRequest(asl)
							ENDIF
						
						CASE BUT_TESTACT
							IF lv_acts THEN killlist(lv_acts, win)
							IF act_curr
								temp := str_info.specialinfo::stringinfo.buffer
								IF temp
									-> Destroy old
									END act_curr.name
									
									-> Make new
									NEW act_curr.name[StrLen(temp)+1]
									IF act_curr.name
										CopyMem(temp, act_curr.name, StrLen(temp)+1)
										do_action(act_curr)
									ELSE
										put_error('Out of memory @ 446', win)
										done := -1
									ENDIF
								ENDIF
							ENDIF
							
						CASE CY_TYPE
							IF act_curr
								SELECT code
									CASE 0	-> "Sound"
										act_curr.type := ACT_SOUNDFILE
									CASE 1	-> "Executable"
										act_curr.type := ACT_EXECUTE
									CASE 2	-> "Other ID"
										act_curr.type := ACT_IFX
									CASE 3  -> "Nothing"
										act_curr.type := ACT_NOTHING
								ENDSELECT
							ELSE
								put_error('DEBUG: Error @ 337')
							ENDIF
							
						CASE STR_INFO
						CASE STR_ID
					ENDSELECT
				
				CASE IDCMP_VANILLAKEY
					SELECT code
						CASE "n" -> New ID
							id := BUT_ADDID
						CASE "a" -> Add action
							id := BUT_ADDACT
						CASE "r" -> Remove ID
							id := BUT_REMID
						CASE "k" -> Keep ID
							id := BUT_OKAYID
						CASE "d" -> Delete Action
							id := BUT_REMACT
						CASE "s" -> Save
							id := BUT_SAVE
						CASE "c" -> Cancel Save
							id := BUT_CANCEL
						CASE "t"
							id := BUT_TESTACT
						DEFAULT
							id := NIL
					ENDSELECT
					IF id THEN JUMP gadgethit
					
				CASE IDCMP_CLOSEWINDOW
					IF ask('Do you really want\nto quit?  Your prefs\nwill not be saved!', win)
						done := -1
					ENDIF
			ENDSELECT
			
			-> Loop to next message
		ENDWHILE
	ENDWHILE
	
	-> Set up the ID name when the screen goes off of it
	IF id_curr AND (done = {actgadgets})
		-> Read the new string
		IF id_curr.name THEN END id_curr.name
		
		IF str_id
			obj := str_id.specialinfo::stringinfo.buffer
			IF obj
				NEW id_curr.name[StrLen(obj)+1]
				IF id_curr.name
					CopyMem(obj, id_curr.name, StrLen(obj)+1)
				ELSE
					put_error('Out of memory @ 517', win)
				ENDIF
			ELSE
				put_error('Missing string @ 520', win)
			ENDIF
		ENDIF
	ENDIF
	
	-> Check for total exit
	IF done=-1 THEN RETURN NIL
	
ENDPROC done

PROC maingadgets(scr:PTR TO screen, vi, glistptr)
	DEF ng:PTR TO newgadget,
		gad:PTR TO gadget,
		topborder
		
		-> Allocate the memory
		NEW ng
		
    	-> Get the top border size
    	topborder := scr.wbortop + scr.font.ysize + 4
    	
    	-> Create the context
    	gad := CreateContext(glistptr)
    	
    	-> Create the listview gadget
    	ng.leftedge := scr.wborleft+2
    	ng.topedge  := topborder
    	ng.width    := 480-scr.wborleft-scr.wborright-2
	    	ng.height	:= 85
    	ng.gadgettext := ''
    	ng.textattr := topaz80
    	ng.gadgetid := LV_IDS
    	ng.flags	:= NIL
    	ng.visualinfo := vi
    	ng.userdata := NIL			-> TODO
    	
    	lv_ids := gad := CreateGadgetA(LISTVIEW_KIND, gad, ng,
    		[GTLV_LABELS,		ids,
    		 NIL,				NIL])
    		 
    	-> Create the "Add" button
    	ng.topedge	:= ng.topedge + ng.height + 5
    	ng.height	:= 15
    	ng.gadgettext := '_New'
    	ng.gadgetid	:= BUT_ADDID
    	ng.userdata := NIL			-> TODO
    	
    	but_addid := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
    		[GT_UNDERSCORE,		"_",
    		 NIL,				NIL])
    	
    	-> Create the "Cancel" button
    	ng.topedge	:= ng.topedge + ng.height + 5
    	ng.leftedge := ng.leftedge + ng.width
    	ng.width	:= (ng.width/2)-10
    	ng.leftedge := ng.leftedge - ng.width
    	ng.gadgettext := '_Cancel'
    	ng.gadgetid	:= BUT_CANCEL
    	ng.userdata := NIL			-> TODO
    	
    	but_cancel := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
	    		[GT_UNDERSCORE,		"_",
	    		 NIL,				NIL])

    	-> Create the "Save" button
    	ng.leftedge := scr.wborleft+2
    	ng.gadgettext := '_Save'
    	ng.gadgetid := BUT_SAVE
    	ng.userdata := NIL			-> TODO
    	but_save := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
    			[GT_UNDERSCORE,		"_",
	    		 NIL,				NIL])

ENDPROC gad, NIL

PROC idgadgets(scr:PTR TO screen, vi, glistptr)
	DEF ng:PTR TO newgadget,
		gad:PTR TO gadget,
		topborder
		
		-> Allocate the memory
		NEW ng
		
    	-> Get the top border size
    	topborder := scr.wbortop + scr.font.ysize + 4
    	
    	-> Create the context
    	gad := CreateContext(glistptr)
    	
    	-> Create the listview gadget
    	ng.leftedge := scr.wborleft+2
    	ng.topedge  := topborder
    	ng.width    := 480-scr.wborleft-scr.wborright-2
	    ng.height	:= 65
    	ng.gadgettext := ''
    	ng.textattr := topaz80
    	ng.gadgetid := LV_ACTS
    	ng.flags	:= NIL
    	ng.visualinfo := vi
    	ng.userdata := id_curr
    	
    	lv_acts := gad := CreateGadgetA(LISTVIEW_KIND, gad, ng,
    		[GTLV_LABELS,		id_curr.list,
    		 NIL,				NIL])
    	
		-> Create the ID name entry string
    	ng.topedge		:= ng.topedge + ng.height + 5
    	ng.height		:= 15
    	ng.gadgetid		:= STR_ID
    	str_id := gad := CreateGadgetA(STRING_KIND, gad, ng,
    		[GT_UNDERSCORE,			"_",
			 GTST_STRING,			id_curr.name,
			 GTST_MAXCHARS,			256,
			 NIL,					NIL])
	 
    	-> Create the "Add" button
    	ng.topedge		:= ng.topedge + ng.height + 5
    	ng.height		:= 15
    	ng.gadgettext	:= '_Add an Action'
    	ng.gadgetid		:= BUT_ADDACT
    	
    	but_addact := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
    		[GT_UNDERSCORE,		"_",
    		 NIL,				NIL])
    	
    	-> Create the "OK" button
    	ng.topedge		:= ng.topedge + ng.height + 5
    	ng.gadgettext	:= '_Keep this'
    	ng.gadgetid		:= BUT_OKAYID
    	ng.width		:= (ng.width/3)
    	but_okayid := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
    		[GT_UNDERSCORE, 	"_",
    		 NIL,				NIL])
    		 
    	
    	-> Create the "Remove" Button
    	ng.gadgettext	:= '_Remove This ID'
    	ng.gadgetid		:= BUT_REMID
		ng.leftedge		:= ng.leftedge + (2*ng.width)
	    but_remid := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
    		[GT_UNDERSCORE,		"_",
    		 NIL,				NIL])
    	
ENDPROC gad, lv_acts

PROC actgadgets(scr:PTR TO screen, vi, glistptr)
	DEF ng:PTR TO newgadget,
		gad:PTR TO gadget,
		typenum,
		topborder
		
		typenum := act_curr.type
		SELECT typenum
			CASE ACT_SOUND
				typenum := 0
			CASE ACT_SOUNDFILE
				typenum := 0
			CASE ACT_EXECUTE
				typenum := 1
			CASE ACT_IFX
				typenum := 2
			CASE ACT_NOTHING
				typenum := 3
		ENDSELECT
		
		-> Allocate the memory
		NEW ng
		
    	-> Get the top border size
    	topborder := scr.wbortop + scr.font.ysize + 4
    	
    	-> Create the context
    	gad := CreateContext(glistptr)
    	
    	-> Create the cycle gadget
    	ng.leftedge := scr.wborleft+2
    	ng.topedge  := topborder
    	ng.width    := 480-scr.wborleft-scr.wborright-2
	    ng.height	:= 15
    	ng.gadgettext := '_Type'
    	ng.textattr := topaz80
    	ng.gadgetid := CY_TYPE
    	ng.flags	:= NIL
    	ng.visualinfo := vi
    	ng.userdata := act_curr			-> TODO
    	cy_type := gad := CreateGadgetA(CYCLE_KIND, gad, ng,
    		[GTCY_LABELS,		['Sound', 'Command', 'Other ID', 'Do Nothing', NIL],
    		 GTCY_ACTIVE,		typenum,
    		 NIL,				NIL])
    	
    	-> Create the string gadget
		ng.topedge 		:= ng.topedge + 20
		ng.gadgettext	:= '_Info'
		ng.gadgetid		:= STR_INFO
		str_info := gad := CreateGadgetA(STRING_KIND, gad, ng,
			[GT_UNDERSCORE,			"_",
			 GTST_STRING,			act_curr.name,
			 GTST_MAXCHARS,			512,
			 NIL,					NIL])
		
		-> Create the 'File...' gadget
		ng.topedge 		:= ng.topedge + 20
		ng.gadgettext	:= '_File...'
		ng.gadgetid		:= BUT_FILEREQ
		but_filereq := gad := CreateGadgetA(BUTTON_KIND, gad, ng, 
			[GT_UNDERSCORE,			"_",
			 NIL,					NIL])
		
		-> Create the "Okay" gadget
		ng.topedge		:= (ng.topedge * 2)+13
		ng.width        := ng.width / 3
		ng.gadgettext	:= '_Okay'
		ng.gadgetid		:= BUT_OKAYACT
		but_okayact := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
			[GT_UNDERSCORE,			"_",
			 NIL,					NIL])
		
		-> Create the "Remove" gadget
		ng.leftedge		:= ng.leftedge + (2*ng.width)
		ng.gadgettext	:= '_Delete this Action'
		ng.gadgetid		:= BUT_REMACT
		but_remact := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
			[GT_UNDERSCORE,			"_",
			 NIL,					NIL])
		
		-> Create the "Test" gadget
		ng.topedge		:= ng.topedge - 40
		ng.gadgettext	:= 'Test'
		ng.gadgetid		:= BUT_TESTACT
		but_testact := gad := CreateGadgetA(BUTTON_KIND, gad, ng,
			[GT_UNDERSCORE, 		"_",
			 NIL,					NIL])
ENDPROC gad

	->
	-> load_config()
	->
	-> This is not compatible with the IFX code, because it loads
	-> everything into a different type of list, and sets different
	-> global variables.
	->

PROC load_config() HANDLE
	DEF file, buf, temp:PTR TO CHAR, done=0, list:PTR TO LONG
	DEF ifx:PTR TO ifx_id, action:PTR TO ifx_action
	DEF type
	
	-> Open the file
	file := Open('s:IFX.ids', MODE_OLDFILE)
	IF file
		NEW buf[1024]
		IF buf
			WHILE done=0
				-> Read a line
				IF Fgets(file, buf, 1024)=NIL THEN done:=1
				
				-> Set string to lower case
				LowerStr(buf)
				
				-> Split the line into pieces
				list := argSplit(buf)
				IF list=NIL THEN Throw("MEM", ' (for arg list)')
				
				->
				-> All types MUST be longer then 3 chars, otherwise
				-> they will be considered comments
				->
				IF StrLen(list[0])>3
				
					-> Find type
					type := NIL
					IF StrCmp(list[0], 'sound')   THEN type	:= ACT_SOUNDFILE
					IF StrCmp(list[0], 'exec')    THEN type	:= ACT_EXECUTE
					IF StrCmp(list[0], 'other')   THEN type	:= ACT_IFX
					IF StrCmp(list[0], 'defprefsdir')
						type := NIL			-> Prevent unnecessary pain
						IF last_dir=NIL		-> Only accept one
							-> Allocate memory
							NEW last_dir[StrLen(list[1]+1)]
							IF last_dir
								StrCopy(last_dir, list[1])
							ELSE
								put_error('Out of memory @ 808')
							ENDIF
						ENDIF
					ENDIF
					IF StrCmp(list[0], 'nothing') THEN type := -1
					
					IF type<>NIL
						-> Check if the ID already exists
						ifx := ids.findname(list[1])
						IF ifx=NIL THEN ifx := newid(list[1])
						
						-> Special case for type: ACT_NOTHING
						IF type=-1 THEN type := ACT_NOTHING		/* NIL */
						
						-> Add the entry
						IF list[2]
							-> Allocate the structure
							NEW action
					        IF action=NIL THEN Throw("MEM", ' (for action data)')
					        
							-> Get the data string
							NEW temp[StrLen(list[2])+1]
							IF temp=NIL THEN Throw("MEM", ' (for action string)')
							StringF(temp, '\s', list[2])
							action.name 		:= temp
							
							-> Set the type
							action.type			:= type
							
							-> Add it to the list
							ifx.list.addtail(action)
				    	ENDIF
					ENDIF
				ENDIF
				
				-> Free the list
				DisposeLink(list)
			ENDWHILE
			
			-> Free buffer
			END buf
		ELSE
			Throw("MEM", ' (for read buffer)')
		ENDIF
		Close(file)
	ELSE
		Throw("INIT", "LOAD")
	ENDIF
EXCEPT
	IF list THEN DisposeLink(list)
	IF file THEN Close(file)
	RETURN exception
ENDPROC

PROC save_config() HANDLE
	DEF file, buf:PTR TO CHAR, idname:PTR TO CHAR, actname:PTR TO CHAR
	DEF ifx:PTR TO ifx_id, act:PTR TO ifx_action
	DEF sel
	
	-> Error prevention
	IF ids=NIL THEN RETURN -1
	IF ids.is_empty() THEN RETURN NIL
	
	-> Open the file
	file := Open('s:IFX.ids', MODE_NEWFILE)
	IF file
		-> Allocate a write buffer
		NEW buf[1024]
		IF buf
			-> Write out our little header
			StringF(buf, '*\n* IFX.ids    generated by:\n* IFX Preferences\n* © by Dobes Vandermeer\n*\n\ndefprefsdir \s', IF last_dir THEN last_dir ELSE 'RAM:')
			Fputs(file, buf)
			
			-> Loop through all the ids and write them out
			REPEAT
				-> Go to head id
				ifx := ids.head
				IF ifx=NIL THEN RETURN NIL
			    
				-> Set up name string
				idname := ifx.name
				
				-> Write out a comment line
				StringF(buf, '#\n## ID: "\s"\n#\n', idname)
				Fputs(file, buf)
				
				-> Write out all the effects
				WHILE ifx.list.is_empty()=NIL
					act := ifx.list.head	-> Get head
					IF act=NIL THEN RETURN NIL	-> Bad list
					sel := act.type
					SELECT sel
						CASE ACT_SOUNDFILE
							actname := 'SOUND'
						CASE ACT_EXECUTE
							actname := 'EXEC'
						CASE ACT_IFX
							actname := 'OTHER'
						CASE ACT_NOTHING
							actname := 'NOTHING'
						DEFAULT
							actname := NIL
					ENDSELECT
					
					IF actname AND idname
						-> Write out the data
						StringF(buf, '\s "\s" "\s"\n', actname, idname, act.name)
						Fputs(file, buf)
					ENDIF
					
					-> Remove and destroy the action
					END act
				ENDWHILE
				
				-> Remove and destroy the id
				END ifx
			UNTIL ids.is_empty()
			-> Write out our little footer
			StringF(buf, '*\n* END IFX.ids (generated by IFX Preferences)\n')
		ENDIF
		Close(file)
	ELSE
		Throw("SAVE", "OPEN")
	ENDIF
EXCEPT
	IF file THEN Close(file)
	IF buf THEN END buf
	RETURN exception
ENDPROC

->
-> ID functions
->

PROC newid(id:PTR TO CHAR) HANDLE
	DEF ifx:PTR TO ifx_id
	DEF list:PTR TO list
	DEF temp:PTR TO CHAR
	
	-> Create the object
	NEW ifx
	IF ifx=NIL THEN Throw("MEM", ' (for new id)')
	
	NEW list.init()
	IF list=NIL THEN Throw("MEM", ' (for new ids list)')
	ifx.list := list
	
	-> Copy the ID
	NEW temp[StrLen(id)+1]
	IF temp
		StringF(temp, '\s', id)
		ifx.name := temp
		ids.addtail(ifx)
	ELSE
		Throw("MEM", ' (for id string)')
	ENDIF
EXCEPT
	IF ifx  THEN END ifx
	IF list THEN END list
	IF temp THEN END temp
ENDPROC ifx

->
-> entrynum(l, n)
->
-> Get entry "n" of the list "l"
->

PROC entrynum(l:PTR TO list, num:LONG)
	DEF entry:PTR TO node
	DEF i
	-> Get the first entry
	entry := l.head
	
	-> Iterate to wanted entry
	FOR i := 1 TO num
		entry := entry.succ
	ENDFOR
ENDPROC entry

	->
	-> do_action(ext)
	->

PROC do_action(action:PTR TO ifx_action, ext=NIL)
	DEF type, data, temp
	DEF snd:PTR TO sound, tsnd:PTR TO sound
	DEF rexx:PTR TO rxobj
	
	-> Crash prevention
	IF str_info=NIL THEN RETURN
	IF act_curr=NIL THEN RETURN
	
	-> Assign our useful values here
->	data := action.name
	data := str_info.specialinfo::stringinfo.buffer
	type := action.type
	
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
		CASE ACT_SOUND
		CASE ACT_SOUNDFILE	
			NEW snd
			IF snd
				IF snd.load(data)=NIL
					-> If either of the sounds plays, we should wait for it
					IF (snd.play(0)=NIL) OR (snd.play(1)=NIL)
						Wait(snd.sigbits() OR SIGBREAKF_CTRL_C)
					ENDIF
				ENDIF
				END snd
			ENDIF
			
		CASE ACT_IFX
			NEW temp[1024]
			StrCopy(temp, 'id ')
			StrAdd(temp, data)
			NEW rexx.rxobj('IFXPREFS_ACTION')
			rexx.send('PLAY', temp, NIL, NIL)
			END rexx      
			END temp
	ENDSELECT
ENDPROC

->
-> put_error (str, win)
->
-> output errors in an extensible way
->

PROC put_error(str, win=NIL) IS EasyRequestArgs(win, 
								[SIZEOF easystruct, NIL, 'IFX Prefs Error', str, 'Okay'],
								NIL, NIL)

->
-> ask(str, win)
->
-> Ask user a yes or no question
->

PROC ask(str, win=NIL) IS EasyRequestArgs(win,
							[SIZEOF easystruct, NIL, 'IFX Preferences', str, 'Yes|No'],
							NIL, NIL)

->
-> Kill the listview's list
->

PROC killlist(gad, win)
	IF gad THEN RETURN Gt_SetGadgetAttrsA(gad, win, NIL, [GTLV_LABELS, -1, NIL, NIL])
ENDPROC -1

	->              <-
	-> ifx_#? PROCs <-
	->              <-

PROC end() OF ifx_id
	self.remove()
	self.list.die()
	END self.list
	END self.name
ENDPROC

	->
	-> Version string
	->
	
vers: CHAR 0, '$VER: IFX Preferences 2.00', 0
