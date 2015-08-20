OPT PREPROCESS

MODULE '*sound'
MODULE 'graphics', 'dos/rdargs', 'dos/dos'

PROC main()
	DEF rdargs:PTR TO rdargs, list:PTR TO LONG
	rdargs := ReadArgs('NAME/A', list, NIL)
	play(list[])
	FreeArgs(rdargs)
ENDPROC

PROC play(name)
	DEF snd:sound
	NEW snd
	
	WriteF('\s...', name)
	IF snd
		IF snd.load(name)=NIL
			WriteF('Playing ')
			
				-> Tell user type
			IF snd.type="DTYP" THEN WriteF('using datatypes...')
			IF snd.type="8SVX" THEN WriteF('as 8SVX...')
			IF snd.type="RAW" THEN WriteF('as RAW...')
			
				-> Play sounds
			IF snd.play(2) THEN WriteF('Channel 2 failed..')
			IF snd.play(3) THEN WriteF('Channel 3 failed..')
			WriteF('Waiting...')
			Wait((snd.sigbits() OR SIGBREAKF_CTRL_C))
			WriteF('Done.\n')
			
				-> Output sample info
			WriteF('Played \d samples @ \d samples per second.\n', snd.length, snd.rate())
			
				-> Give accurate playing time
			WriteF('Time spent playing was: \d milliseconds.\n', ((snd.length * 1000) / snd.rate()))
		ELSE
			WriteF('Load failed.\n')
		ENDIF
		END snd
	ELSE
		WriteF('Out of memory!\n')
	ENDIF
ENDPROC
