/*

   PlaySamp.e               Michael Zucchi 1993

   A disk based raw sample player, written in AmigaE.

 */

MODULE  'tools/async',
    'dos/dos',
    'exec/io', 'exec/memory', 'exec/ports',
    'devices/audio'

CONST BUFFSIZE = 5120,
    BUFFCOUNT = 3

PROC main()

DEF args:PTR TO LONG,rdargs,rate,names:PTR TO LONG

args:=[0,0]
IF rdargs:=ReadArgs('Name/A/M,Rate/N',args,0)

   -> is there an easier way to do this crap?
    MOVE.L  args,A0
    MOVE.L  4(A0),D0
    BEQ.S   rate_def
    MOVE.L  D0,A0
    MOVE.L  (A0),D0
    CMP.L   #999,D0
    BLT.S   rate_def
    CMP.L   #60001,D0
    BLT.S   rate_ok
rate_def:
    MOVE.L  #10000,D0
rate_ok:
    MOVE.L  D0,rate

    names:=args[0];
    WHILE (names[0])
        play(names[]++,rate)
    ENDWHILE
    FreeArgs(rdargs)
ENDIF

ENDPROC

PROC play(filename,rate)

DEF arequest1:PTR TO ioaudio,
    arequest2:ioaudio,
    reply1,reply2,
    ior:PTR TO io,
    ioa:PTR TO ioaudio,
    mnode:PTR TO mn,
    buffer1,buffer2,
    file,
    len,
    go

WriteF('playing "\s"\n',filename)

IF file:=as_Open(filename,MODE_OLDFILE,BUFFCOUNT,BUFFSIZE)
    IF buffer1:=AllocVec(BUFFSIZE,MEMF_CHIP)
        IF buffer2:=AllocVec(BUFFSIZE,MEMF_CHIP)
            IF reply1:=CreateMsgPort()
                IF reply2:=CreateMsgPort()
                    IF arequest1:=CreateIORequest(reply1, SIZEOF ioaudio)

                        /* want to allocate and use any channel with OpenDev() */
                        arequest1.data:=[1,2,4,8]:CHAR
                        arequest1.length:=4;
                        ior:=arequest1
                        ior.command:=ADCMD_ALLOCATE
                        IF (0=OpenDevice('audio.device', 0, arequest1, 0))

                            /* copy iorequest for double buffered operation */
                            ior.flags:=ADIOF_PERVOL
                            arequest1.volume:=64            /* volume = MAX */
                            arequest1.period:=Div(3546895,rate)/* set frequency */
                            arequest1.cycles:=1             /* 1 cycle only */
                            ior.command:=CMD_WRITE
                            CopyMem(arequest1,arequest2,SIZEOF ioaudio)
                            mnode:=arequest2
                            mnode.replyport:=reply2

                            /* send first two buffer's full to device */
                            len:=as_Read(file,buffer1,BUFFSIZE)
                            arequest1.data:=buffer1
                            arequest1.length:=len
                            beginio(arequest1)
                            len:=as_Read(file,buffer2,BUFFSIZE)
                            arequest2.data:=buffer2
                            arequest2.length:=len
                            beginio(arequest2)

                            /* wait on request1 first time around */
                            mnode:=arequest1
                            go:=1

                            /* until done, keep feeding data to device */
                            WHILE (go)
                                WaitPort(mnode.replyport)
                                VOID GetMsg(mnode.replyport)
                                ioa:=mnode
                                IF ioa=arequest1
                                    mnode:=arequest2
                                ELSE
                                    mnode:=arequest1
                                ENDIF
                                IF ((len:=as_Read(file, ioa.data, BUFFSIZE))>0) AND (CheckSignal(SIGBREAKF_CTRL_C)=0)
                                    ioa.length:=len
                                    beginio(ioa)
                                ELSE
                                    go:=0
                                ENDIF
                            ENDWHILE

                            /* clean up */
                            WaitPort(mnode.replyport)
                            GetMsg(mnode.replyport)

                            CloseDevice(arequest1)
                        ENDIF
                        DeleteIORequest(arequest1)
                    ENDIF
                    DeleteMsgPort(reply2)
                ENDIF
                DeleteMsgPort(reply1)
            ENDIF
            FreeVec(buffer2)
        ENDIF
        FreeVec(buffer1)
    ENDIF
    as_Close(file)
ENDIF

ENDPROC


/* from amiga.lib */
PROC beginio(arequest)
    MOVE.L  arequest,A1
    MOVE.L  A6,-(A7)
    MOVE.L  $14(A1),A6
    JSR -$1E(A6)
    MOVE.L  (A7)+,A6
ENDPROC




