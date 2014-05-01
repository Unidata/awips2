C MODULE FGDEF
C-----------------------------------------------------------------------
C
C ROUTINE TO DEFINE FORECAST GROUPS.
C
      SUBROUTINE FGDEF (ISPECL)
C
C  ISPECL = 0 FOR NORMAL FORECAST GROUP
C         = 1 FOR SPECIAL FORECAST GROUP
C
C  ROUTINE ORIGINALLY WRITTEN BY -- ED JOHNSON -- HRL -- 11/1979
C
      CHARACTER*1 DLIM/' '/
      CHARACTER*8 RTNNAM,OPNOLD,CMDNAM,FGNAM
      PARAMETER (NCOMD=9)
      CHARACTER*10 XCOMD(NCOMD)/
     *   'FGDEF',
     *   'IDENTIFIER','ID','I',
     *   'TITLE','T',
     *   'SEGMENTS','SEGS','S'/
cMGM 1/18/02 Increase MCARDS to 1000
      PARAMETER (MCARDS=1000)
      CHARACTER*80 CARDS(MCARDS),IBUF,STRNG,STRNGT
      LOGICAL REQD
C
      DIMENSION FGIDIN(2),INFGID(2),WORK(20)
      DIMENSION LIST(5,100),NUMC(100),REQD(3)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/fp'
      INCLUDE 'common/fd'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fcsegp'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fctime'
      COMMON /FGINFO/ MAXFG,LUFGL
C
      EQUIVALENCE (IBLANK,BLANK)
      EQUIVALENCE (FGIDIN(1),INFGID(1))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/fgdef.f,v $
     . $',                                                             '
     .$Id: fgdef.f,v 1.5 2002/02/11 20:24:46 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLANK/4h    /
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER FGDEF'
C
      IBUG=IFBUG('FGDF')
C
      RTNNAM='FGDEF'
      IOPNUM=0
      CALL FSTWHR (RTNNAM,IOPNUM,OPNOLD,IOLDOP)
C
      NBUF=72
C
      WRITE (IPR,10)
10    FORMAT ('0',80('*') /
     *   ' ','*',26X,'FORECAST GROUP DEFINITION',27X,'*' /
     *   ' ',80('*'))
C
C  READ ALL CARDS
      CMDNAM='FGDEF'
      CALL CDINPT (CARDS,MCARDS,NCARDS,CMDNAM,IERR)
      IF (IERR.GT.0) GO TO 640
C
      ICARD1=1
      ICARDS=0
C
20    INDERR=0
C
      DO 30 I=1,3
         REQD(I)=.FALSE.
30       CONTINUE
C
C  CHECK CARDS FOR AT LEAST ONE OF ALL SUBCOMMANDS -
C  MUST HAVE 'IDENTIFIER', 'TITLE' AND 'SEGMENTS' SUBCOMMANDS -
C  CAN BE IN ANY ORDER
      ICONT=0
      DO 120 I=ICARD1,NCARDS
         CALL UMEMOV (CARDS(I),IBUF,20)
C     GET FIRST FIELD
         NSCAN=1
         CALL USCAN2 (IBUF,DLIM,NSCAN,STRNG,LSTRNG,IERR)
         IF (STRNG.EQ.' ') GO TO 120
C     CHECK FOR COMMENT CARD
         IF (STRNG(1:1).EQ.'$') GO TO 120
         IF (ICONT.EQ.1) GO TO 110
         DO 40 IDEST=1,NCOMD
            IF (STRNG.EQ.XCOMD(IDEST)) GO TO 60
40          CONTINUE
         WRITE (IPR,50) STRNG(1:LENSTR(STRNG)),IBUF
50    FORMAT ('0**ERROR** INVALID KEYWORD (',A,
     *      ') FOUND ON THE FOLLOWING CARD:' /
     *   ' ',A)
         CALL ERROR
         INDERR=1
         GO TO 120
60       GO TO (70,
     *          80,80,80,
     *          90,90,
     *        100,100,100),IDEST
         GO TO 120
C     FGDEF CARD FOUND
70       GO TO 110
C     IDENTIFIER CARD FOUND
80       REQD(1)=.TRUE.
         GO TO 110
C     TITLE CARD FOUND
90       REQD(2)=.TRUE.
         GO TO 110
C     SEGMENTS CARD FOUND
100      REQD(3)=.TRUE.
         GO TO 110
C     CHECK FOR CONTINUATION INDICATOR
110      ICONT=0
         CALL ULENTH (IBUF,LEN(IBUF),LENGTH)
         IF (IBUF(LENGTH:LENGTH).EQ.'&') ICONT=1
120      CONTINUE
C
C  CHECK IF ALL SUBCOMMANDS FOUND
      DO 140 I=1,3
         IF (REQD(I)) GO TO 140
            IF (I.EQ.1) WRITE (IPR,130) 'IDENTIFIER'
            IF (I.EQ.2) WRITE (IPR,130) 'TITLE'
            IF (I.EQ.3) WRITE (IPR,130) 'SEGMENTS'
130   FORMAT ('0**ERROR** NO ''',A,''' CARD FOUND.')
            CALL ERROR
            INDERR=1
140      CONTINUE
C
C  DECODE EACH CARD
      DO 150 I=1,3
         REQD(I)=.FALSE.
150      CONTINUE
C
160   ICARDS=ICARDS+1
      IF (ICARDS.GT.NCARDS) GO TO 640
      CALL UMEMOV (CARDS(ICARDS),IBUF,20)
C
C  GET FIRST FIELD
      NSCAN=1
      CALL USCAN2 (IBUF,DLIM,NSCAN,STRNG,LSTRNG,IERR)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'STRNG=',STRNG
      IF (STRNG.EQ.' ') GO TO 160
C
C  CHECK FOR COMMENT CARD
      IF (STRNG(1:1).EQ.'$') GO TO 160
C
      DO 170 IDEST=1,NCOMD
         IF (STRNG.EQ.XCOMD(IDEST)) GO TO 180
170      CONTINUE
      GO TO 190
C
180   GO TO (200,
     *       210,210,210,
     *       260,260,360,
     *       360,360),IDEST
190   GO TO 160
C
C  FGDEF CARD
200   GO TO 160
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  IDENTIFIER CARD FOUND
C
210   REQD(1)=.TRUE.
C
C  GET IDENTIFIER
C
      NSCAN=2
      CALL USCAN2 (IBUF,DLIM,NSCAN,STRNG,LSTRNG,IERR)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'STRNG=',STRNG
      IF (STRNG.EQ.' ') THEN
         WRITE (IPR,220)
220   FORMAT ('0**ERROR** NO IDENTIFIER FOUND ON ''IDENTIFIER'' CARD.')
         CALL ERROR
         GO TO 160
         ENDIF
C
      NWORDS=2
      CALL UMEMOV (STRNG,INFGID,NWORDS)
C
C  CHECK IF VALID IDENTIFIER
      LFGID=8
      IPACKD=1
      IDTYPE=3
      IPRINT=1
      CALL FCIDCK (FGIDIN,LFGID,IPACKD,IDTYPE,IPRINT,IERR)
      IF (IERR.NE.0) GO TO 630
C
      ILAST=0
C
C  CHECK IF ANY FORECAST GROUPS DEFINED
      IF (NFGREC.GT.0) THEN
         DO 250 I=1,NFGREC
            CALL UREADT (KFFGST,I,FGID,IERR)
            IF (FGID(1).NE.FGIDIN(1).OR.FGID(2).NE.FGIDIN(2)) GO TO 240
               WRITE (IPR,230) FGID
230   FORMAT ('0**ERROR** FORECAST GROUP ',2A4,' ALREADY EXISTS.')
               CALL ERROR
               GO TO 630
240         ILAST=MAX0(ILAST,IREC+NSEG-1)
250         CONTINUE
         ENDIF
C
      IF (REQD(1).AND.REQD(2).AND.REQD(3)) GO TO 410
      GO TO 160
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  TITLE CARD
C
260   REQD(2)=.TRUE.
C
C  GET TITLE
C
      CALL UINDEX (IBUF,NBUF,'''',1,LFOUND)
      IF (LFOUND.NE.0) THEN
         STRNGT=' '
         NSCAN=1
         CALL USCAN2 (IBUF(LFOUND:NBUF),'''',NSCAN,STRNGT,LSTRNGT,IERR)
         IF (STRNGT.EQ.' ') THEN
            IBEGIN=0
            DO 270 IY=1,NBUF
               IF (IBUF(IY:IY).EQ.' ') THEN
                  IBEGIN=IY
                  GO TO 280
                  ENDIF
270         CONTINUE
280         STRNGT=' '
            NSCAN=1
            CALL USCAN2 (IBUF(IBEGIN:NBUF),'''',NSCAN,STRNGT,LSTRNGT,
     *         IERR)
            GO TO 320
            ENDIF
         ENDIF
      IF (LFOUND.EQ.0) THEN
         STRNGT=' '
         LSTRNGT=0
         DO 310 IZ=1,NBUF-1
            IF (IBUF(IZ:IZ).EQ.' '.AND.IBUF(IZ+1:IZ+1).NE.' ') THEN
               IEND=NBUF
               DO 290 IX=NBUF,IZ+1,-1
                  IF (IBUF(IX:IX).NE.' ') THEN
                     IEND=IX
                     GO TO 300
                     ENDIF
290            CONTINUE
300            STRNGT=IBUF(IZ+1:IEND)
               IF (STRNGT.NE.STRNG) LSTRNGT=LENSTR(STRNG)
               GO TO 320
               ENDIF
310      CONTINUE
         ELSE
            NSCAN=1
            CALL USCAN2 (IBUF(LFOUND:NBUF),'''',NSCAN,STRNGT,LSTRNGT,
     *         IERR)
         ENDIF
C
320   IF (IBUG.GE.1) WRITE (IODBUG,*) 'STRNGT=',STRNGT
C
      IF (STRNGT.EQ.' ') THEN
         WRITE (IPR,330)
330   FORMAT ('0**WARNING** NO DESCRIPTION FOUND ON ''TITLE'' CARD.')
         CALL WARN
         DO 340 I=1,5
            DESCR(I)=BLANK
340         CONTINUE
         GO TO 350
         ENDIF
C
      NWORDS=5
      CALL UMEMOV (STRNGT,DESCR,NWORDS)
C
350   IF (REQD(1).AND.REQD(2).AND.REQD(3)) GO TO 410
      GO TO 160
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SEGMENTS CARD
C
360   REQD(3)=.TRUE.
      NSEGS=0
      NWORDS=2
      CALL RDLIST (ICARDS,NCARDS,CARDS,LIST,NSEGS,NWORDS,NUMC,IERR)
      IF (IERR.GT.0) GO TO 630
      IF (NSEGS.EQ.0) THEN
         WRITE (IPR,370)
370   FORMAT ('0**ERROR** NO SEGMENT NAMES FOUND ON ''SEGMENTS'' CARD.')
         CALL ERROR
         GO TO 630
         ENDIF
      IF (NSEGS.GT.1) THEN
         DO 400 I=1,NSEGS
            DO 390 J=1,NSEGS
               IF (J.EQ.I) GO TO 390
               IF (LIST(1,I).EQ.LIST(1,J).AND.
     *             LIST(2,I).EQ.LIST(2,J)) THEN
               WRITE (IPR,380) LIST(1,I),LIST(2,I),I,J
380   FORMAT ('0**ERROR** SEGMENT IDENTIFIER ',2A4,
     *   ' FOUND AT POSITIONS ',I3,' AND ',I3,' OF LIST.')
                   CALL ERROR
                   INDERR=1
                   LIST(1,I)=IBLANK
                   LIST(2,I)=IBLANK
                   ENDIF
390            CONTINUE
400         CONTINUE
         ENDIF
      IF (REQD(1).AND.REQD(2).AND.REQD(3)) GO TO 410
      GO TO 160
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
410   IF (INDERR.EQ.1) GO TO 630
C
      IREC=ILAST+1
      ISPEC=0
      IF (ISPECL.EQ.1) ISPEC=1
      MINDTF=1
C
      DO 500 I=1,NSEGS
         CALL FLOCSG (LIST(1,I),IRSEG)
         IF (IRSEG.EQ.0) THEN
            WRITE (IPR,420) LIST(1,I),LIST(2,I)
420   FORMAT ('0**ERROR** SEGMENT ',2A4,' DOES NOT EXIST.')
            CALL ERROR
            IF (ISPEC.EQ.0) THEN
               INDERR=1
               GO TO 500
               ENDIF
            GO TO 630
            ENDIF
         CALL FGETSG (IDSEGN,IRSEG,1,X,1,X,1,X,1,1,IERR)
         IF (IERR.NE.0) THEN
            WRITE (IPR,430) LIST(1,I),LIST(2,I)
430   FORMAT ('0**ERROR** IN FGDEF - ERROR READING SEGMENT ',2A4,'.')
            IF (ISPEC.EQ.0) THEN
               INDERR=1
               GO TO 500
               ENDIF
            GO TO 630
            ENDIF
C     CHECK IF SEGMENT IS COMPLETE AND ALL ASSOCIATED TIME SERIES EXIST
         ISEG(1)=IDSEGN(1)
         ISEG(2)=IDSEGN(2)
         IF (IDEFSG.EQ.0) GO TO 450
         IF (IDEFSG.LT.2) THEN
            INDERR=1
            GO TO 500
            ENDIF
C     TIME SERIES WERE NOT DEFINED WHEN SEGMENT DEFINITION TIME -
C     CHECK IF EXIST NOW - MUST FILL TS ARRAY FIRST
         CALL FGETSG (IDSEGN,IRSEG,MP,P,MT,T,MTS,TS,1,0,IER)
         CALL FCKTSX (TS,MTS,MSNG,MD,LDMAX,LXCEED)
         IF (MSNG.GT.0) THEN
            WRITE (IPR,440) IDSEGN
440   FORMAT ('0**ERROR** ALL TIME SERIES FOR SEGMENT ',2A4,
     * ' HAVE NOT BEEN DEFINED.')
            CALL ERROR
            INDERR=1
            GO TO 500
            ENDIF
         IF (LXCEED.GT.0) THEN
            INDERR=1
            GO TO 500
            ENDIF
         IF (LDMAX.LE.ND) GO TO 450
C        UPDATE ND ON SEGSTS FILE
            ND=LDMAX
450      IDEFSG=0
         CALL FCLCD (MINDTF,MINDT)
         J=IREC+I-1
C     CHECK IF SPACE FOR THIS SEGMENT NAME ON FILE FCFGLIST
         IF (J.GT.MRSTS) THEN
            WRITE (IPR,460) IDSEGN
460   FORMAT ('0**ERROR** FILE FCFGLIST IS FULL. ',
     *   'CANNOT STORE SEGMENT ',2A4,'.')
            CALL ERROR
            INDERR=1
            GO TO 500
            ENDIF
         CALL UWRITT (KFFGL,J,IDSEGN,IERR)
         IF (ISPEC.EQ.1) GO TO 500
         IF (IFGID(1).EQ.IBLANK.AND.IFGID(2).EQ.IBLANK) GO TO 480
            WRITE (IPR,470) IDSEGN,IFGID
470   FORMAT ('0**ERROR** SEGMENT ',2A4,' BELONGS TO FORECAST GROUP ',
     *   2A4,'.')
            CALL ERROR
            INDERR=1
            GO TO 500
480      IFGID(1)=INFGID(1)
         IFGID(2)=INFGID(2)
         CALL FPUTSG (X,X,X,0,1,0,1,0,IERR)
         IF (IERR.NE.0) THEN
            WRITE (IPR,490) IDSEGN
490   FORMAT ('0**ERROR** IN FGDEF - ERROR WRITING SEGMENT ',
     *  2A4,' TO FILE FCSEGSTS.')
            CALL ERROR
            INDERR=1
            GO TO 500
            ENDIF
500      CONTINUE
C
      IF (INDERR.EQ.1) GO TO 590
C
      NSEG=NSEGS
C
      DO 510 I=1,2
         FGID(I)=FGIDIN(I)
         CGIDF(I)=BLANK
510      CONTINUE
      DO 520 I=1,5
         ICRDTF(I)=NOW(I)
520      CONTINUE
C
C  CHECK FILE FCFGSTAT FOR AN OBSOLETE DEFINITION
      DO 530 I=1,NFGREC
         CALL UREADT (KFFGST,I,WORK,IERR)
         CALL UMEMOV (WORK(1),FGNAM,2)
         IF (FGNAM.EQ.'OBSOLETE') THEN
            IFGREC=I
            GO TO 560
            ENDIF
530       CONTINUE
C
C  ADD TO END OF FILE FCFGSTAT
      IF (MAXFG.GT.NFGREC) GO TO 550
         WRITE (IPR,540)FGID
540   FORMAT ('0**ERROR** NOT ENOUGH ROOM ON FILE FCFGSTAT ',
     1 'TO DEFINE FG ',2A4)
         CALL ERROR
         GO TO 590
C
550   NFGREC=NFGREC+1
      IFGREC=NFGREC
C
560   IDUMYG=0
      IF (IFGREC.EQ.1) IDUMYG=NFGREC
      IF (IFGREC.EQ.2) IDUMYG=MAXFG
      CALL UWRITT (KFFGST,IFGREC,FGID,IERR)
C
C  CHECK IF HAVE WRITTEN RECORD
      IF (IFGREC.EQ.1) GO TO 570
         CALL UREADT (KFFGST,1,FGID,IERR)
         IDUMYG=NFGREC
         CALL UWRITT (KFFGST,1,FGID,IERR)
C     READ RECORD IFGREC INTO COMMON BLOCK FCFGS
         CALL UREADT (KFFGST,IFGREC,FGID,IERR)
C
C  UPDATE LAST USED RECORD IN FCFGLIST FILE
570   LUFGL=IREC+NSEG-1
      WRITE (IPR,580) FGIDIN,IFGREC
580   FORMAT ('0**NOTE** FORECAST GROUP ' ,2A4,
     *   ' ADDED TO FILES FCFGLIST AT RECORD ',I5,'.')
      CALL FCDMP3
      CALL FGSGLS(IREC,NSEG)
      GO TO 630
C
C  ERROR ENCOUNTERED - MUST SET FORECAST GROUP IDENTIFIER TO BLANKS
C  FOR ALL SEGMENTS THAT WERE ADDED TO NON-SPECIAL FORECAST GROUPS
C
590   IF (NRSTS.EQ.0) GO TO 630
C
      DO 620 IRSEG=1,NRSTS
         CALL FGETSG (IDSEGN,IRSEG,1,X,1,X,1,X,1,1,IER)
         IF (IER.EQ.0) GO TO 610
            WRITE (IPR,600) IRSEG
600   FORMAT ('0**ERROR** IN FGDEF - ERROR WITH RECORD ',I6,
     *  ' ON FILE FCSEGSTS.')
            CALL ERROR
            GO TO 630
610      IF (IFGID(1).NE.INFGID(1).OR.IFGID(2).NE.INFGID(2)) GO TO 620
         IFGID(1)=IBLANK
         IFGID(2)=IBLANK
         CALL FPUTSG (X,X,X,0,1,0,1,0,IER)
         IF (IER.EQ.0) GO TO 620
            WRITE (IPR,600) IRSEG
            CALL ERROR
            GO TO 630
620      CONTINUE
C
630   ICARD1=ICARDS+1
      IF (ICARDS.LT.NCARDS) GO TO 20
C
640   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IPR,*) 'EXIT FGDEF'
C
      RETURN
C
      END
