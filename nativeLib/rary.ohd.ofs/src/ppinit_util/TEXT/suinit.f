C MODULE SUINIT
C-----------------------------------------------------------------------
C
C  ROUTINE TO INITIALIZE DATA BASES.
C
      SUBROUTINE SUINIT (NFLD,ISTAT)
C
      PARAMETER (MOPTN=9)
      CHARACTER*8 OPTN(MOPTN)
      DATA OPTN/'ALL     ','SASM    ','GOES    ',
     *          'PPDB    ','PPPDB   ','PDB     ',
     *          'PPD     ','PPP     ','PRD     '/
      CHARACTER*8 BLNK8/' '/
      CHARACTER*20 STRNG,STRNG2
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'pdbcommon/pdbdta'
      INCLUDE 'pdbcommon/pdddfc'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdhshi'
      INCLUDE 'pdbcommon/pdhshc'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pdbcommon/pdsifc'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'prdcommon/pdatas'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/ptsctl'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urpddd'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urrrsc'
      INCLUDE 'urcommon/urhshc'
      INCLUDE 'urcommon/urhshi'
      INCLUDE 'urcommon/urxctl'
      INCLUDE 'urcommon/urppmc'
      INCLUDE 'urcommon/urppdt'
      INCLUDE 'urcommon/urtscl'
      INCLUDE 'urcommon/urftbl'
      INCLUDE 'urcommon/urmaxm'
      INCLUDE 'urcommon/urprd'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/suinit.f,v $
     . $',                                                             '
     .$Id: suinit.f,v 1.2 2001/06/13 14:00:45 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUINIT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('INIT')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NFLD=',NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LSTRNG=LEN(STRNG)/4
      LSTRNG2=LEN(STRNG2)/4
      IALL=0
      NUMOPT=0
      NUMERR=0
      IENDIN=0
      ISTRT=1
C
C  SET DEFAULT OPTIONS
      NEWPAG=0
C
C  CHECK WHICH DATA BASES ALLOCATED
      IDPPD=1
      IDPPP=1
      IDPRD=1
      IDSASM=1
      IDGOES=1
      IDFMM=1
      INDMSG=-2
      CALL SUDALC (0,0,0,IDPPD,IDPPP,IDPRD,0,IDSASM,IDGOES,IDFMM,
     *   INDMSG,IERR)
C
C  OPEN DATA BASES
      CALL SUDOPN (1,'PPD ',IERR)
      CALL SUDOPN (1,'PPP ',IERR)
      CALL SUDOPN (1,'PRD ',IERR)
C
C  SET UNIT NUMBERS
      CALL UMEMOV (KPDSIF,KURSIF,7)
      CALL UMEMOV (KPPIDX,KURIDX,7)
      CALL UMEMOV (KMAPTS,KUMAPT,8)
C
C  COPY COMMON BLOCKS
      NWORDS=MXHSHC+1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IPDHSC,IURHSC,NWORDS)
      NWORDS=MXHSHI+1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IPDHSI,IURHSI,NWORDS)
      NWORDS=4*5
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IPDDFC,JPDDFC,NWORDS)
      NWORDS=24*30
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IDDTDR,JDDTDR,NWORDS)
      NWORDS=15
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (MXRRSF,MAXRSF,NWORDS)
      NWORDS=16
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (NWDCTL,NWCTL,NWORDS)
      NWORDS=8*50
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IPDTDR,JPDTDR,NWORDS)
      NWORDS=8*9
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IPMCTL,JPMCTL,NWORDS)
      NWORDS=8
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (MXPXRC,MAXPXR,NWORDS)
      NWORDS=18*50
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (DATFIL,IDATFL,NWORDS)
      NWORDS=60
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (USERPR,INAMRF,NWORDS)
      NWORDS=20
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IBLNK,LBLNK,NWORDS)
      NWORDS=16*5
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (TSCNTR,ITSCNT,NWORDS)
C
C  PRINT CARD
      CALL SUPCRD
C
C  PRINT HEADING
      WRITE (LP,260)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR OPTIONS
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) THEN
          IENDIN=1
          GO TO 220
          ENDIF
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
             WRITE (IOSDBG,310) NFLD
             CALL SULINE (IOSDBG,1)
             ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.NE.0) THEN
         IENDIN=1
         GO TO 220
         ENDIF
C
C  CHECK FOR PARENTHESES
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
      IF (NFLD.EQ.1) CALL SUPCRD
C
C  CHECK FOR OPTION
      DO 50 IOPTN=1,MOPTN
         CALL SUCOMP (2,STRNG2,OPTN(IOPTN),IMATCH)
         IF (IMATCH.EQ.1) GO TO 70
50       CONTINUE
C
C  INVALID OPTION
      WRITE (LP,290) STRNG2
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C  ALL OPTION
60    IALL=1
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    GO TO (60,90,90,
     *       90,90,90,
     *       90,90,90),IOPTN
80    WRITE (LP,300) IOPTN
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    NUMOPT=NUMOPT+1
C
      IXINIT=1
C
      IF (IALL.EQ.-1) GO TO 110
C
      GO TO (100,110,120,
     *       170,180,190,
     *       170,180,190),IOPTN
100   WRITE (LP,410) IOPTN
      CALL SUERRS (LP,2,NUMERR)
      IF (IENDIN.EQ.1) GO TO 230
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SASM CONTROL FILE
C
110   IF (IDSASM.EQ.0) GO TO 120
      IF (NEWPAG.EQ.1) CALL SUPAGE
      IF (NEWPAG.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  WRITE TO PROGRAM LOG
      IF (IALL.EQ.-1) IOPTN=3
      NPAGE=0
      ISTART=1
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
C  ZERO NUMBER OF STATIONS DEFINED
      CALL DSZERO (PUSRID,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,365) 'SASM',IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IERR.EQ.0) THEN
         WRITE (LP,370) PUSRID,'SASM'
         CALL SULINE (LP,2)
         ENDIF
C
C  WRITE TO PROGRAM LOG
      ISTART=0
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      IF (IABS(IALL).EQ.1) GO TO 120
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GOES CONTROL FILE
C
120   IF (IDGOES.EQ.0) GO TO 170
      IF (NEWPAG.EQ.1) CALL SUPAGE
      IF (NEWPAG.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  WRITE TO PROGRAM LOG
      IF (IALL.EQ.-1) IOPTN=4
      NPAGE=0
      ISTART=1
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
C  ZERO NUMBER OF STATIONS DEFINED
      CALL DGZERO (PUSRID,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,365) 'GOES',IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (IERR.EQ.0) THEN
         WRITE (LP,370) PUSRID,'GOES'
         CALL SULINE (LP,2)
         ENDIF
C
C  WRITE TO PROGRAM LOG
      ISTART=0
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      IF (IABS(IALL).EQ.1) GO TO 170
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PREPROCESSOR DATA BASE
C
170   IF (IDPPD.EQ.0) GO TO 180
      IF (NEWPAG.EQ.1) CALL SUPAGE
      IF (NEWPAG.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  WRITE TO PROGRAM LOG
      IF (IALL.EQ.-1) IOPTN=6
      NPAGE=0
      ISTART=1
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      CALL URIPDB (IXINIT)
C
C  WRITE TO PROGRAM LOG
      ISTART=0
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      IF (IABS(IALL).EQ.1) GO TO 180
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PREPROCESSOR PARAMETRIC DATA BASE
C
180   IF (IDPPP.EQ.0) GO TO 190
      IF (NEWPAG.EQ.1) CALL SUPAGE
      IF (NEWPAG.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  WRITE TO PROGRAM LOG
      IF (IALL.EQ.-1) IOPTN=7
      NPAGE=0
      ISTART=1
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      CALL URIPPP (IXINIT)
C
C  WRITE TO PROGRAM LOG
      ISTART=0
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      IF (IABS(IALL).EQ.1) GO TO 190
      GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PROCESSED DATA BASE
C
190   IF (IDPRD.EQ.0) GO TO 200
      IF (NEWPAG.EQ.1) CALL SUPAGE
      IF (NEWPAG.EQ.0) THEN
         WRITE (LP,340)
         CALL SULINE (LP,2)
         ENDIF
      IF (ISLEFT(10).GT.0) CALL SUPAGE
C
C  WRITE TO PROGRAM LOG
      IF (IALL.EQ.-1) IOPTN=8
      NPAGE=0
      ISTART=1
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      CALL URIPRD (IXINIT)
C
C  WRITE TO PROGRAM LOG
      ISTART=0
      CALL SUWLOG ('OPTN',OPTN(IOPTN),BLNK8,NPAGE,ISTART,IERR)
C
      IF (IABS(IALL).EQ.0) GO TO 210
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
200   IF (NEWPAG.EQ.1) GO TO 210
         WRITE (LP,340)
         CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
210   IF (IENDIN.EQ.1) GO TO 230
      IF (NUMOPT.EQ.0.AND.IALL.EQ.1) GO TO 230
      IALL=0
      IF (IOPTN.EQ.0.OR.IOPTN.GT.MOPTN) GO TO 10
      GO TO 10
C
C  CHECK NUMBER OF KEYWORDS FOUND
220   IF (NUMOPT.GT.0) GO TO 230
C
C  NO OPTIONS FOUND
      WRITE (LP,400)
      CALL SULINE (LP,2)
      IALL=-1
      GO TO 90
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  COPY COMMON BLOCKS
230   NWORDS=MXHSHC+1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IURHSC,IPDHSC,NWORDS)
      NWORDS=MXHSHI+1
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IURHSI,IPDHSI,NWORDS)
      NWORDS=4*5
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (JPDDFC,IPDDFC,NWORDS)
      NWORDS=24*30
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (JDDTDR,IDDTDR,NWORDS)
      NWORDS=15
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (MAXRSF,MXRRSF,NWORDS)
      NWORDS=16
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (NWCTL,NWDCTL,NWORDS)
      NWORDS=8*50
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (JPDTDR,IPDTDR,NWORDS)
      NWORDS=8*9
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (JPMCTL,IPMCTL,NWORDS)
      NWORDS=8
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (MAXPXR,MXPXRC,NWORDS)
      NWORDS=18*50
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (IDATFL,DATFIL,NWORDS)
      NWORDS=60
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (INAMRF,USERPR,NWORDS)
      NWORDS=20
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (LBLNK,IBLNK,NWORDS)
      NWORDS=16*5
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NWORDS=',NWORDS
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UMEMOV (ITSCNT,TSCNTR,NWORDS)
C
C  CHECK NUMBER OF ERRORS
      IF (NUMERR.GT.0) THEN
         WRITE (LP,420) NUMERR
         CALL SULINE (LP,2)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUINIT'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
260   FORMAT ('0*--> INITIALIZE DATA BASES')
290   FORMAT ('0*** ERROR - INVALID OPTION : ',A)
300   FORMAT ('0*** ERROR - PROCESSING OPTION NUMBER ',I2,'.')
310   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
340   FORMAT ('0',132('#'))
365   FORMAT (' ZERO ROUTINE CALLED FOR ',A4,3X,'ISTAT=',I2)
370   FORMAT ('0*** NOTE - NUMBER OF STATIONS FOR USER ',2A4,
     *   ' SET TO ZERO IN ',A,' CONTROL FILE.')
400   FORMAT ('0*** NOTE - NO DATA BASE KEYWORD (SASM, GOES,',
     *   ' PPDB, PPPDB OR PDB) WAS FOUND. ''ALL'' IS ASSUMED.')
410   FORMAT ('0*** ERROR - PROCESSING OPTION NUMBER ',I2)
420   FORMAT ('0*** NOTE - ',I3,' ERRORS ENCOUNTERED BY STATUS ',
     *   'COMMAND.')
C
      END
