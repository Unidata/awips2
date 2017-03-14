C MODULE SFAREA
C-----------------------------------------------------------------------
C
C  ROUTINE TO DEFINE GENERAL AREA PARAMETERS.
C
      SUBROUTINE SFAREA (LARRAY,ARRAY,DISP,PRPARM,PRNOTE,PRBASN,
     *   PLOT,NFLD,IRUNCK,ISTAT)
C
      CHARACTER*4 DISP,PRPARM,PRNOTE,PRBASN,PLOT
      CHARACTER*4 TYPE,XPARM,XNOTE,XPLOT
      CHARACTER*20 STRNG,STRNG2
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/stypax'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfarea.f,v $
     . $',                                                             '
     .$Id: sfarea.f,v 1.3 2001/06/13 13:59:53 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SFAREA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('DEFN')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*)
     *      ' LARRAY=',LARRAY,
     *      ' DISP',DISP,
     *      ' PRPARM',PRPARM,
     *      ' PRBASN',PRBASN,
     *      ' PLOT',PLOT,
     *      ' NFLD',NFLD,
     *      ' '
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
      LSTRNG=LEN(STRNG)/4
      LSTRNG2=LEN(STRNG2)/4
C
      NUMERR=0
      NUMWRN=0
      ISTRT=-1
      ILPFND=0
      IRPFND=0
      TYPE=' '
      ICMERR=0
      NUMKEY=0
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FIELDS FOR DEFINE AREA OPTIONS
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (NFLD.EQ.-1) GO TO 70
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,100) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK IF ERROR ON PREVIOUS COMMAND
      IF (ICMERR.EQ.1) THEN
         IF (LATSGN.EQ.0) THEN
            IF (NFLD.EQ.1) CALL SUPCRD
            ENDIF
         IF (LATSGN.EQ.1) THEN
            ICMERR=0
            NSCARD=NRDCRD-NCSKIP
            IF (NSCARD.GT.0) THEN
               WRITE (LP,110) NSCARD
               CALL SULINE (LP,2)
               ENDIF
            ENDIF
         ENDIF
C
C  CHECK FOR COMMAND
      IF (LATSGN.EQ.1) GO TO 70
C
C  CHECK FOR PAIRED PARENTHESIS
      IF (ILPFND.GT.0.AND.IRPFND.EQ.0) THEN
         WRITE (LP,130) NFLD
         CALL SULINE (LP,2)
         ILPFND=0
         IRPFND=0
         ENDIF
      IF (LLPAR.GT.0) ILPFND=1
      IF (LRPAR.GT.0) IRPFND=1
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
C  CHECK FOR KEYWORD
      CALL SUIDCK ('DEFN',STRNG2,NFLD,0,IKEYWD,IERR)
      IF (IERR.NE.0) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,*) 'STRNG2=',STRNG2
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
C  CHECK PARAMETER TYPE
      DO 20 I=1,MATYPE
         IF (STRNG2.EQ.ATYPE(I)) GO TO 30
20       CONTINUE
      WRITE (LP,120) STRNG2
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
30    TYPE=STRNG2
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'TYPE=',TYPE
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      NUMKEY=NUMKEY+1
C
      XPARM=PRPARM
      IF (XPARM.EQ.' ') XPARM='YES'
      XNOTE=PRNOTE
      IF (XNOTE.EQ.' ') XNOTE='YES'
      XPLOT=PLOT
      IF (XPLOT.EQ.' ') XPLOT='YES'
      WRITE (LP,160) DISP,XPLOT,XNOTE,XPARM
      CALL SULINE (LP,2)
C
C  MAP AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(1)) THEN
         IF (DISP.EQ.'OLD'.AND.
     *      (ISUPRT('OLD ').EQ.0.OR.ISUPRT('OLDX').EQ.0)) GO TO 40
         CALL SFMAP (LARRAY,ARRAY,DISP,PRPARM,PRNOTE,PLOT,NFLD,
     *      IRUNCK,IERR)
         GO TO 60
         ENDIF
C
C  FMAP AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(2)) THEN
         CALL SFFMAP (LARRAY,ARRAY,DISP,PRPARM,NFLD,IRUNCK,IERR)
         GO TO 60
         ENDIF
C
C  MAT AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(3)) THEN
         CALL SFMAT (LARRAY,ARRAY,DISP,PRPARM,PLOT,NFLD,IRUNCK,IERR)
         GO TO 60
         ENDIF
C
C  MAPE AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(4)) THEN
         CALL SFMAPE (LARRAY,ARRAY,DISP,PRPARM,PRNOTE,PLOT,NFLD,
     *      IRUNCK,IERR)
         GO TO 60
         ENDIF
C
C  MARO AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(5)) THEN
         IF (DISP.EQ.'OLD'.AND.
     *      (ISUPRT('OLD ').EQ.0.OR.ISUPRT('OLDX').EQ.0)) GO TO 40
         CALL SFMARO (LARRAY,ARRAY,DISP,PRPARM,PRNOTE,PLOT,NFLD,
     *      IRUNCK,IERR)
         GO TO 60
         ENDIF
C
C  MAPX AREA PARAMETERS
      IF (STRNG2.EQ.ATYPE(6)) THEN
         IF (DISP.EQ.'OLD'.AND.
     *      (ISUPRT('OLD ').EQ.0.OR.ISUPRT('OLDX').EQ.0)) GO TO 40
         CALL SFMAPX (LARRAY,ARRAY,DISP,PRPARM,NFLD,IRUNCK,IERR)
         GO TO 60
         ENDIF
C
      GO TO 50
C
C  NON-SUPPORTED FEATURE SPECIFIED - SKIP TO NEXT COMMAND
40    IF (NFLD.EQ.1) CALL SUPCRD
      WRITE (LP,90)
      CALL SULINE (LP,1)
      WRITE (LP,90)
      CALL SULINE (LP,1)
      WRITE (LP,140) DISP,TYPE
      IF (IOPOVP.EQ.1) THEN
         WRITE (LP,140) DISP,TYPE
         WRITE (LP,140) DISP,TYPE
         ENDIF
      CALL SUERRS (LP,1,NUMERR)
      ICMERR=1
      NCSKIP=NRDCRD
      GO TO 10
C
C  INVALID PARAMETER TYPE
50    WRITE (LP,150) TYPE
      CALL SUERRS (LP,2,NUMERR)
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR END OF INPUT
60    IF (NFLD.EQ.-1) GO TO 70
C
C  CHECK CURRENT FIELD FOR KEYWORD
      ISTRT=-1
      GO TO 10
C
C  CHECK NUMBER OF KEYWORDS FOUND
70    IF (NUMKEY.EQ.0) THEN
         WRITE (LP,170)
         CALL SUWRNS (LP,2,NUMWRN)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SFAREA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT (' ')
100   FORMAT (' NULL FIELD FOUND IN FIELD ',I2)
110   FORMAT ('0*** NOTE - ',I4,' CARD IMAGES NOT PROCESSED BECAUSE ',
     *   'INVALID OPTION FOUND.')
120   FORMAT ('0*** ERROR - INVALID AREA PARAMETER TYPE : ',A)
130   FORMAT ('0*** NOTE - RIGHT PARENTHESES ASSUMED IN FIELD ',
     *   I2,'.')
140   FORMAT ('+*** ERROR - USE OF THE DISPOSITION OF ',A,' ',
     *   'IS NOT CURRENTLY ALLOWED FOR DEFINE AREA ',A,'. ',
     *   'COMMAND WILL NOT BE PROCESSED.')
150   FORMAT ('0*** ERROR - IN SFAREA - PARAMETER TYPE ',A,' CANNOT ',
     *   'BE PROCESSED.')
160   FORMAT ('0DEFINE OPTIONS IN EFFECT : ',
     *   'DISP=',A,3X,
     *   'PLOT=',A,3X,
     *   'PRNTNOTE=',A,3X,
     *   'PRNTPARM=',A,3X)
170   FORMAT ('0*** WARNING - NO PARAMETER TYPE KEYWORD ',
     *   '(FMAP, MAP, MAPE, MAPX OR MAT) WAS FOUND.')
C
      END
