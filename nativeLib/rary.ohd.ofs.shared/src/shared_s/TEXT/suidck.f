C MODULE SUIDCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR A VALID KEYWORD.
C
      SUBROUTINE SUIDCK (TYPE,XNAME,NFLD,IPRINT,IFOUND,ISTAT)
C
      CHARACTER*4 TYPE,ZNAME
      CHARACTER*4 TCMDS/'CMDS'/,TID/'ID  '/,TDEFN/'DEFN'/,TSTA/'STA '/
      CHARACTER*4 TDUMP/'DUMP'/,TDMPG/'DMPG'/,TDELT/'DELT'/
      CHARACTER*4 TCHNG/'CHNG'/
      CHARACTER*4 TSTAT/'STAT'/
      CHARACTER*8 XNAME
      CHARACTER*4 SCHAR(10)
      CHARACTER*4 NMBRS(10)
      CHARACTER*4 SHEFS(4)
      CHARACTER*8 DSWORK(1)
      CHARACTER*8 RESVD(4)
      CHARACTER*8 KEYWD(8)
      CHARACTER*8 DEFNK(16)
      CHARACTER*8 DUMPK(4),DUMPG(44)
      CHARACTER*8 DELTG(12),DELTK(4)
      CHARACTER*8 CHNGK(4)
      CHARACTER*8 STATK(20)
C
      CHARACTER*8 RNAME
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sucmdx'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
      DATA NSCHAR/10/
      DATA SCHAR/' ','(',')','*','$','=','@','&','''','-'/
C
      DATA NNMBRS/10/
      DATA NMBRS/'0','1','2','3','4','5','6','7','8','9'/
C
      DATA NSHEFS/4/
      DATA SHEFS/'W','X','Y','Z'/
C
      DATA MRESVD/4/
      DATA RESVD/'END     ','        ','        ','       '/
C
      DATA MKEYWD/8/
      DATA KEYWD/'AREA    ','BASIN   ','STA     ','STATION ',
     *           '        ','ELEV    ','GBOX    ','RFRO    '/
C
      DATA MDEFNK/16/
      DATA DEFNK/'BASN    ','STA     ','UGNL    ','URRS    ',
     *           'STBN    ','STAN    ','OLD     ','NEW     ',
     *           'PRNTPARM','PRNTNOTE','PLOT    ','GBOX    ',
     *           'RFRO    ','        ','        ','        '/
C
      DATA MDUMPG/44/
      DATA DUMPG/'AREA    ','BASIN   ','NAMES   ','ORDER   ',
     *           'STATION ','STA     ','PUNCH   ','PRINT   ',
     *           'BOTH    ','UNITS   ','SUMMARY ','PRNTEST ',
     *           'LEVEL   ','NETWORK ','ARRAY   ','PPDPTRS ',
     *           'SASMSTA ','GOESSTA ','FCID    ','PARMS   ',
     *           'DEGMIN  ','DFLT    ','PPDINDX ','PPPINDX ',
     *           'PDBINDX ','PPPTYPE ','SPACEING','STATS   ',
     *           'CHAR    ','MMMT    ','GBOX    ','RFRO    ',
     *           'PPFNDR  ','OUTPUT  ','PPDCNTL ','FILECRAT',
     *           'DSATTR  ','TSHDR   ','PPPPTRS ','MDRGRID ',
     *           'TYPE    ','        ','        ','        '/
      DATA MDUMPK/4/
      DATA DUMPK/'SORT    ','BASN    ','        ','        '/
C
      DATA MDELTG/12/
      DATA DELTG/'AREA    ','BASIN   ','STATION ','STA     ',
     *           'PPD     ','PPP     ','PRD     ','SASM    ',
     *           'GOES    ','GBOX    ','RFRO    ','        '/
      DATA MDELTK/4/
      DATA DELTK/'ALL     ','CHECKREF','RUNNTWK ','        '/
C
      DATA MCHNGK/4/
      DATA CHNGK/'ID      ','        ','        ','        '/
C
      DATA MSTATK/20/
      DATA STATK/'LEVEL   ','ALL     ','SASM    ','GOES    ',
     *           '        ','PPDB    ','PPPDB   ','PDB     ',
     *           'NEWPAGE ','DEGMIN  ','PRNTNTWK','PRNTORDR',
     *           'FILECHK ','PPD     ','PPP     ','PRD     ',
     *           'UPRM    ','STATION ','STA     ','        '/
C
      EQUIVALENCE (DSWORK(1),SWORK(2))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/suidck.f,v $
     . $',                                                             '
     .$Id: suidck.f,v 1.4 2001/06/13 14:00:32 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      CALL SBLTRC ('SYS ','        ','SUIDCK  ',LTRACE)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,270) TYPE,XNAME,NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      CALL SBLDBG ('SYS ','        ','SUIDCK  ',LDEBUG)
C
      ISTAT=0
C
      IFOUND=0
      IPCMD=0
      ID=0
C
      ZNAME=' '
      CALL ULENTH (XNAME,LEN(XNAME),LENTH)
      IF (LENTH.LE.4) CALL SUBSTR (XNAME,1,4,ZNAME,1)
C
C  CHECK IF NAME TO BE CHECKED IS BLANK
      IF (XNAME.EQ.' ') THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (LP,280)
            CALL SULINE (LP,2)
            ENDIF
         GO TO 260
         ENDIF
C
C  CHECK IF TYPE IS ID OR STA
      IF (TYPE.EQ.TID.OR.TYPE.EQ.TSTA) THEN
         ID=1
         GO TO 20
         ENDIF
C
      IF (TYPE.EQ.TCMDS) GO TO 20
      IF (TYPE.EQ.TDEFN) GO TO 90
      IF (TYPE.EQ.TDELT) GO TO 90
      IF (TYPE.EQ.TDUMP) GO TO 130
      IF (TYPE.EQ.TDMPG) GO TO 150
      IF (TYPE.EQ.TCHNG) GO TO 200
      IF (TYPE.EQ.TSTAT) GO TO 215
10       WRITE (LP,290) TYPE
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    IF (IPRINT.EQ.0) GO TO 40
C
C  PRINT COMMANDS
      WRITE (LP,300)
      CALL SULINE (LP,2)
      ISPTR=0
      CALL SUSORT (2,NPCMDS,PCMDS,DSWORK,ISPTR,IERR)
      DO 30 I=1,NPCMDS
         IF (DSWORK(I).EQ.' ') GO TO 30
         WRITE (LP,310) DSWORK(I)
         CALL SULINE (LP,1)
30       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR COMMAND
C
40    CALL SUBSTR (XNAME,1,LEN(XNAME),RNAME,-1)
      DO 50 I=1,NPCMDS
         IF (RNAME.NE.PCMDS(I)) GO TO 50
            IPCMD=I
            IFOUND=IPCMD
            ISTAT=2
            GO TO 260
50       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ID.EQ.0) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR RESERVED WORD
C
      DO 60 I=1,MRESVD
         IF (XNAME.NE.RESVD(I)) GO TO 60
            ISTAT=2
            GO TO 260
60       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR SPECIAL CHARACTERS
C
      IBEG=IFSTRT(NFLD)
      IF (ICDBUF(IFSTRT(NFLD):IFSTRT(NFLD)).EQ.'''') IBEG=IBEG+1
      IEND=IFSTOP(NFLD)
      IF (ICDBUF(IFSTOP(NFLD):IFSTOP(NFLD)).EQ.'''') IEND=IEND-1
      DO 80 I=IBEG,IEND
         DO 70 J=1,NSCHAR
            IF (ICDBUF(I:I).EQ.SCHAR(J)) THEN
               ISTAT=2
               GO TO 260
               ENDIF
70          CONTINUE
80       CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR GROUP KEYWORD
C
90    DO 100 I=1,MKEYWD
         IF (XNAME.NE.KEYWD(I)) GO TO 100
            IFOUND=1
            ISTAT=2
            GO TO 260
100      CONTINUE
C
      IF (ZNAME.NE.' ') CALL SUIDC2 (ZNAME,IFOUND,ISTAT)
      IF (IFOUND.EQ.1) GO TO 260
C
      IF (ID.EQ.1) GO TO 110
C
      IF (TYPE.EQ.TDEFN) GO TO 110
      IF (TYPE.EQ.TDELT) GO TO 170
C
      GO TO 10
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR DEFINE KEYWORD
C
110   DO 120 I=1,MDEFNK
         IF (XNAME.NE.DEFNK(I)) GO TO 120
            IFOUND=1
            ISTAT=2
            GO TO 260
120      CONTINUE
C
      IF (ZNAME.NE.' ') CALL SUIDC2 (ZNAME,IFOUND,ISTAT)
      IF (IFOUND.EQ.1) GO TO 260
C
      IF (ID.EQ.0) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR DUMP KEYWORD
C
130   DO 140 I=1,MDUMPK
         IF (XNAME.NE.DUMPK(I)) GO TO 140
            IFOUND=1
            ISTAT=2
            GO TO 260
140      CONTINUE
C
      IF (ZNAME.NE.' ') CALL SUIDC2 (ZNAME,IFOUND,ISTAT)
      IF (IFOUND.EQ.1) GO TO 260
C
C  CHECK FOR DUMP GROUP
C
150   DO 160 I=1,MDUMPG
         IF (XNAME.NE.DUMPG(I)) GO TO 160
            IFOUND=1
            ISTAT=2
            GO TO 260
160      CONTINUE
C
      IF (TYPE.EQ.TDUMP) GO TO 260
      IF (TYPE.EQ.TDMPG) GO TO 260
C
      IF (ID.EQ.0) GO TO 260
      IF (TYPE.NE.TSTA) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR DELETE KEYWORD
C
170   DO 180 I=1,MDELTK
         IF (XNAME.NE.DELTK(I)) GO TO 180
            IFOUND=1
            ISTAT=2
            GO TO 260
180      CONTINUE
C
      IF (ZNAME.NE.' ') CALL SUIDC2 (ZNAME,IFOUND,ISTAT)
      IF (IFOUND.EQ.1) GO TO 260
C
C  CHECK FOR DELETE GROUP
C
      DO 190 I=1,MDELTG
         IF (XNAME.NE.DELTG(I)) GO TO 190
            IFOUND=1
            ISTAT=2
            GO TO 260
190      CONTINUE
C
      IF (ZNAME.NE.' ') CALL SUIDC2 (ZNAME,IFOUND,ISTAT)
      IF (IFOUND.EQ.1) GO TO 260
C
      IF (TYPE.EQ.TDELT) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR CHANGE KEYWORD
C
200   DO 210 I=1,MCHNGK
         IF (XNAME.NE.CHNGK(I)) GO TO 210
            IFOUND=1
            ISTAT=2
            GO TO 260
210      CONTINUE
C
      IF (TYPE.EQ.TCHNG) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR STATUS KEYWORD
C
215   DO 217 I=1,MSTATK
         IF (XNAME.NE.STATK(I)) GO TO 217
            IFOUND=1
            ISTAT=2
            GO TO 260
217      CONTINUE
C
      IF (TYPE.EQ.TSTAT) GO TO 260
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (ID.EQ.0) GO TO 260
      IF (TYPE.NE.TSTA) GO TO 260
C
C  CHECK IF STATION IDENTIFIER IS SHEF STRANGER STATION FORMAT
C  (LETTER W, X, Y OR Z FOLLOWED BY ALL NUMBERS)
      DO 220 I=1,NSHEFS
         CALL USCHFL (SHEFS(I),NFLD,ICOL)
         IF (ICOL.EQ.1) GO TO 230
220      CONTINUE
      GO TO 260
C
230   IBEG=IFSTRT(NFLD)+1
      IEND=IFSTOP(NFLD)
      DO 250 I=IBEG,IEND
         DO 240 J=1,NNMBRS
            IF (ICDBUF(I:I).NE.NMBRS(J)) GO TO 260
240         CONTINUE
250      CONTINUE
      ISTAT=3
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
260   IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,320) IFOUND,IPCMD,ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
270   FORMAT (' *** ENTER SUIDCK : ',
     *   'TYPE=',A,3X,'XNAME=',A,3X,'NFLD=',I2)
280   FORMAT (' NAME TO BE CHECKED IS BLANK')
290   FORMAT ('0*** ERROR - IN SUIDCK - INVALID TYPE : ',A)
300   FORMAT ('0',T14,'- AVAILABLE COMMANDS ARE -')
310   FORMAT (T23,A)
320   FORMAT (' *** EXIT SUIDCK : ',
     *   'IFOUND=',I2,3X,
     *   'IPCMD=',I2,3X,
     *   'ISTAT=',I2)
C
      END
