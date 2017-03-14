C MODULE HCARDI
C-----------------------------------------------------------------------
C
      SUBROUTINE HCARDI (ISTAT)
C
C  THIS ROUTINE CHECKS FOR COMMANDS.
C  THE COMMAND MUST START WITH AN '@'.
C  IF THE FIRST FIELD IS A '$', THE CARD IS ASSUMED TO BE A COMMENT.
C  IF THE COMMAND HAS A KEYWORD, IT IS CHECKED
C
C  ARGUMENT LIST:
C
C       NAME      TYPE  I/O   DIM   DESCRIPTION
C       ------    ----  ---   ---   -----------
C       ISTAT      I     O     1    STATUS CODE:
C                                    0=NORMAL RETURN
C                                    1=COMMAND OR KEYWORD INVALID,
C                                      END-OF-FILE OR READ ERROR
      CHARACTER*8 RTNNAM,OLDOPN
C
      DIMENSION ICMD(2),IKEY(2)
C
C  ICMLST HAS THE COMMAND NAME, COMMAND NUMBER AND KEYWORD INDICATOR:
C    NAME IN WORDS 1 AND 2
C    INDEX NUMBER IN WORD 3
C    KEYWORD INDICATOR IN WORD 4:
C       0=NO KEYWORD
C       1=MUST HAVE KEYWORD
      PARAMETER (NCMLST=61)
      DIMENSION ICMLST(4,NCMLST)
      DATA ICMLST /
     *             4HADDT,4HECH ,01,0, 4HADDT,4H    ,01,0,
     *             4H....,4H....,02,0,
     *             4HCLEA,4HR   ,03,0, 4HCLR ,4H    ,03,0,
     *             4HCOMP,4HUTE ,04,0, 4HCOMP,4H    ,04,0,
     *             4H....,4H....,05,0,
     *             4HCHAN,4HGE  ,06,1, 4HCHAN,4H    ,06,1,
     *             4H....,4H....,07,0, 4H....,4H....,07,0,
     *             4H....,4H....,08,0, 4H....,4H....,08,0,
     *             4HDEFI,4HNEL ,09,1, 4HDEFL,4H    ,09,1,
     *             4HDEFI,4HNEG ,10,1, 4HDEFG,4H    ,10,1,
     *             4HDELE,4HTETS,11,1, 4HDELT,4HS   ,11,1,
     *             4H....,4H....,12,0,
     *             4H....,4H....,13,0,
     *             4HDUMP,4HOPT ,14,0, 4HDUMP,4HO   ,14,0,
     *             4H....,4H....,15,0,
     *             4H....,4H....,16,0,
     *             4HDUMP,4HSYS ,17,1, 4HDUMP,4HS   ,17,1,
     *             4H....,4H....,18,0,
     *             4H....,4H....,19,0,
     *             4HEXEC,4HUTE ,20,0, 4HEXEC,4H    ,20,0,
     *             4H....,4H....,21,1,
     *             4H....,4H....,22,0, 4H....,4H....,22,0,
     *             4HSETO,4HPTIO,23,0, 4HSETO,4HPT  ,23,0,
     *             4H....,4H....,24,0,
     *             4HREPL,4HACE ,25,1, 4HREPL,4H    ,25,1,
     *             4HSETG,4HDFLT,26,1,
     *             4HSETT,4HODAY,27,0, 4HSETT,4HOD  ,27,0,
     *             4HSTOP,4H    ,28,0,
     *             4HDEBU,4HG   ,29,0,
     *             4HNONF,4HCST ,30,0,
     *             4HFCST,4H    ,31,0,
     *             4HSETL,4HDFLT,32,1,
     *             4HDELE,4HTEL ,33,1, 4HDELL,4H    ,33,1,
     *             4HDELE,4HTEG ,34,1, 4HDELG,4H    ,34,1,
     *             4H....,4H....,35,0, 4H....,4H....,35,0,
     *             4HDELL,4HDFLT,36,1,
     *             4HSETU,4HPARM,37,0,
     *             4H....,4H....,38,0,
     *             4HRUN ,4H    ,39,0,
     *             4HNEWU,4HSER ,40,0,
     *             4HPAGE,4HSIZE,41,0,
     *             4HSTAT,4HUS  ,42,0
     *             /
C
      PARAMETER (NKEYWD=18)
      DIMENSION KEYLST(2,NKEYWD)
      DATA KEYLST /4HPROC,1,
     *             4HFUNC,2,
     *             4HTECH,3,
     *             4HOPTI,4, 4HOPT ,4,
     *             4HBEGI,5,
     *             4HNORE,6,
     *             4HRESE,7,
     *             4HEND ,8,
     *             4H....,9, 
     *             4H....,10,
     *             4H....,11,
     *             4HREG ,12,
     *             4H....,13,
     *             4H....,14,
     *             4H....,15,
     *             4H....,16,
     *             4HFUT ,17/
C
      DATA LATSGN/4H@   /
      DATA LEND/4HEND /
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'udatas'
      INCLUDE 'ufreei'
      INCLUDE 'ufstcd'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hcomnd'
      INCLUDE 'hclcommon/hsvcrd'
      INCLUDE 'hclcommon/hprocc'
C
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hcardi.f,v $
     . $',                                                             '
     .$Id: hcardi.f,v 1.4 2003/11/26 18:39:26 scv Exp $
     . $' /
C    ===================================================================
C
C
      RTNNAM='HCARDI'
      IOPNUM=-1
      CALL FSTWHR (RTNNAM,IOPNUM,OLDOPN,IOLDOP)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'ENTER HCARDI'
C
      ISTAT=0
C
      IFCARD=0
      IEPFLG=0
      IHCMND=0
      KEYWRD=0
      IEND=72
      IFSTCD=1
      IBLCRD=0
      NBLCRD=0
C
C  IF ANY SAVED COMMENTS, PUT IN FIRST
      IF (ICSAV.EQ.0) GO TO 20
C
C  READ COMMENTS FROM SAVED FILE (NCARD WAS LAST COMMAND CARD)
      ICARD=NCARD+1
      DO 10 I=1,ICSAV
         CALL UREADT (KHCARD,ICARD,IBUF,IERR)
         CALL HCARDW (I,IERR)
         IF (IERR.NE.0) GO TO 280
         ICARD=ICARD+1
10       CONTINUE
C
20    NCARD=ICSAV
      ICSAV=0
C
C  FIRST CARD IS IN COMMON
      CALL UMEMOV (ICBUF,IBUF,80)
      GO TO 50
C
30    CALL RPCARD (IBUF,IERR)
      IF (IERR.EQ.0) GO TO 50
C
C  STATUS ERROR - MUST BE END-OF-FILE
C
C  CHECK PROC FLAG - IF 1 WERE EXECUTING A PROC AND NOW MUST
C  SWITCH BACK TO REGULAR CARD INPUT
      IF (IPROCF.EQ.1) THEN
         IPROCF=0
         ICD=ICDSAV
         IERR=0
C     GET SAVED NEXT CARD
         CALL UMEMOV (IPBSAV,IBUF,80)
         GO TO 50
         ENDIF
C
      IF (IHCMND.EQ.0) GO TO 290
C
C  MAKE THE END-OF-FILE AN IMPLIED STOP CARD
      ICBUF(1)=28
      IF (IHCLDB.GT.0) WRITE (IOGDB,310) IERR,IHCMND
      GO TO 270
C
C  CHECK IF AN IMPLIED STOP
50    IF (IBUF(1).EQ.28) THEN
         CALL ULINE (LP,2)
         WRITE (LP,320)
         IHCMND=-IBUF(1)
         GO TO 300
         ENDIF
C
      IEND=72
C
C  CHECK FOR BLANK INPUT CARD - WARN IF SO AND IF FIRST TIME
      DO 60 I=1,IEND
         IF (IBUF(I).NE.IBLNK) GO TO 70
60       CONTINUE
      IF (IBLCRD.GT.0) THEN
         NBLCRD=NBLCRD+1
         IF (NBLCRD.EQ.0) THEN
            CALL ULINE (LP,2)
            WRITE (LP,330)
            CALL WARN
            ENDIF
         ENDIF
      GO TO 110
C
C  CHECK FOR COMMENT CHARACTER
70    DO 80 I=1,IEND
         IF (IBUF(I).EQ.IDOLR) GO TO 90
80       CONTINUE
      GO TO 100
C
C  FOUND A $ - RESET IEND
90    IEND=I-1
      IF (I.LE.1) GO TO 110
C
C  FIND FIELDS ON CARD
100   CALL UFREE (1,IEND)
C
C  IF $ IN FIRST FIELD, PUT TO FILE AND GET NEXT CARD
      IF (NFIELD.GT.0) GO TO 120
C
110   ICSAV=ICSAV+1
      GO TO 230
C
C  IF COMMAND WAS DEFINE PROC, LOOK FOR AN END(PROC) CARD INSTEAD OF
C  NEW COMMAND
120   IF (IEPFLG.EQ.1) GO TO 140
      IF (KEYWRD.NE.1) GO TO 140
      IF (IHCMND.NE.6.AND.
     *    IHCMND.NE.9.AND.
     *    IHCMND.NE.10.AND.
     *    IHCMND.NE.25) GO TO 140
      IF (IHCLDB.GT.0) WRITE (IOGDB,340) IBUF
C
C  PASSED ALL CHECKS, THEREFORE IT IS A DEF PROC
      CALL UPACK1 (IBUF(IFSTRT(1)),ITEMP,3)
C
C  IF NOT END(PROC) WRITE TO FILE
      IF (ITEMP.NE.LEND) GO TO 230
      NCHAR=IFSTOP(1)-IFSTRT(1)+1
      IF (NCHAR.EQ.3) GO TO 130
      IF (NCHAR.NE.7) GO TO 230
      CALL UPACK1 (IBUF(IFSTRT(1)+3),ITEMP,4)
      IF (ITEMP.NE.KEYLST(1,1)) GO TO 230
C
130   IEPFLG=1
C
C  RESET COMMENT COUNT
      ICSAV=0
      GO TO 230
C
140   IF (IBUF(IFSTRT(1)).EQ.LATSGN.AND.IFCARD.NE.0) GO TO 260
      ICSAV=0
C
      CALL UMEMST (IBLNK,ICMD,2)
      NFLD=1
      IBEG=IFSTRT(NFLD)
      IF (IBUF(IBEG).NE.LATSGN) GO TO 210
      IBEG=IBEG+1
      NCHAR=IFSTOP(NFLD)-IBEG+1
      IF (NCHAR.GT.8) NCHAR=8
      CALL UPACK1 (IBUF(IBEG),ICMD,NCHAR)
C
C  CHECK FOR COMMAND
      DO 150 ICOMND=1,NCMLST
         CALL UNAMCP (ICMD,ICMLST(1,ICOMND),IMATCH)
         IF (IMATCH.EQ.0) GO TO 160
150      CONTINUE
      CALL ULINE (LP,2)
      WRITE (LP,350)
      CALL ERROR
      GO TO 240
C
160   IHCMND=ICMLST(3,ICOMND)
      IF (IHCLDB.GT.0) WRITE (IOGDB,*) 'IHCMND=',IHCMND
C
C  CHECK IF COMMAND SHOULD HAVE A KEYWORD
      IF (ICMLST(4,ICOMND).EQ.0) GO TO 210
C
C  CHECK FOR KEYWORD
      NFLD=2
      IF (NFIELD.LT.NFLD) GO TO 180
      IBEG=IFSTRT(NFLD)
      NCHAR=IFSTOP(NFLD)-IBEG+1
      IF (NCHAR.GT.8) NCHAR=8
      CALL UPACK1 (IBUF(IBEG),IKEY,NCHAR)
      DO 170 IKEYWD=1,NKEYWD
         IF (IKEY(1).EQ.KEYLST(1,IKEYWD)) GO TO 200
170      CONTINUE
      GO TO 190
C
C  ALLOW FOR DEFAULT OF 'RESET' FOR FILESTAT COMMAND
180   IF (IHCMND.NE.21) GO TO 190
         IKEYWD=8
         GO TO 200
C
190   CALL ULINE (LP,2)
      WRITE (LP,360)
      CALL WARN
      IHCMND=0
      GO TO 240
C
200   KEYWRD=KEYLST(2,IKEYWD)
C
210   IF (IHCMND.EQ.0) THEN
         CALL ULINE (LP,2)
         WRITE (LP,380)
         CALL ERROR
         CALL WPCARD (IBUF)
         GO TO 30
         ENDIF
C
C  CHECK IF FIRST CARD FOR COMMAND
      IF (IFCARD.EQ.0) GO TO 220
C
C  CHECK IF COMMAND ALLOWS MORE THAN ONE INPUT CARD
      IF (IHCMND.EQ.03) GO TO 250
      IF (IHCMND.EQ.04) GO TO 250
      IF (IHCMND.EQ.14) GO TO 250
      IF (IHCMND.EQ.17) GO TO 250
      IF (IHCMND.EQ.21) GO TO 250
      IF (IHCMND.EQ.27) GO TO 250
      IF (IHCMND.EQ.29) GO TO 250
      IF (IHCMND.EQ.30) GO TO 250
      IF (IHCMND.EQ.31) GO TO 250
      IF (IHCMND.EQ.33) GO TO 250
      IF (IHCMND.EQ.34) GO TO 250
      IF (IHCMND.EQ.35) GO TO 250
      IF (IHCMND.EQ.36) GO TO 250
      IF (IHCMND.EQ.38) GO TO 250
      IF (IHCMND.EQ.39) GO TO 250
      IF (IHCMND.EQ.40) GO TO 250
      IF (IHCMND.EQ.41) GO TO 250
      IF (IHCMND.EQ.42) GO TO 250
C
220   IFCARD=1
C
C  OUTPUT CARD
230   NCARD=NCARD+1
      CALL HCARDW (NCARD,IERR)
      IF (IERR.NE.0) GO TO 280
C
C  CHECK IF STOP COMMAND
      IF (IHCMND.EQ.28) GO TO 300
      GO TO 30
C
240   CALL ULINE (LP,2)
      WRITE (LP,370)
      IFSTCD=1
      CALL WPCARD (IBUF)
      GO TO 30
C
250   CALL ULINE (LP,2)
      WRITE (LP,390)
      CALL ERROR
      CALL ULINE (LP,2)
      WRITE (LP,370)
      CALL WPCARD (IBUF)
      GO TO 30
C
C  SAVE THIS CARD FOR NEXT TIME
260   CALL UMEMOV (IBUF,ICBUF,80)
C
C  RESET CARD COUNT FOR TRAILING COMMENT CARDS
270   NCARD=NCARD-ICSAV
      GO TO 300
C
280   ISTAT=IERR
      GO TO 300
C
290   CALL ULINE (LP,2)
      WRITE (LP,400)
      CALL ERROR
C
C  SET STATUS TO END-OF-FILE
      ISTAT=-1
C
300   IF (IHCLDB.GT.0) WRITE (IOGDB,410) IHCMND,KEYWRD,ISTAT,ICSAV
C
      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IHCLTR.GT.0) WRITE (IOGDB,*) 'EXIT HCARDI'
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
310   FORMAT (' IN HCARDI - IERR=',I3,' IHCMND=',I3)
320   FORMAT ('0**NOTE** STOP COMMAND ASSUMED.')
330   FORMAT ('0**WARNING** BLANK INPUT CARD FOUND AND ',
     *   'WILL BE IGNORED.')
340   FORMAT (' IN HCARDI - IBUF=',80A1)
350   FORMAT ('0**ERROR** INVALID COMMAND.')
360   FORMAT ('0**WARNING** KEYWORD NOT RECOGNIZED OR MISSING.')
370   FORMAT ('0THE FOLLOWING CARD WILL BE IGNORED:')
380   FORMAT ('0**ERROR** COMMAND NOT FOUND ON THE FOLLOWING CARD:')
390   FORMAT ('0**ERROR** COMMAND DOES NOT ALLOW MORE THAN ',
     *   'ONE INPUT CARD.')
400   FORMAT ('0**ERROR** NO COMMAND ENTERED.')
410   FORMAT (' IN HCARDI - IHCMND=',I4,' KEYWRD=',I4,' ISTAT=',I4,
     *   ' ICSAV=',I4)
C
      END
