C MODULE FSTATS
C-----------------------------------------------------------------------
C
C  CONTROL ROUTINE FOR STATUS REPORT.
C
      SUBROUTINE FSTATS (NOREAD)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      CHARACTER*10 XCHAR
      PARAMETER (NCOMND=15)
      CHARACTER*10 XCOMND(NCOMND)/
     *    'CG',
     *    'CGROUP',
     *    'FG',
     *    'FGROUP',
     *    'SEG',
     *    'SEGMENT',
     *    'RC',
     *    'ALL',
     *    'OB',
     *    'OBSOLETE',
     *    'NOOB',
     *    'NOOBSOLETE',
     *    'DIMENSIONS',
     *    'ESP',
     *    '$'
     *    /
      PARAMETER (NSGOPT=7)
      CHARACTER*10 XSGCMD(NSGOPT)/
     *   'CONN',
     *   'CONNECTIVI',
     *   'D1',
     *   'DESCRP1',
     *   'D2',
     *   'DESCRP2',
     *   'ALL'
     *   /
      CHARACTER*80 XCARD(30)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_status/RCS/fstats.f,v $
     . $',                                                             '
     .$Id: fstats.f,v 1.5 2002/02/11 20:22:34 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER FSTATS'
C
      IBUG=IFBUG('STAT')
C
      IF (IBUG.EQ.1) WRITE (IODBUG,*) 'NOREAD=',NOREAD
C
      IDIMNS=1
      IOPT=0
      IALL=0
      ICG=0
      IFG=0
      ISEG=0
      IRC=0
      IESP=0
      NOOBSO=-1
C
C  CHECK IF INPUT CARDS TO BE READ
      IF (NOREAD.EQ.1) THEN
         IDIMNS=-1
         GO TO 260
         ENDIF
C
C  READ INPUT CARDS FOR STATUS COMMAND
      CALL CDINPT (XCARD,30,NXCARD,'STATUS  ',IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,320)
         GO TO 40
         ENDIF
C
C  CHECK IF NO INPUT
      IF (NXCARD.EQ.0) THEN
C     SET DEFAULT OPTIONS
40       ICG=1
         IFG=1
         ISEG=1
         IRC=1
         IESP=1
         ISEGTB=3
         NOOBSO=1
         GO TO 260
         ENDIF
C
C  PROCESS EACH CARD INPUT AND SET OPTIONS
      LXCARD=72
      DO 250 IXCARD=1,NXCARD
C     GET FIRST FIELD ON CARD
         NSCAN=1
         CALL USCAN (XCARD(IXCARD),LXCARD,' ',1,NSCAN,
     *      XCHAR,LEN(XCHAR),LXCHAR,IERR)
         IF (IBUG.GT.0) WRITE (IODBUG,*) 'XCHAR=',XCHAR
         IF (XCHAR.EQ.' ') GO TO 250
         IF (XCHAR(1:1).EQ.'$') GO TO 245
         DO 60 ICOMND=1,NCOMND
            IF (XCHAR.EQ.XCOMND(ICOMND)) GO TO 70
60          CONTINUE
         GO TO 80
70       IF (IBUG.GT.0) WRITE (IODBUG,*) 'ICOMND=',ICOMND
         GO TO (90,90,100,100,110,
     *          110,200,210,220,220,
     *          230,230,240,243,245),ICOMND
C     INVALID STATUS COMMAND
80       WRITE (IPR,330) XCARD(IXCARD)
         GO TO 250
C     CG OR CGROUP COMMAND
90       ICG=1
         IOPT=1
         GO TO 250
C     FG OR FGROUP COMMAND
100      IFG=1
         IOPT=1
         GO TO 250
C     SEG OR SEGMENT COMMAND  -   -   -   -   -   -   -   -   -   -   -
110      ISEG=1
         IOPT=1
C     CHECK FOR SUBSEQUENT FIELDS FOR SEGMENT SUBCOMMANDS
120      NSCAN=NSCAN+1
         CALL USCAN (XCARD(IXCARD),LXCARD,' ',1,NSCAN,
     *      XCHAR,LEN(XCHAR),LXCHAR,IERR)
         IF (IBUG.GT.0) WRITE (IODBUG,*) 'XCHAR=',XCHAR
         IF (XCHAR.EQ.' ') GO TO 250
         DO 130 ISGCMD=1,NSGOPT
            IF (XCHAR.EQ.XSGCMD(ISGCMD)) GO TO 140
130         CONTINUE
         GO TO 150
140      IF (IBUG.GT.0) WRITE (IODBUG,*) 'ISGCMD=',ISGCMD
         GO TO (160,160,170,170,180,180,190),ISGCMD
C     INVALID SEGMENT SUBCOMMAND
150      WRITE (IPR,340) XCARD(IXCARD)
         GO TO 120
C     CONN OR CONNECTIVITY SUBCOMMAND
160      ISEGTB=ISEGTB*2
         GO TO 120
C     D1 OR DESCRP1 SUBCOMMAND
170      ISEGTB=ISEGTB*3
         GO TO 120
C     D2 OR DESRCP2 SUBCOMMAND
180      ISEGTB=ISEGTB*5
         GO TO 120
C     ALL SUBCOMMAND
190      ISEGTB=ISEGTB*2*3*5
         GO TO 250
C     END OF SEGMENT SUBCOMMANDS  -   -   -   -   -   -   -   -   -   -
C     RC COMMAND
200      IRC=1
         IOPT=1
         GO TO 250
C     ALL COMMAND - ALSO SET ALL SWITCH FOR SEGMENT SUBCOMMAND
210      IALL=1
         IOPT=1
         ISEGTB=ISEGTB*2*3*5
         IESP=1
         GO TO 250
C     OB OR OBSOLETE COMMAND
220      NOOBSO=0
         GO TO 250
C     NOOB OR NOOBSOLETE COMMAND
230      NOOBSO=1
         GO TO 250
C     DIMENSIONS OPTION
240      IDIMNS=-1
         IOPT=1
         GO TO 250
C     ESP OPTION
243      IESP=1
         IOPT=1
         GO TO 250
C     COMMENT
245      GO TO 250         
250      CONTINUE
C
C  SET TO DEFAULT ANY OPTIONS NOT ENTERED
      IF (IALL.EQ.1.OR.IOPT.EQ.0) GO TO 40
      IF (ISEGTB.EQ.1) ISEGTB=3
      IF (NOOBSO.EQ.-1) NOOBSO=1
C
260   IF (IBUG.GT.0) WRITE (IODBUG,*)
     *    ' ICG=',ICG,
     *    ' IFG=',IFG,
     *    ' ISEG=',ISEG,
     *    ' IRC=',IRC,
     *    ' ISEGTB=',ISEGTB,
     *    ' NOOBSO=',NOOBSO,
     *    ' IDIMNS=',IDIMNS
C
      IPART1=1
      IPART2=1
      IF (NOREAD.EQ.1) IPART2=0
C
      CALL FSTAT1 (IPART1,IPART2)
C
C  CHECK IF ONLY DIMENSIONS TO BE PRINTED
      IF (IDIMNS.EQ.-1) GO TO 300
C
C  CHECK IF TO PRINT CARRYOVER GROUP STATUS
      IF (ICG.EQ.1) THEN
         CALL FSTACG
         ENDIF
C
C  CHECK IF TO PRINT FORECAST GROUP STATUS
      IF (IFG.EQ.1) THEN
         CALL FSTAFG (NOOBSO)
         ENDIF
C
C  CHECK IF TO PRINT SEGMENT STATUS
      IF (ISEG.EQ.1) THEN
         CALL FSTASG (NOOBSO,ISEGTB)
         ENDIF
C
C  CHECK IF TO PRINT RATING CURVE STATUS
      IF (IRC.EQ.1) THEN
         CALL FSTARC (NOOBSO)
         ENDIF
C
C  CHECK IF TO PRINT ESP STATUS
      IF (IESP.EQ.1) THEN
         IFIRST=0
         CALL FSTAHD (IFIRST)
         CALL ESTS
         ENDIF
C
300   CALL UCLOSL
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT FSTATS'
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
320   FORMAT ('0**NOTE** STATUS COMMAND CONTINUES USING DEFAULT ',
     *   'OPTIONS.')
330   FORMAT ('0**WARNING** THE FOLLOWING CARD CONTAINS AN INVALID ',
     *  'STATUS SUBCOMMAND AND WILL BE IGNORED:' / 1X,A)
340   FORMAT ('0**WARNING** THE FOLLOWING FIELD IS INVALID FOR THE ',
     *  'SEGMENT SUBCOMMAND AND WILL BE IGNORED:' / 1X,A)
C
      RETURN
C
      END
