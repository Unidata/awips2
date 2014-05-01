C MEMBER HDELFN
C  (from old member HCLDELFN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:12:20 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDELFN (IGL,NAME,IPASS,NUMCAR)
C
C
C          ROUTINE:  HDELFN
C
C             VERSION:  1.0.2 4-19-82
C                            ADD CHECK FOR DELG AND DELL
C             VERSION:  1.0.1 3-22-82
C                            CHANGE TO DELETE GLOBAL DEFS
C
C             VERSION:  1.0.0
C
C                DATE:  1-4-82
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE TO DELETE A PROCEDURE DEFINITION RECORD
C    CORRECT PASSWORD MUST BE SPECIFIED
C    SEARCHES ALL LOCAL PROCS TO SEE IF FUNCTION IS USED
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL        I     I    1     DELG/DELL INDICATOR
C
C       NAME       A8    I    2     NAME OF FUNCTION
C
C       IPASS      A4    I    1     PASSWORD FROM INPUT
C
C       NUMCAR     I     I    1     # OF CHARACTERS IN NAME
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'udatas'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hdatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER IROUT(2),ITEMP(2),NAME(2),IFUN(256),IXBUF(4),IPROC(512)
      INTEGER ISTRNG(20),IUPSTR(80),IATCMP(2),IUPATC(8),IDELET(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdelfn.f,v $
     . $',                                                             '
     .$Id: hdelfn.f,v 1.1 1995/09/17 18:41:54 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT(1)/4HHDEL/,IROUT(2)/4HFN  /,IDELET(1)/4HDELE/,
     1    IDELET(2)/4HTED /,MAX/256/,MAX2/512/,MAXS/20/,
     2    IATCMP(1)/4H@COM/,IATCMP(2)/4HP   /
C
C***********************************************************************
C
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C
      IERR=0
      ITYPE=2
      ISUB=1
      IUNIT=KDEFNL
      IXUNIT=KINDXL
C
C        GET THE FUNCTION RECORD
C
      CALL HFNDDF(NAME,IREC,ITYPE,IXREC)
      IF (IREC.EQ.0) GO TO 70
C
C        CHECK FOR DELG AND DELL
C
      IGL=IGL*2
      IF (ITYPE.EQ.IGL) GO TO 40
      IERR=1
      IF (IGL.GT.0) GO TO 20
      WRITE (LPE,10)
10    FORMAT (' **ERROR** @DELG AND @DELETG COMMANDS NOT VALID FOR ',
     1    'LOCAL FUNCTIONS')
      GO TO 190
20    CONTINUE
      WRITE (LPE,30)
30    FORMAT (' **ERROR** @DELL AND @DELETEL COMMANDS NOT VALID FOR',
     1       ' GLOBAL FUNCTIONS')
      GO TO 190
40    CONTINUE
      IF (ITYPE.GT.0) GO TO 50
C
C         GLOBAL - SEE IF USED BY LOCAL PROCS
C
      ISUB=5
      IUNIT=KDEFNG
      IXUNIT=KINDXG
      CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      IPOINT=IXBUF(4)
      CALL HSRLDR(NAME,IPOINT,3,IDUM,ISTAT)
      IF (ISTAT.NE.0) IERR=1
50    CONTINUE
      CALL HGTRDN(IUNIT,IREC,IFUN,MAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C
C        CHECK PASSWORD
C
      IF (IPASS.EQ.IFUN(6)) GO TO 90
      WRITE (LPE,60) NAME
60    FORMAT (' **ERROR**  INVALID PASSWORD FOR FUNCTION ',2A4)
      IERR=1
      GO TO 190
70    CONTINUE
      WRITE (LPE,80) NAME
80    FORMAT (' **ERROR**  FUNCTION ',2A4,' NOT DEFINED')
      IERR=1
      GO TO 190
90    CONTINUE
C
C        SEE IF FUNCTION IS IN ANY PROCS
C
      IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 150
      DO 140 I =IS,IE
          CALL UREADT (IXUNIT,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 170
C IS PROC DELETED?
          CALL UNAMCP(IDELET,IXBUF(1),ISTAT)
          IF (ISTAT.EQ.0) GO TO 140
C GET THE PROC
          IF (IXBUF(3).LE.0) GO TO 170
          CALL HGTRDN(IUNIT,IXBUF(3),IPROC,MAX2,ISTAT)
          IF (ISTAT.NE.0) GO TO 170
C SEE IF FUNC IN PROC
          IPOS=12+2*IPROC(9)
          NUMCOM=IPROC(IPOS - 2)
          IF (NUMCOM.EQ.0) GO TO 140
          DO 110 II=1,NUMCOM
             CALL HGTSTR(MAXS,IPROC(IPOS),ISTRNG,LENGTH,ISTAT)
             IF (ISTAT.NE.0) GO TO 170
             NWDS=IPROC(IPOS - 1)
             IPOS=IPOS+NWDS+1
C LOOK FOR @COMP IN STRING
             CALL UFDSTR(IATCMP,1,IUPATC,8,2,5,ISTRNG,1,
     1                   IUPSTR,80,NWDS,INDEX,ISTAT)
             IF (ISTAT.NE.0) GO TO 100
             IF (INDEX.EQ.0) GO TO 110
C LOOK FOR FUNCTION NAME IN STRING
             CALL UFDSTR(NAME,1,IUPATC,8,2,NUMCAR,ISTRNG,1,
     1                   IUPSTR,80,NWDS,INDEX2,ISTAT)
             IF (ISTAT.NE.0) GO TO 100
             IF (INDEX2.EQ.0) GO TO 110
C MAKE SURE ABOUT NAME
             IF (INDEX2.LT.INDEX+6) GO TO 110
            IF (IUPSTR(INDEX2-1).NE.IBLNK.AND.IUPSTR(INDEX2-1).NE.COMMA)
     1         GO TO 110
             IF (IUPSTR(INDEX2+NUMCAR).NE.IBLNK.AND.
     1          IUPSTR(INDEX2+NUMCAR).NE.COMMA) GO TO 110
C FOUND IT
             GO TO 120
100          CONTINUE
             IERR=1
110          CONTINUE
C    TRY NEXT PROC
          GO TO 140
120       CONTINUE
          WRITE (LPE,130) NAME,IPROC(4),IPROC(5)
130       FORMAT (' **ERROR**  FUNCTION ',2A4,' FOUND IN PROCEDURE ',
     1           2A4)
          IERR=1
140       CONTINUE
C
C           DELETE IT IF NO ERRORS
C
150   CONTINUE
      IF (IERR.EQ.1) GO TO 190
C  CHANGE INDEX RECORD
      CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      CALL UMEMOV (IDELET,IXBUF(1),2)
      CALL UWRITT(IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
C  CHANGE FUNCTION RECDORD
      IFNUM=IFUN(2)
      IFUN(2)=0
      CALL WVLRCD(IUNIT,IREC,IFUN(1),IFUN,LRECLH,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      CALL HPFNUM(IFNUM,ISTAT)
      IF (ISTAT.NE.0) GO TO 170
      WRITE (LP,160) NAME
160   FORMAT (' *** FUNCTION ',2A4,' DELETED')
      IF (ITYPE.GT.0) CALL HDLLDR(NAME,ITYPE,ISTAT)
      GO TO 190
C
C       SYSTEM ERROR
C
170   CONTINUE
      WRITE (LPE,180)
180   FORMAT (' **ERROR**  SYSTEM ERROR')
      IERR=1
C
C    END
C
190   CONTINUE
      IF (IERR.EQ.1) WRITE (LP,200) NAME
200   FORMAT (' *** FUNCTION ',2A4,' NOT DELETED')
      IF (IERR.EQ.1) CALL ERROR
      CALL UMEMOV (ITEMP,OPNAME,2)
      RETURN
      END
