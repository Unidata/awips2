C MEMBER HDELTN
C  (from old member HCLDELTN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/29/94.08:13:23 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HDELTN (IGL,NAME,PASS)
C
C
C          ROUTINE:  HDELTN
C
C             VERSION:  1.0.2 4-19-82 JCE
C                              ADD CHECKS FOR DELG AND DELL
C             VERSION:  1.0.1  3-22-82 JCE
C                              ABLE TO DELETE GLOBAL TECHNIQUES
C
C             VERSION:  1.0.0
C
C                DATE:  12-8-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    SUBROUTINE TO DELETE A TECHNIQUE DEFINITION
C    CORRECT PASSWORD IS REQUIRED  AND NO FUNCTION MUST USE THE
C    TECHNIQUE
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IGL       I     I     1     DELG/DELL INDICATOR
C
C       NAME      A8    I     2     TECHNIQUE NAME
C
C       PASS      A4    I     1     PASSWORD
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'common/where'
      INCLUDE 'hclcommon/hunits'
      INCLUDE 'hclcommon/hindx'
      INCLUDE 'hclcommon/hdatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER NAME(2),PASS,IROUT(2),ITEMP(2),IXBUF(4),IDELET(2)
      INTEGER ITNBUF(600),IFNBUF(600)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hdeltn.f,v $
     . $',                                                             '
     .$Id: hdeltn.f,v 1.1 1995/09/17 18:41:58 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
      DATA IROUT(1)/4HHDEL/,IROUT(2)/4HTN  /,IMAX/600/
      DATA IDELET(1)/4HDELE/,IDELET(2)/4HTED /
C
C***********************************************************************
C
C
      IERR=0
      ITYPE=3
      ISUB=2
      IUNIT=KDEFNL
      IXUNIT=KINDXL
C
      CALL UMEMOV (OPNAME,ITEMP,2)
      CALL UMEMOV (IROUT,OPNAME,2)
C
C          FIND NAME IN INDEX AND READ RECORD
C
      CALL HFNDDF(NAME,IREC,ITYPE,IXREC)
      IF (IREC.EQ.0) GO TO 70
C
C        CHECK FOR DELG AND DELL
C
      IGL=IGL*3
      IF (ITYPE.EQ.IGL) GO TO 40
      IERR=1
      IF (IGL.GT.0) GO TO 20
      WRITE (LPE,10)
10    FORMAT (' **ERROR** @DELG AND DELETEG COMMANDS NOT VALID FOR',
     1       ' LOCAL FUNCTIONS')
      GO TO 180
20    CONTINUE
      WRITE (LPE,30)
30    FORMAT (' **ERROR** @DELL AND @DELETEL COMMANDS NOT VALID FOR',
     1       ' GLOBAL FUNCTIONS')
      GO TO 180
40    CONTINUE
      IF (ITYPE.GT.0) GO TO 50
C
C        GLOBAL - SEE IF USED BY ANY LOCAL FUNCTIONS
C
      IXUNIT=KINDXG
      IUNIT=KDEFNG
      ISUB=6
      CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
      IPOINT=-IXBUF(4)
      CALL HSRLDR(NAME,IPOINT,3,IDUM,ISTAT)
      IF (ISTAT.NE.0) IERR=1
50    CONTINUE
      CALL HGTRDN(IUNIT,IREC,ITNBUF,IMAX,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
      INTNUM=ITNBUF(2)
      IF (ITYPE.LT.0) INTNUM=-INTNUM
C
C         CHECK PASSWORD
C
      IF (PASS.EQ.ITNBUF(6)) GO TO 90
      WRITE (LPE,60) NAME
60    FORMAT (' **ERROR**  INVALID PASSWORD FOR TECHNIQUE ',2A4)
      CALL ERROR
      GO TO 180
C
70    CONTINUE
      WRITE (LPE,80) NAME
80    FORMAT (' **ERROR**  TECHNIQUE ',2A4,' NOT DEFINED')
      CALL ERROR
      GO TO 180
90    CONTINUE
C
C          SEE IF TECHNIQUE IS USED BY ANY FUNCTION
C
      IS=HINDEX(2,ISUB)
      IE=HINDEX(3,ISUB)
      IF (IE.LT.IS) GO TO 140
      DO 130 I=IS,IE
          CALL UREADT (IXUNIT,I,IXBUF,ISTAT)
          IF (ISTAT.NE.0) GO TO 160
C IS FUNCTION DELETED?
          CALL UNAMCP(IDELET,IXBUF(1),ISTAT)
          IF (ISTAT.EQ.0) GO TO 130
C GET THE FUNCTION
          IRECN=IXBUF(3)
          IF (IRECN.LE.0) GO TO 160
          CALL HGTRDN(IUNIT,IRECN,IFNBUF,IMAX,ISTAT)
          IF (ISTAT.NE.0) GO TO 160
C SEE IF TECHNIQUE IS IN FUNCTION
          NUMTN=IFNBUF(9)
          IF (NUMTN.EQ.0) GO TO 130
          DO 100 II=1,NUMTN
             IF (IFNBUF(II+9).EQ.INTNUM) GO TO 110
100       CONTINUE
C
C FELL THROUGH - TRY NEXT FUNCTION
C
          GO TO 130
C
C FOUND TECHNIQUE
C
110       CONTINUE
          WRITE (LPE,120) NAME,IFNBUF(4),IFNBUF(5)
120       FORMAT (' **ERROR** TECHNIQUE ',2A4,' FOUND IN FUNCTION ',2A4)
          IERR=1
          CALL ERROR
130       CONTINUE
C
C           TIME TO DELETE IF NO ERRORS
C
140   CONTINUE
      IF (IERR.EQ.1) GO TO 180
C CHANGE INDEX RECORD
      CALL UREADT (IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
      CALL UMEMOV (IDELET,IXBUF(1),2)
      CALL UWRITT(IXUNIT,IXREC,IXBUF,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
C CHANGE TECHNIQUE RECORD
      ITNBUF(2)=0
      CALL WVLRCD(IUNIT,IREC,ITNBUF(1),ITNBUF,LRECLH,ISTAT)
      IF (ISTAT.NE.0) GO TO 160
      WRITE (LP,150) NAME
150   FORMAT (' *** TECHNIQUE ',2A4,' DELETED')
      IF (ITYPE.GT.0) CALL HDLLDR(NAME,ITYPE,ISTAT)
      GO TO 180
C
C          SYSTEM ERROR
C
160   CONTINUE
      WRITE (LPE,170) NAME
170   FORMAT (' **ERROR**  SYSTEM ERROR FOR TECHNIQUE ',2A4)
      IERR=1
      CALL ERROR
C
C   DONE
C
180   CONTINUE
      IF (IERR.EQ.1) WRITE (LPE,190) NAME
190   FORMAT (' ***  TECHNIQUE ',2A4,' NOT DELETED')
      IF (IHCLDB.EQ.3) WRITE (IOGDB,200)
200   FORMAT ('  HDELTN EXECUTED')
      CALL UMEMOV (ITEMP,OPNAME,2)
C
      RETURN
C
      END
