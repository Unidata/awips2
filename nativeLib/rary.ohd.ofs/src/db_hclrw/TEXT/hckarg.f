C MEMBER HCKARG
C  (from old member HCLCHECK)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/18/94.12:40:52 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE HCKARG (IS,IE,ITYPE,ANS,MAX,NANS,ISTAT)
C
C          ROUTINE:  HCKARG
C
C             VERSION:  1.0.0
C
C                DATE:  8-7-81
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WILL CHECK AN ARGUMENT TO DETERMINE TYPE
C    AND PACK THE VALUE INTO ANS WHICH IS AN ARRAY
C    CHARACTERS ARE ASSUMED TO BE IN IBUF(CARD IMAGE) AND
C    ROUTINE WORKS ON COLUMNS IS TO IE.
C
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         IS        I    I     1     STARTING COLUMN IN IBUF
C         IE        I    I     1     ENDING COLUMN IN IBUF
C       ITYPE       I    O     1     THE TYPE FOUND 1=INT,2=REAL,
C                                        3=ALPHA, 4=LOGICAL,5=DATE
C       ANS         I    O     1     THE ANSWER ACCORDING TO TYPE
C       MAX         I    I     1     THE DIMENSION OF ANS
C       NANS        I    O     1      THE LENGTH OF ANS AFTER FILLED
C       ISTAT       I    O     1     STATUS 0=OK, NOT 0=ERROR
C
C
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufreei'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER ANS(72)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hckarg.f,v $
     . $',                                                             '
     .$Id: hckarg.f,v 1.1 1995/09/17 18:41:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
      DATA LQUOTE/4H'   /
C
C
C***********************************************************************
C
C
      ISTAT=0
      NANS=1
      ITYPE=0
      CALL UMEMST (0,ANS,MAX)
      IF (IS.GT.IE) GO TO 120
C
C CHECK FOR INTEGER
C
      CALL UCKINT (IBUF,IS,IE,IERR)
      IF (IERR.EQ.1) GO TO 10
      CALL UNUMIC (IBUF,IS,IE,ANS(1))
      ITYPE=1
      GO TO 120
C
C CHECK FOR FLOATING POINT
C
10    CALL UCKFLT (IBUF,IS,IE,KK,IERR)
      IF (IERR.EQ.1) GO TO 50
C
C NOW CONVERT IT
C
      A=0.0
      B=0.0
      IF (KK.EQ.IS) GO TO 20
      CALL UNUMIC (IBUF,IS,KK-1,NUM)
      A=NUM
20    IF (KK.EQ.IE) GO TO 40
      CALL UNUMIC (IBUF,KK+1,IE,NUM)
      N=IE-KK
      B=NUM
      B=B*10.0**(-N)
      IF (A.LT.0.0) B=-B
      A=A+B
40    CALL USWITC (A,ANS(1))
      ITYPE=2
      GO TO 120
C
C CHECK FOR LOGICAL
C
50    CALL UCKLOG (IS,IE,ANS(1),IERR)
      IF (IERR.NE.0) GO TO 60
      ITYPE=4
      GO TO 120
C
C CHECK FOR A DATE  MM/DD/YY/TTCCCC
C
60    CALL HCKDAT (IS,IE,ANS,IERR)
      IF (IERR.NE.0) GO TO 70
      NANS=7
      ITYPE=5
      GO TO 120
C
C ALL ELSE HAS FAILED, SET TO ALPHA
C
70    IF (IBUF(IS).EQ.LQUOTE) IS=IS+1
      IF (IBUF(IE).EQ.LQUOTE) IE=IE-1
      N=IE-IS+1
      IF (N.GT.MAX*4) GO TO 100
      CALL HPTSTR (IBUF(IS),N,ANS,IERR)
      IF (IERR.NE.0) GO TO 80
      NANS=N
      ITYPE=3
      GO TO 120
C
C ERROR
C
80    WRITE (LP,90) IS,IE,IERR
      ISTAT=1
90    FORMAT ('0**ERROR** INVALID ARGUMENT IN COLS ',I2,' TO ',I2,
     1  ' ISTAT=',I2)
      GO TO 120
100   WRITE (LP,110)
110   FORMAT ('0**ERROR** IN HCKARG - STRING ARRAY TOO SMALL')
      GO TO 120
C
120   CONTINUE
      IF (IHCLDB.NE.3) GO TO 220
      WRITE (IOGDB,130) IS,IE,ITYPE,MAX,NANS,ISTAT
130   FORMAT (' IN HCKARG - IS=',I2,' IE=',I2,' ITYPE=',I2,
     1 ' MAX=',I3,' NANS=',I3,' ISTAT=',I2)
      IF (ITYPE.LT.1.OR.ITYPE.GT.5) GO TO 220
      GO TO (140,160,180,140,200),ITYPE
140    WRITE (IOGDB,150) ANS(1)
150   FORMAT (20X,'ANS=',I10)
      GO TO 220
160   WRITE (IOGDB,170) ANS(1)
170   FORMAT (20X,'ANS=',F20.5)
      GO TO 220
180   CONTINUE
      WRITE (IOGDB,190) (ANS(I),I=1,NANS)
      GO TO 220
190   FORMAT (20X,'ANS=',20A4)
200   WRITE (IOGDB,210) (ANS(I),I=1,7)
210   FORMAT (20X,'ANS=',5I3,A4,I3)
C
220   RETURN
C
      END
