C MEMBER HFND3B
C  (from old member HCLSTRNG)
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE HFND3B (IBUF,ISTRT,ISTOP,IPOS)
C
C          ROUTINE:  HFND3B
C
C             VERSION:  1.0.0
C
C                DATE:  8-21-81
C
C              AUTHOR:  JIM ERLANDSON
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    ROUTINE SEARCHES IBUF(ISTRT) TO IBUF(ISTOP) FOR THREE
C    CONSECUTIVE BLANKS.  IF FOUND, RETURNS THE POSITION OF THE
C    FIRST OF THE BLANKS. IF NOT FOUND, RETURNS ISTOP+1
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IBUF        I    I     ?     INPUT BUFFER
C
C       ISTRT       I    I     1     STARTING POSITION
C
C       ISTOP       I    I     1     ENDING POSITION
C
C       IPOS        I    O     1     POSITION BLANKS FOUND
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'udebug'
      INCLUDE 'uio'
      INCLUDE 'udatas'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IBUF(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_hclrw/RCS/hfnd3b.f,v $
     . $',                                                             '
     .$Id: hfnd3b.f,v 1.1 1995/09/17 18:42:17 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      ICOUNT=0
      IPOS=0
      IF (ISTRT.GT.ISTOP) GO TO 30
      DO 20 I =ISTRT,ISTOP
          IF (IBUF(I).NE.IBLNK) GO TO 10
          IF (IPOS.EQ.0) IPOS=I
          ICOUNT=ICOUNT+1
          IF (ICOUNT.EQ.3) GO TO 40
          GO TO 20
10        CONTINUE
          IPOS=0
          ICOUNT=0
20        CONTINUE
C
C     3 CONTIGUOUS BLANKS NOT FOUND
C
30    CONTINUE
      IPOS=ISTOP+1
40    CONTINUE
      IF (IHCLTR.GT.2) WRITE (IOGDB,50) ISTRT,ISTOP,IPOS
50    FORMAT (' EXIT HFND3B - ISTRT=',I2,' ISTOP=',I2,' IPOS=',I2)
C
      RETURN
C
      END
