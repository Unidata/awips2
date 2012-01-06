C MEMBER COX9
C  (from old member FCCOX9)
C
      SUBROUTINE COX9(POLD,COLD,PONEW,CONEW)
C
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE
C     FOR THE MUSKINGUM ROUTING OPERATION
C
C     THIS SUBROUTINE ORIGINALLY WRITTEN BY
C          DAVID REED-HRL    OCT 1979
C
C
C
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
C
C     COMMON BLOCKS
C
      COMMON /FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox9.f,v $
     . $',                                                             '
     .$Id: cox9.f,v 1.1 1995/09/17 18:47:31 dws Exp $
     . $' /
C    ===================================================================
C
C
C     CHECK TRACE LEVEL-FOR THIS SUBROUTINE =1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** COX9 ENTERED)
C
C     CHECK TO SEE IF DEBUG OUTPUT IS NEEDED
C
      IBUG=0
      IF(IDBALL.GT.0)  IBUG=1
      DO 10 I=1,NDEBUG
      IF(IDEBUG(I).EQ.9) GO TO 11
   10 CONTINUE
      GO TO 12
   11 IBUG=1
   12 IF(IBUG.EQ.0)GO TO 100
C
C     PRINT DEBUG OUTPUT
      WRITE (IODBUG,901)COLD(1),COLD(2)
  901 FORMAT(1H0,10X,19HOLD CARRYOVER ARRAY,//,11X,2(F10.3,2X))
C
C      SET NEW CARRYOVER VALUES EQUAL TO THE OLD VALUES
C
  100 CONEW(1)=COLD(1)
      CONEW(2)=COLD(2)
C
C
C............................................................
C
C     CARRYOVER TRANSFER RULE
C     NEW CARRYOVER VALUES ARE SET EQUAL TO THE OLD VALUES
C
C.................................................................
C
C
      IF(IBUG.EQ.0) GO TO 101
      WRITE(IODBUG,902)CONEW(1),CONEW(2)
  902 FORMAT(1H0,10X,21HNEW CARRYOVER VALUES,//,11X,2(F10.3,2X))
  101 RETURN
      END
