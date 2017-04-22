C MEMBER FCLCD
C
      SUBROUTINE FCLCD(I,J)
C                             LAST UPDATE: 01/03/95.09:58:34 BY $WC21DT
C
C.......................................
C     THIS SUBROUTINE COMPUTES THE LOWEST COMMON DENOMINATOR
C        OF I AND J.  I IS THE LOWEST COMMON DENOMINATOR
C        UPON OUTPUT.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ED JOHNSON - HRL     JUNE 1979
C.......................................
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_util/RCS/fclcd.f,v $
     . $',                                                             '
     .$Id: fclcd.f,v 1.1 1995/09/17 18:54:20 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=3
      IF (ITRACE.GE.3) WRITE (IODBUG,900)
  900 FORMAT (1H0,16H** FCLCD ENTERED)
C     NO DEBUG OUTPUT - NO ERROR MESSAGES.
C.......................................
C     FIND MAX AND MIN OF I AND J - CAN'T BE LT 1.
      MAX=MAX0(MAX0(I,J),1)
      MIN=MAX0(MIN0(I,J),1)
C
C     TEST IF MAX IS LOWEST COMMON DENOMINATOR.
      I=MAX
      IF (MOD(MAX,MIN).EQ.0) RETURN
C
C     CHECK MULTIPLES OF THE MAX.
      DO 100 MULT=2,MIN
      I=MAX*MULT
      IF (MOD(I,MIN).EQ.0) RETURN
  100 CONTINUE
C.......................................
C     CAN NOT GET HERE.
      RETURN
      END
