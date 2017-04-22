C MEMBER EX4
C  (from old member FCEX4)
C
      SUBROUTINE EX4(DATA,NUM)
C.......................................
C     THIS SUBROUTINE EXECUTES THE CLEAR OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     AUGUST 1979
C.......................................
      DIMENSION DATA(1)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex4.f,v $
     . $',                                                             '
     .$Id: ex4.f,v 1.1 1995/09/17 18:57:12 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT (1H0,14H** EX4 ENTERED)
C     NO DEBUG OUTPUT.
C.......................................
C     SET DATA ARRAY TO ZERO.
      DO 100 I=1,NUM
      DATA(I)=0.0
  100 CONTINUE
C.......................................
      RETURN
      END
