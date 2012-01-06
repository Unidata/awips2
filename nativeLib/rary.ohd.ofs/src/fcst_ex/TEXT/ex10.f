C MEMBER EX10
C  (from old member FCEX10)
C
      SUBROUTINE EX10(PO,QZERO,QIN,QOUT,QT)
C.......................................
C     THIS IS THE EXECUTION SUBROUTINE FOR THE ADD/SUB OPERATION.
C        MISSING DATA VALUES ARE ALLOWED IN THIS OPERATION.
C        WORKING SPACE NEEDED IF TIME INTERVALS OF TIME SERIES ARE NOT
C        THE SAME.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C        ERIC ANDERSON - HRL   OCT. 1979
C.......................................
      DIMENSION PO(1),QIN(1),QOUT(1),QT(1)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/ex10.f,v $
     . $',                                                             '
     .$Id: ex10.f,v 1.1 1995/09/17 18:56:32 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** EX10 ENTERED)
C.......................................
C     GET CONTROL VALUES FROM THE PO ARRAY.
      IADD=PO(2)
      ICO=PO(11)
      IDT=PO(10)
      IODT=PO(6)
      NEG=PO(12)
C.......................................
C
C     CALL THE ADD/SUBTRACT SUBROUTINE.
      IADD = IADD-2
      CALL FADD(IADD,ICO,NEG,IDT,IODT,QZERO,QIN,QOUT,QT)
      IF (ITRACE.GE.1) WRITE(IODBUG,901)
  901 FORMAT(1H0,12H** EXIT EX10)
      RETURN
      END
