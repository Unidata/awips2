C MEMBER COX10
C  (from old member FCCOX10)
C
      SUBROUTINE COX10(POLD,COLD,PONEW,CONEW)
C.......................................
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE
C        ADD/SUBTRACT OPERATION.  OLD CARRYOVER IS USED AS IS
C        IF... 1. BOTH PARAMETER SETS NEED CARRYOVER VALUE, AND
C        2. TIME SERIES A HAS THE SAME DIMENSION AND THE
C        SAME UNITS IN BOTH THE OLD AND NEW PARAMETER SETS.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1)
C
C     COMMON BLOCK.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox10.f,v $
     . $',                                                             '
     .$Id: cox10.f,v 1.1 1995/09/17 18:47:08 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL =1. - NO DEBUG OUTPUT.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** COX10 ENTERED)
C.......................................
C     CHECK THAT BOTH OLD AND NEW HAVE CARRYOVER VALUES.
      ICO=PONEW(11)
      IF(ICO.EQ.0) RETURN
      ICO=POLD(11)
      IF(ICO.EQ.0) RETURN
C.......................................
C     CHECK DIMENSION AND UNITS OF TIME SERIES A.
      CALL FDCODE(PONEW(9),UNEW,DIMNEW,MSG,NPDT,TSCALE,NADD,IERR)
      CALL FDCODE(POLD(9),UOLD,DIMOLD,MSG,NPDT,TSCALE,NADD,IERR)
      IF((DIMNEW.EQ.DIMOLD).AND.(UNEW.EQ.UOLD)) GO TO 100
      RETURN
C.......................................
C     OLD CARRYOVER VALUE CAN BE USED.
  100 CONEW(1)=COLD(1)
C.......................................
      RETURN
      END
