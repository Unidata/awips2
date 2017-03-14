C MEMBER COXDR1
C  (from old member FCCOXDR1)
C
      SUBROUTINE COXDR1(POLD,COLD,PONEW,CONEW,D,MD,NUMOP,IERR)
C.......................................
C     THIS IS THE PORTION OF THE CARRYOVER TRANSFER DRIVER FOR
C     OPERATIONS 1 THRU 19
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C       ERIC ANDERSON - HRL  DECEMBER 1980
C.......................................
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),D(MD)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/coxdr1.f,v $
     . $',                                                             '
     .$Id: coxdr1.f,v 1.1 1995/09/17 18:47:31 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** COXDR1 ENTERED)
C.......................................
C     CHECK THAT CALL IS INCLUDED FOR THE OPERATION
      IF((NUMOP.GT.0).AND.(NUMOP.LT.20)) GO TO 100
      GO TO 190
C.......................................
C     GO TO THE PROPER SECTION FOR THE OPERATION.
  100 GO TO(101,102,190,190,190,106,107,108,109,110,111,190,113,114,
     1  190,190,190,190,119),NUMOP
C.......................................
C     SAC-SMA OPERATION
  101 CALL COX1(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     UNIT-HG OPERATION.
  102 CALL COX2(POLD,COLD,PONEW,CONEW,D(1),MD)
      GO TO 199
C.......................................
C     MEAN-Q OPERATION
  106 CALL COX6(POLD,COLD,PONEW,CONEW,D(1),MD)
      GO TO 199
C.......................................
C     LAG/K OPERATION
  107 CALL COX7(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     CHANLOSS OPERATION
  108 CALL COX8(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     MUSKROUT OPERATION
  109 CALL COX9(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     ADD/SUB OPERATION.
  110 CALL COX10(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     LAY-COEF OPERATION.
  111 CALL COX11(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     TATUM ROUTING
  113 CALL COX13(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     ADJUST-Q OPERATION
  114 CALL COX14(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     SNOW-17 OPERATION
  119 CALL COX19(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C     NO CARRYOVER TRANSFER ROUTINE
  190 IERR=1
  199 CONTINUE
      RETURN
      END
