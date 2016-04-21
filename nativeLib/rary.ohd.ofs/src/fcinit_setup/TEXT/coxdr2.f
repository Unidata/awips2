C MEMBER COXDR2
C  (from old member FCCOXDR2)
C
      SUBROUTINE COXDR2(POLD,COLD,PONEW,CONEW,D,MD,NUMOP,IERR)
C.......................................
C  THIS SUBROUTINE CALLS THE CARRYOVER TRANSFER DRIVER FOR
C
C      OPERATIONS 20 THRU 38.
C.......................................
C  SUBROUTINE WRITTEN BY...
C
C     ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),D(MD)
C
C
C  COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/coxdr2.f,v $
     . $',                                                             '
     .$Id: coxdr2.f,v 1.2 1996/05/07 10:49:53 page Exp $
     . $' /
C    ===================================================================
C
C.......................................
C  TRACE LEVEL=1
C
      IF(ITRACE.GE.1)WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** COXDR2 ENTERED)
C.......................................
C  CHECK THAT A CALL TO THE OPERATION IS INCLUDED.
C
      IF ((NUMOP.GT.19).AND.(NUMOP.LT.39)) GO TO 100
      GO TO 190
C.......................................
C  GO TO THE SECTION FOR THE CURRENT OPERATION.
C
  100 NUM=NUMOP-19
      GO TO (200,201,202,203,204,190,206,190,190,209,
     1       190,211,190,213,214,215,216,190,218)     , NUM
C.......................................
C  CHANGE TIME INTERVAL OPERATION.
C
  200 CALL COX20(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  DWOPER OPERATION.
C
  201 CALL COX21(POLD,POLD,COLD,PONEW,PONEW,CONEW)
      GO TO 199
C.......................................
C  HFS OPERATION.
C
  202 CALL COX22(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  STAGE-DISCHARGE CONVERSION.
C
  203 CALL COX23(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C   API-CONT OPERATION
C
  204 CALL COX24(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  RESERVOIR OPERATION
C
  206 CALL COX26(POLD,COLD,PONEW,CONEW,D(1),MD)
      GO TO 199
C.......................................
C  KANSAS CITY API OPERATION
C
 209  CALL COX29(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  SNOW-43 OPERATION
C
 211  CALL COX31(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  CINCINNATI API OPERATION
C
  213 CALL COX33(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  SALT LAKE CITY API OPERATION
C
  214 CALL COX34(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  HARRISBURG RFC API OPERATION
C
  215 CALL COX35(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  XINANJIANG MODEL
C
  216 CALL COX36(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  BASEFLOW OPERATION
C
 218  CALL COX38(POLD,COLD,PONEW,CONEW)
      GO TO 199
C.......................................
C  NO CARRYOVER TRANSFER ROUTINE
C
  190 IERR=1
  199 CONTINUE
      RETURN
      END
