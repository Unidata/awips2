C MEMBER COX42
C  (from old member FCCOX42)
C
      SUBROUTINE COX42(POLD,COLD,PONEW,CONEW)
C.......................................
C  CARRYOVER TRANSFER FOR THE 'RSNWELEV' OPERATION
C.......................................
C  INITIALLY WRITTEN BY ERIC ANDERSON - HRL   DEC 1991
C.......................................
      DIMENSION POLD(1),COLD(1),PONEW(1),CONEW(1),SNAME(2)
C   COMMON BLOCK
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox42.f,v $
     . $',                                                             '
     .$Id: cox42.f,v 1.1 1995/09/17 18:47:25 dws Exp $
     . $' /
C    ===================================================================
C
C  DATA STATEMENT
      DATA SNAME/4HCOX4,4H2   /
C.......................................
C   TRACE LEVEL=1, DEBUG SWITCH = IBUG
      CALL FPRBUG(SNAME,1,42,IBUG)
C.......................................
C   IF DEBUG ON, PRINT OLD CARRYOVER IF ANY EXISTS
      IF (IBUG.EQ.0) GO TO 100
      WRITE(IODBUG,900) COLD(1)
  900 FORMAT(1H0,'RSNWELEV OPERATION -- OLD CARRYOVER=',F10.2)
C.......................................
C  TRANSFER CARRYOVER -- NO ADJUSTMENT
  100 CONEW(1)=COLD(1)
C.......................................
C  IF DEBUG ON, PRINT NEW CARRYOVER
      IF (IBUG.EQ.0) GO TO 190
      WRITE(IODBUG,901) CONEW(1)
  901 FORMAT(1H0,'RSNWELEV OPERATION -- NEW CARRYOVER=',F10.2)
C.......................................
  190 IF (ITRACE.GE.1) WRITE(IODBUG,902)
  902 FORMAT(1H0,'** EXIT COX42')
      RETURN
      END
