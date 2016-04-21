C MEMBER COX45
C  (from old member FCCOX45)
C @PROCESS LVL(77)
C
C                             LAST UPDATE: 02/03/94.10:57:46 BY $WC30KH
C
      SUBROUTINE COX45(POLD,COLD,PONEW,CONEW)
C.......................................
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE
C        DELTA-TS OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        KUANG HSU - HRL   DEC. 1993
C.......................................
      DIMENSION POLD(*),COLD(*),PONEW(*),CONEW(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK.
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/cox45.f,v $
     . $',                                                             '
     .$Id: cox45.f,v 1.1 1995/09/17 18:47:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HCOX4,4H5   /
C.......................................
C     TRACE LEVEL FOR SUBROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,45,IBUG)
C
C.......................................................................
C     DEBUG OUTPUT - PRINT POLD() AND COLD(), PONEW() AND CONEW()
      IF(IBUG.EQ.0) GO TO 108
      IUSEP=10
      WRITE(IODBUG,904) (POLD(I),I=1,IUSEP)
  904 FORMAT(1X,'POLD=',F10.2,2X,2A4,1X,A4,F10.2,2X,2A4,1X,A4,2F10.2)
      WRITE(IODBUG,905) COLD(1)
 905  FORMAT(1X,'COLD= ',F10.2)
      WRITE(IODBUG,907) (PONEW(I),I=1,IUSEP)
 907  FORMAT(1X,'PONEW=',F10.2,2X,2A4,1X,A4,F10.2,2X,2A4,1X,A4,2F10.2)
      WRITE(IODBUG,908) CONEW(1)
 908  FORMAT(1X,'CONEW= ',F10.2)
  108 CONTINUE
C.......................................
C     CHECK DIMENSION AND UNITS OF TIME SERIES A.
      CALL FDCODE(PONEW(4),UNEW,DIMNEW,MSG,NPDT,TSCALE,NADD,IERR)
      CALL FDCODE(POLD(4),UOLD,DIMOLD,MSG,NPDT,TSCALE,NADD,IERR)
      IF((DIMNEW.EQ.DIMOLD).AND.(UNEW.EQ.UOLD)) GO TO 100
      WRITE(IPR,901) UOLD,DIMOLD,UNEW,DIMNEW
 901  FORMAT(10X,'** WARNING **  NO CARRYOVER TRANSFERED, OLD AND NEW ',
     1'TIME SERIES DO NOT HAVE SAME UNITS AND DIMENSION CODE',
     2/25X,'UNITS AND DIMENSION CODE OF OLD TIME SERIES=',1X,A4,1X,A4,
     3/25X,'UNITS AND DIMENSION CODE OF NEW TIME SERIES=',1X,A4,1X,A4)
      CALL WARN
      GO TO 101
C.......................................
C     OLD CARRYOVER VALUE CAN BE USED.
  100 CONEW(1)=COLD(1)
  101 IF(IBUG.GE.1) WRITE(IODBUG,911) CONEW(1)
 911  FORMAT(1X,'CONEW= ',F10.2)
C.......................................
      RETURN
      END
