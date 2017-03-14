C MEMBER TAB42
C  (from old member FCTAB42)
C
      SUBROUTINE TAB42(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C.......................................
C    THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C    'RSNWELEV' OPERATION.
C.......................................
C    INITIALLY WRITTEN BY ERIC ANDERSON -- HRL DEC. 1991
C.......................................
      INTEGER TO(1)
      DIMENSION TS(MTS),PO(1),SNAME(2)
C   COMMON BLOCK
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab42.f,v $
     . $',                                                             '
     .$Id: tab42.f,v 1.1 1995/09/17 18:49:23 dws Exp $
     . $' /
C    ===================================================================
C
C    DATA STATEMENT
      DATA SNAME/4HTAB4,4H2   /
C.......................................
C    TRACE LEVEL=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,42,IBUG)
C.......................................
C   INITIAL VALUE
      LENGTH=7
      LWORK=0
      IZIN=PO(12)
C.......................................
C    CHECK IF SPACE AVAIALBLE IN T()
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C    SPACE IS AVAIALABLE -- MAKE ENTRIES
  100 TO(1)=42
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
      IF (IZIN.EQ.0) TO(4)=0
C.......................................
C    TEMPERATURE TIME SERIES
      IDT=PO(2)
      CALL CKINPT(PO(3),PO(5),IDT,LD,TS,MTS,IERR)
      TO(5)=LD
C.......................................
C     RAIN-SNOW ELEVATION TIME SERIES
      CALL FINDTS(PO(9),PO(11),IDT,LD,LTS,DIM)
      TO(6)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
C.......................................
C    FREEZING LEVEL TIME SERIES
      IF (IZIN.EQ.1) GO TO 110
      TO(7)=0
      GO TO 120
  110 CALL CKINPT(PO(13),PO(15),IDT,LD,TS,MTS,IERR)
      TO(7)=LD
  120 IUSET=LENGTH
C    ALL ENTRIES HAVE BEEN MADE
C.......................................
C    DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 190
      WRITE(IODBUG,900) IDT,LWORK
  900 FORMAT(1H0,'RSNWELEV-CONTENTS OF TO.',5X,'DT=',I3,5X,
     -'LWORK=',I6)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)
C.......................................
  190 IF (ITRACE.GE.1) WRITE(IODBUG,902)
  902 FORMAT(1H0,'** EXIT TAB42')
      RETURN
      END
