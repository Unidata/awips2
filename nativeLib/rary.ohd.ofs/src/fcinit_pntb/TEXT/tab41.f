C MEMBER TAB41
C  (from old member FCTAB41)
C
      SUBROUTINE TAB41(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C         HARRISBURG RFC API OPERATION -- 'API-HAR2'
C.......................................
C     WRITTEN BY -- ERIC ANDERSON -- HRL   NOVEMBER 1991
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab41.f,v $
     . $',                                                             '
     .$Id: tab41.f,v 1.1 1995/09/17 18:49:23 dws Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB4,4H1   /
C.......................................
C     CHECK FOR DEBUG OUTPUT--TRACE LEVEL=1
      CALL FPRBUG(SNAME,1,41,IBUG)
C.......................................
C     INITIAL VALUES
      LWORK=0
C.......................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T ARRAY.
      LENGTH=10
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE - MAKE ENTRIES IN TO( ).
  100 TO(1)=41
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C.......................................
C     GET TIME SERIES TIME INTERVAL AND SET MINIMUM RUN INTERVAL
      IDELTA=PO(26)
      IDT=24
C.......................................
C     GET LOCATION OF RAIN+MELT
      CALL CKINPT(PO(32),PO(34),IDELTA,LD,TS,MTS,IERR)
      TO(5)=LD
C     GET LOCATION OF PE
      IT=24
      CALL CKINPT(PO(35),PO(37),IT,LD,TS,MTS,IERR)
      TO(6)=LD
C     LOCATION OF RUNOFF TIME SERIES
      CALL FINDTS(PO(38),PO(40),IDELTA,LD,LTS,DIM)
      TO(7)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C.......................................
C     LOCATION OF OPTIONAL TIME SERIES
      IOFAAA=PO(30)
      IF(IOFAAA.GT.0) GO TO 110
      TO(8)=0
      TO(9)=0
      TO(10)=0
      GO TO 120
C     ANTECEDENT INDEX(AI)
  110 CALL FINDTS(PO(41),PO(43),IDELTA,LD,LTS,DIM)
      TO(8)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C     ANTECEDENT PRECIPITATION INDEX (API)
      CALL FINDTS(PO(44),PO(46),IDELTA,LD,LTS,DIM)
      TO(9)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C     ANTECEDENT EVAPORATION INDEX (AEI)
      CALL FINDTS(PO(47),PO(49),IDELTA,LD,LTS,DIM)
      TO(10)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
  120 IUSET=LENGTH
C     ALL ENTRIES HAVE BEEN MADE
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)(TO(I),I=1,IUSET)
  900 FORMAT(1H0,31HTAB41 DEBUG--CONTENTS OF TO( )=,15I6)
C.......................................
C     TO ARRAY ENTRIES FOR THE 'API-HAR' OPERATION.
C     POSITION    CONTENTS
C        1.    ID NUMBER FOR OPERATION=41
C        2.    LOCATION OF NEXT OPERATION IN T ARRAY
C        3.    LOCATION OF PARAMETERS IN P ARRAY-LPO
C        4.    LOCATION OF CARRYOVER IN C ARRAY-LCO
C        5.    LOCATION OF RAIN+MELT IN D ARRAY
C        6.    LOCATION OF PE IN D ARRAY
C        7.    LOCATION TO PUT RUNOFF IN D ARRAY
C        8.    LOCATION TO PUT AI IN D ARRAY
C                 (=0 IF NOT USED)
C        9.    LOCATION TO PUT API IN D ARRAY
C                 (=0 IF NOT USED)
C       10.    LOCATION TO PUT AEI IN D ARRAY
C                 (=0 IF NOT USED)
C.......................................
  199 CONTINUE
      RETURN
      END
