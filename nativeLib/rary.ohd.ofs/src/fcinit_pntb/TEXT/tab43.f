C MEMBER TAB43
C
      SUBROUTINE TAB43(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C         HARTFORD RFC API OPERATION -- 'API-HFD'
C.......................................
C     WRITTEN BY -- TIM SWEENEY -- HRL SEPTEMBER 1995
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab43.f,v $
     . $',                                                             '
     .$Id: tab43.f,v 1.1 1998/01/29 20:10:16 dws Exp $ 
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB4,4H3   /
c
C............................................
C     CHECK FOR DEBUG OUTPUT--TRACE LEVEL=1
      CALL FPRBUG(SNAME,1,43,IBUG)
C
C............................................
c     GET CONTROL VARIABLES
C............................................
      IDT = 24
      IDELTA = PO(24)
      IOFAAA = PO(28)
      LMTS   = PO(32)
      LWKT   = PO(33)
      LOTS   = PO(34)
C
C............................................
C     INITIAL VALUES
C............................................
      LWORK  = 0
      LENGTH = 9
C
C....................................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T ARRAY.
C....................................................
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET = 0
      IDT = 0
      RETURN
C
C...................................................
C     SPACE IS AVAILABLE - MAKE ENTRIES IN TO( ).
C...................................................
  100 TO(1) = 43
      TO(2) = NXT + LENGTH
      TO(3) = LPO
      TO(4) = LCO
C
C.......................................
C     GET LOCATION OF RAIN+MELT
C.......................................
      CALL CKINPT(PO(LMTS),PO(LMTS+2),IDELTA,LD,TS,MTS,IERR)
      TO(5) = LD
C
C........................................
C     LOCATION OF RUNOFF TIME SERIES
C........................................
      CALL FINDTS(PO(LMTS+3),PO(LMTS+5),IDELTA,LD,LTS,DIM)
      TO(6) = LD
      IF(LTS.GT.0) TS(LTS+8) = 1.01
C
C............................................
C     LOCATION OF OPTIONAL TIME SERIES
C............................................
      IF(IOFAAA.GT.0) GO TO 110
      TO(7) = 0
      TO(8) = 0
      TO(9) = 0
      GO TO 120
C
C..............................................
C     ANTECEDENT PRECIPITATION INDEX (API)
C..............................................
  110 CALL FINDTS(PO(LOTS),PO(LOTS+2),IDELTA,LD,LTS,DIM)
      TO(7) = LD
      IF(LTS.GT.0) TS(LTS+8) = 1.01
C
C.............................................
C     ANTECEDENT TEMPERATURE INDEX (ATI)
C.............................................
      CALL FINDTS(PO(LOTS+3),PO(LOTS+5),IDT,LD,LTS,DIM)
      TO(8) = LD
      IF(LTS.GT.0) TS(LTS+8) = 1.01
C
C...................................................................
C     RUNOFF INDEX (RI) [SAME AS FINAL INDEX (FI) IN OTHER MODELS]
C...................................................................
      CALL FINDTS(PO(LOTS+6),PO(LOTS+8),IDELTA,LD,LTS,DIM)
      TO(9) = LD
      IF(LTS.GT.0) TS(LTS+8) = 1.01
C
  120 IUSET = LENGTH
C     ALL ENTRIES HAVE BEEN MADE
C
C.......................................
C     DEBUG OUTPUT
C.......................................
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)(TO(I),I=1,IUSET)
  900 FORMAT(1H0,31HTAB43 DEBUG--CONTENTS OF TO( )=,15I6)
C
C.......................................
C     TO ARRAY ENTRIES FOR THE 'API-HFD' OPERATION.
C     POSITION    CONTENTS
C        1.    ID NUMBER FOR OPERATION=43
C        2.    LOCATION OF NEXT OPERATION IN T ARRAY
C        3.    LOCATION OF PARAMETERS IN P ARRAY-LPO
C        4.    LOCATION OF CARRYOVER IN C ARRAY-LCO
C        5.    LOCATION OF RAIN+MELT IN D ARRAY
C        6.    LOCATION TO PUT RUNOFF IN D ARRAY
C        7.    LOCATION TO PUT API IN D ARRAY
C                 (=0 IF NOT USED)
C        8.    LOCATION TO PUT ATI IN D ARRAY
C                 (=0 IF NOT USED)
C        9.    LOCATION TO PUT RI IN D ARRAY
C                 (=0 IF NOT USED)
C.......................................
  199 CONTINUE
      RETURN
      END
