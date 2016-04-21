C MEMBER TAB38
C  (from old member FCTAB38)
C
      SUBROUTINE TAB38(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C........................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE
C       FOR THE BASEFLOW OPERATION.
C........................................
C     WRITTEN BY ERIC ANDERSON--HRL MARCH 1987
C........................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab38.f,v $
     . $',                                                             '
     .$Id: tab38.f,v 1.1 1995/09/17 18:49:21 dws Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H8   /
C
C........................................
C     TRACE LEVEL=1--DEBUG FLAG=IBUG
      CALL FPRBUG(SNAME,1,38,IBUG)
C
C........................................
C     INITIAL VALUES
      LWORK=0
C
C........................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T ARRAY
      LENGTH=6
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C
C........................................
C     SPACE IS AVAILABLE--MAKE ENTRIES IN TO( ).
 100  TO(1)=38
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      NPO=PO(12)
      IF(NPO.GT.0) GO TO 105
      TO(4)=0
      GO TO 110
 105  TO(4)=LCO
C
C........................................
C     SET COMPUTATIONAL TIME INTERVAL
 110  IDT=PO(6)
C........................................
C     GET LOCATION OF REQUIRED BASEFLOW TIME SERIES
      CALL FINDTS(PO(3),PO(5),IDT,LD,LTS,DIM)
      TO(5)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C
C........................................
C     LOCATION OF RECESSION COEFFICIENT TIME SERIES, IF NEEDED
      IOPT=PO(9)
      IF(IOPT.EQ.2) GO TO 115
      TO(6)=0
      GO TO 120
 115  IBFR=24
      CALL CKINPT(PO(18),PO(20),IBFR,LD,TS,MTS,IERR)
      TO(6)=LD
 120  IUSET=LENGTH
C
C     ALL ENTRIES HAVE BEEN MADE.
C........................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(I),I=1,IUSET)
 900  FORMAT(1H0,'TAB35 DEBUG--CONTENTS OF TO( )=',15I6)
C
C........................................
C     TO ARRAY ENTRIES FOR THE BASEFLOW OPERATION.
C     POSITION     CONTENTS
C        1         ID NUMBER OF OPERATION=38
C        2         LOCATION OF NEXT OPERATION IN T ARRAY
C        3         LOCATION OF PARAMETERS IN P ARRAY-LPO
C        4         LOCATION OF CARRYOVER IN C ARRAY
C                  (=0,IF PO(12)=0--=LCO,IF PO(12).GT.0)
C        5         LOCATION TO PUT BASEFLOW IN D ARRAY
C        6         LOCATION OF RECESSION COEFFICIENTS IN D ARRAY
C                  (=0,IF IOPT.NE.2)
C........................................
 199  CONTINUE
      RETURN
      END
