C MEMBER TAB39
C  (from old member FCTAB39)
C
      SUBROUTINE TAB39(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,NDD,
     *LWORK,IDT)
C........................................
C     THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C       FORT WORTH TABLE LOOKUP OPERATION - 'LOOKUP'
C........................................
C     WRITTEN BY ERIC ANDERSON - HRL JULY 1987
C........................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab39.f,v $
     . $',                                                             '
     .$Id: tab39.f,v 1.1 1995/09/17 18:49:22 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H9   /
C........................................
C     TRACE LEVEL=1--DEBUG SWITHCH=IBUG
      CALL FPRBUG(SNAME,1,39,IBUG)
C........................................
C     OBTAIN CONTROL VARIABLES
      LENGTH=5
      LWORK=0
      IDT=PO(14)
C........................................
C     CHECK TO SEE IF SPACE IS AVAILABLE
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C........................................
C     SPACE AVAILABE--MAKE ENTRIES
  100 TO(1)=39
      TO(2)=NXT+LENGTH
      TO(3)=LPO
C
C     LOCATION OF INPUT TIME SERIES
      CALL CKINPT(PO(11),PO(13),IDT,LD,TS,MTS,IERR)
      TO(4)=LD
C
C     LOCATION TO PUT OUTPUT TIME SERIES
      CALL FINDTS(PO(15),PO(17),IDT,LD,LTS,DIM)
      TO(5)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C     ALL ENTRIES HAVE BEEN MADE
      IUSET=LENGTH
C........................................
C     CHECK FOR DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) IDT,LWORK
  900 FORMAT(1H0,31HLOOKUP DEBUG-COMPUTATIONAL DT= ,I3,
     *5X,6HLWORK=,I6,/6X,20HCONTENTS OF TO ARRAY)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)
C........................................
C     CONTENTS OF TO ARRAY
C     POSITION                CONTENTS
C        1              OPERATION NUMBER=39.
C        2              LOCATION OF NEXT OPERATION IN T ARRAY.
C        3              LOCATION OF PARAMETERS IN P ARRAY-LPO.
C        4              LOCATION OF INPUT TIME SERIES IN D ARRAY.
C        5              LOCATION TO PUT OUTPUT TIME SERIES IN D ARRAY.
C........................................
  199 CONTINUE
      RETURN
      END
