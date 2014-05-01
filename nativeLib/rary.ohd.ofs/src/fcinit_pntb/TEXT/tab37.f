C MEMBER TAB37
C  (from old member FCTAB37)
C
      SUBROUTINE TAB37(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,NDD,
     *LWORK,IDT)
C........................................
C     THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C     'LIST-MSP' OPERATION.
C........................................
C     WRITTEN BY ERIC ANDERSON-HRL JULY 1987
C........................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO (1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab37.f,v $
     . $',                                                             '
     .$Id: tab37.f,v 1.1 1995/09/17 18:49:20 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H7   /
C........................................
C     TRACE LEVEL=1 --DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,37,IBUG)
C........................................
C     OBTAIN CONTROL VARIABLES.
      LENGTH=12
      LWORK=0
      IDT=24
C........................................
C     CHECK TO SEE IF SPACE AVAILABLE.
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C........................................
C     SPACE AVAILABLE--MAKE ENTRIES
  100 TO(1)=37
      TO(2)=NXT+LENGTH
      TO(3)=LPO
C     PRECIPITATION TIME SERIES
      IT=PO(28)
      CALL CKINPT (PO(25),PO(27),IT,LD,TS,MTS,IERR)
      TO(4)=LD
C     RAIN+MELT AND RUNOFF
      IT=PO(18)
      CALL CKINPT (PO(19),PO(21),IT,LD,TS,MTS,IERR)
      TO(5)=LD
      CALL CKINPT (PO(22),PO(24),IT,LD,TS,MTS,IERR)
      TO(6)=LD
C     WATER-EQUIVALENT
      L=PO(29)
      IF (L.GT.0) GO TO 101
      TO(7)=0
      GO TO 105
  101 IT=PO(L+3)
      CALL CKINPT (PO(L),PO(L+2),IT,LD,TS,MTS,IERR)
      TO(7)=LD
C     API TIME SERIES
  105 L=PO(30)
      IF (L.GT.0) GO TO 106
      TO(8)=0
      GO TO 110
  106 IT=PO(L+3)
      CALL CKINPT (PO(L),PO(L+2),IT,LD,TS,MTS,IERR)
      TO(8)=LD
C     AI/FI TIME SERIES
  110 L=PO(31)
      IF (L.GT.0) GO TO 111
      TO(9)=0
      GO TO 115
  111 IT=PO(L+3)
      CALL CKINPT (PO(L),PO(L+2),IT,LD,TS,MTS,IERR)
      TO(9)=LD
C     AIADJ FACTOR
  115 L=PO(32)
      K=0
      IF (L.GT.0) K=PO(L)
      IF((L.GT.0).AND.(K.GT.7)) GO TO 116
      TO (10)=0
      GO TO 120
  116 TO(10)=K
C     AEI TIME SERIES
  120 IF((L.GT.0).AND.(K.EQ.1)) GO TO 121
      TO(11)=0
      GO TO 125
  121 IT=PO(L+4)
      CALL CKINPT (PO(L+1),PO(L+3),IT,LD,TS,MTS,IERR)
      TO(11)=LD
C     AESC TIME SERIES
  125 IF((L.GT.0).AND.(K.EQ.2)) GO TO 126
      TO(12)=0
      GO TO 130
  126 IT=PO(L+4)
      CALL CKINPT (PO(L+1),PO(L+3),IT,LD,TS,MTS,IERR)
      TO(12)=LD
C     ALL ENTRIES HAVE BEEN MADE
  130 IUSET=LENGTH
C........................................
C     CHECK FOR DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,900) IDT,LWORK
  900 FORMAT (1H0,33HLIST-MSP DEBUG--COMPUTATIONAL DT=,I3,5X,6HLWORK=,
     * I6,/6X,20HCONTENTS OF TO ARRAY)
      WRITE (IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT (1H ,20I6)
C........................................
C     CONTENTS OF TO ARRAY
C     POSITION                   CONTENTS
C        1              OPERATION NUMBER=37
C        2              LOCATION OF NEXT OPERATION IN T ARRAY
C        3              LOCATION PO IN P ARRAY
C        4              LOCATION OF PRECIPITATION IN D ARRAY
C        5              LOCATION OF RAIN+MELT IN D ARRAY
C        6              LOCATION OF RUNOFF IN D ARRAY
C        7              LOCATION OF WATER-EQUIVALENT IN D ARRAY
C                         (=0 IF NOT AVAILABLE)
C        8              LOCATION OF API IN D ARRAY
C                         (=0 IF NOT AVAILABLE)
C        9              LOCATION OF AI/FI IN D ARRAY
C                         (=0 IF NOT AVAILABLE)
C       10              LOCATION OF AIADJ IN P ARRAY
C                         (=0 IF NOT AVAILABLE)
C       11              LOCATION OF AEI IN D ARRAY
C                         (=0 IF NOT AVAILABLE)
C       12              LOCATION OF AESC IN D ARRAY
C                         (=0 IF NOT AVAILABLE)
C........................................
  199 CONTINUE
      RETURN
      END
