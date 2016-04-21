C MEMBER TAB27
C  (from old member FCTAB27)
C
      SUBROUTINE TAB27(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,
C                             LAST UPDATE: 01/03/95.10:05:04 BY $WC21DT
C
     1  NDD, LWORK, IDT)
C........................................
C     THIS IS THE OPERATION TABLE ENTRY ROUTINE FOR THE
C     'LIST-FTW' OPERATION.
C........................................
C     INITIALLY WRITTEN BY...ERIC ANDERSON-HRL JULY 1987
C........................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON /FDBUG/ IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab27.f,v $
     . $',                                                             '
     .$Id: tab27.f,v 1.1 1995/09/17 18:49:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB2,4H7   /
      DATA BLANK/4H    /
C........................................
C     TRACE LEVEL=1 -- DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,27,IBUG)
C........................................
C     OBTAIN CONTROL VARIABLES
      NTS=PO(4)
      LENGTH=NTS+5
      LWORK=0
      NTITL=PO(3)
      IDT=1
      MINDT=24
C........................................
C     CHECK TO SEE IF SPACE AVAILABLE
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C........................................
C     SPACE AVAILABLE--MAKE ENTRIES
  100 TO(1)=27
      TO(2)=NXT+LENGTH
      TO(3)=LPO
C
C     RATING CURVE ID LOCATION, =0 IF NONE USED.
      IF((PO(10).EQ.BLANK).AND.(PO(11).EQ.BLANK)) GO TO 101
C     RATING CURVE IS USED
      TO(4)=LPO+9
      GO TO 105
C     RATING CURVE NOT USED
  101 TO(4)=0
C
C     FIND LOCATION OF EACH TIME SERIES.
  105 IF (NTS.GT.0) GO TO 106
      IUSET=0
      IDT=0
      RETURN
  106 L=30+NTITL*18
      DO 107 N=1,NTS
      J=L+(N-1)*18+1
      ITI=PO(J+3)
      ITPI=PO(J+4)
      CALL FCLCD(IDT,ITPI)
      IF (ITI.LT.MINDT) MINDT=ITI
      CALL CKINPT(PO(J),PO(J+2),ITI,LD,TS,MTS,IERR)
      TO (5+N)=LD
  107 CONTINUE
      TO(5)=MINDT
C     ALL ENTRIES HAVE BEEN MADE
      IUSET=LENGTH
C........................................
C     CHECK FOR DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,900) IDT,LWORK
  900 FORMAT(1H0,33HLIST-FTW DEBUG--COMPUTATIONAL DT=,I3,
     1  5X,6HLWORK=,I6/6X,20HCONTENTS OF TO ARRAY)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT (1H ,20I6)
C........................................
C     CONTENTS OF TO ARRAY
C     POSITION                   CONTENTS
C        1              OPERATION NUMBER=27
C        2              LOCATION OF NEXT OPERATION IN T ARRAY.
C        3              LOCATION OF PARAMETERS IN P ARRAY-LPO.
C        4              LOCATION OF RATING CURVE IN P ARRAY.
C                        (=0 IF NONE USED)
C        5              MINIMUM TIME INTERNAL OF ANY TIME SERIES.
C      6 TO             LOCATION OF EACH TIME SERIES TO BE TABULATED
C     NTS + 5              IN THE D ARRAY.
C........................................
  199 CONTINUE
      RETURN
      END
