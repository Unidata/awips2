C MEMBER TAB17
C  (from old member MCTAB17)
C
      SUBROUTINE TAB17(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,LWORK,
     1  IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'WY-PLOT '
C     OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY . . .
C            ERIC ANDERSON - HRL     APRIL 1980
C.......................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_wyplot/RCS/tab17.f,v $
     . $',                                                             '
     .$Id: tab17.f,v 1.2 1996/07/11 19:46:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H7   /
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,17,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      NPLOT=PO(6)
      LENGTH=10+NPLOT
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 105
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  105 TO(1)=17
      TO (2)=NXT+LENGTH
      TO (3)=LPO
      TO(4)=LCO
      LOC=PO(10)
      IF (LOC.GT.0) GO TO 106
C
C     NO RAIN+MELT TIME SERIES
      TO(5)=0
      TO(6)=0
      IDT=0
      GO TO 110
C
C     RAIN+MELT TIME SERIES IS USED.
  106 IDT=PO(LOC+3)
      CALL CKINPT(PO(LOC),PO(LOC+2),IDT,LD,TS,MTS,IERR)
      IF (IDT.EQ.24) IDT=0
      TO(5)=IDT
      TO(6)=LD
C.......................................
C     RUNOFF AND SOIL-MOISTURE TIME SERIES.
  110 JDT=24
      DO 111 I=1,2
      LOC=PO(I+10)
      IF (LOC.GT.0) GO TO 112
      TO(I+6)=0
      GO TO 111
  112 CALL CKINPT(PO(LOC),PO(LOC+2),JDT,LD,TS,MTS,IERR)
      TO(I+6)=LD
  111 CONTINUE
C
C     LOCATION AND LENGTH OF WORKING SPACE
      TO(9)=NWORK
      LWORK=101+3*NPLOT
      IF (TO(6).EQ.0) GO TO 115
      TO(10)=NWORK+LWORK
      LWORK=LWORK+31
      GO TO 119
  115 TO(10)=0
  119 CONTINUE
C.......................................
C     TIME SERIES TO BE PLOTTED.
      JDT=24
      DO 120 N=1,NPLOT
      LOC=15+(N-1)*7
      CALL CKINPT(PO(LOC),PO(LOC+2),JDT,LD,TS,MTS,IERR)
      TO(10+N)=LD
  120 CONTINUE
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,903)
  903 FORMAT (1H0,33HTAB5 DEBUG--CONTENTS OF TO ARRAY.)
      WRITE (IODBUG,904) (TO(I),I=1,IUSET)
  904 FORMAT (1H ,20I6)
C.......................................
C     THE CONTENTS OF THE TO ARRAY ARE AS FOLLOWS.
C     POSITION                     CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=17
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY
C        4.        LOCATION OF CARRYOVER IN THE C ARRAY.
C        5.        DELTA T FOR THE OPERATION
C        6.        LOCATION OF RAIN+MELT IN D ARRAY
C        7.        LOCATION OF RUNOFF COMPONENTS IN D ARRAY
C        8.        LOCATION OF SOIL-MOISTURE STORAGES IN D ARRAY
C        9.        LOCATION OF WORKING SPACE FOR PLOTTING
C       10.        LOCATION OF WORKING SPACE FOR DAILY PRECIPITATION
C       11.        LOCATION OF EACH TIME SERIES TO BE PLOTTED IN
C     NPLOT+10       THE D ARRAY (NPLOT=NO.OF T.S. PLOTTED)
C.......................................
  199 CONTINUE
      RETURN
      END
