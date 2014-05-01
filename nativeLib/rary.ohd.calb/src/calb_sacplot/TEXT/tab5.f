C MEMBER TAB5
C  (from old member MCTAB5)
C
      SUBROUTINE TAB5(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,P,MP,NWORK,LWORK)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'SAC-PLOT'
C     OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY . . .
C            ERIC ANDERSON - HRL     SEPTEMBER 1979
C.......................................
      DIMENSION TS(MTS),PO(1),P(MP)
      INTEGER TO(1)
C
C     COMMON BLOCKS
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_sacplot/RCS/tab5.f,v $
     . $',                                                             '
     .$Id: tab5.f,v 1.2 1996/07/11 19:47:35 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** TAB5 ENTERED)
C
C     CHECK IF DEBUG OUTPUT IS NEEDED.
      IBUG=0
      IF (IDBALL.GT.0) IBUG=1
      IF (NDEBUG.EQ.0) GO TO 100
      DO 10 I=1,NDEBUG
      IF (IDEBUG(I).EQ.5) GO TO 11
   10 CONTINUE
      GO TO 100
   11 IBUG=1
C.......................................
C     OBTAIN CONTROL VARIABLES.
  100 NPLOT=PO(6)
      NSAC=0
      I=PO(13)
      IF (I.GT.0) NSAC=PO(I)
      NSNOW=0
      I=PO(14)
      IF (I.GT.0) NSNOW=PO(I)
      LENGTH=10+NPLOT+NSAC+NSNOW
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      IF (LEFT.GE.LENGTH) GO TO 105
      WRITE (IPR,901)
  901 FORMAT (1H0,10X,75H**ERROR** THIS OPERATION NEEDS MORE SPACE THAN 
     1IS AVAILABLE IN THE T ARRAY.)
      CALL ERROR
      IUSET=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  105 TO(1)=5
      TO (2)=NXT+LENGTH
      TO (3)=LPO
      LOC=PO(10)
      IF (LOC.GT.0) GO TO 106
C
C     NO RAIN+MELT TIME SERIES
      TO(4)=0
      TO(5)=0
      GO TO 110
C
C     RAIN+MELT TIME SERIES IS USED.
  106 IDT=PO(LOC+3)
      CALL FINDTS(PO(LOC),PO(LOC+2),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF (ICK.GT.0) GO TO 107
      WRITE (IPR,902) PO(LOC),PO(LOC+1),PO(LOC+2),IDT
  902 FORMAT (1H0,10X,74H**ERROR** NO VALUES HAVE PREVIOUSLY BEEN ASSIGN
     1ED TO THE INPUT TIME SERIES,/16X,7H( I.D.=,2A4,3X,5HTYPE=,A4,3X,
     23HDT=,I2,1X,6HHOURS),1X,19HFOR THIS OPERATION.)
      CALL ERROR
  107 IF (IDT.EQ.24) IDT=0
      TO(4)=IDT
      TO(5)=LD
C.......................................
C     RUNOFF AND SOIL-MOISTURE TIME SERIES.
  110 IDT=24
      DO 111 I=1,2
      LOC=PO(I+10)
      IF (LOC.GT.0) GO TO 112
      TO(I+5)=0
      GO TO 111
  112 CALL FINDTS (PO(LOC),PO(LOC+2),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF (ICK.GT.0) GO TO 114
      WRITE (IPR,902) PO(LOC),PO(LOC+1),PO(LOC+2),IDT
      CALL ERROR
  114 TO(I+5)=LD
  111 CONTINUE
C
C     LOCATION AND LENGTH OF WORKING SPACE
      TO(8)=NWORK
      LWORK=101+3*NPLOT
C.......................................
C     SAC-SMA AND SNOW MODEL OPERATIONS.
      LT=NPLOT+10
      DO 115 I=1,2
      LOC=PO(I+12)
      IF (LOC.GT.0) GO TO 116
      TO(I+8)=0
      GO TO 115
  116 NUM=PO(LOC)
      TO(I+8)=LT+1
      NUMOP=1
      IF (I.EQ.2) NUMOP=19
      DO 117 N=1,NUM
      J=LOC+(N-1)*3+1
      LP=0
      CALL FSERCH (NUMOP,PO(J),LP,P,MP)
      TO(LT+N)=LP
  117 CONTINUE
      LT=LT+NUM
  115 CONTINUE
C.......................................
C     TIME SERIES TO BE PLOTTED.
      IDT=24
      DO 120 N=1,NPLOT
      LOC=15+(N-1)*7
      CALL FINDTS(PO(LOC),PO(LOC+2),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF (ICK.GT.0) GO TO 121
      WRITE (IPR,902) PO(LOC),PO(LOC+1),PO(LOC+2),IDT
      CALL ERROR
  121 TO(10+N)=LD
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
C        1.        I.D. NUMBER FOR THE OPERATION=5
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY
C        4.        DELTA T FOR THE OPERATION
C        5.        LOCATION OF RAIN+MELT IN D ARRAY
C        6.        LOCATION OF RUNOFF COMPONENTS IN D ARRAY
C        7.        LOCATION OF SOIL-MOISTURE STORAGES IN D ARRAY
C        8.        LOCATION OF WORKING SPACE IN THE D ARRAY
C        9.        LOCATION OF SAC-SMA P ARRAY LOCATIONS IN THE T ARRAY
C       10.        LOCATION OF SNOW OPERATION P ARRAY LOCATIONS IN THE T
C                     ARRAY
C       11.        LOCATION OF EACH TIME SERIES TO BE PLOTTED IN
C     NPLOT+10       THE D ARRAY (NPLOT=NO.OF T.S. PLOTTED)
C.......................................
  199 CONTINUE
      RETURN
      END
