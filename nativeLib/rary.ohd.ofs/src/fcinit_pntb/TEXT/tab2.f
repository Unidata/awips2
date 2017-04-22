C MEMBER TAB2
C  (from old member FCTAB2)
C
      SUBROUTINE TAB2(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,NWORK,NDD,
     1   LWORK)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C        UNIT HYDROGRAPH OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     AUGUST 1979
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab2.f,v $
     . $',                                                             '
     .$Id: tab2.f,v 1.1 1995/09/17 18:49:08 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** TAB2 ENTERED)
C.......................................
C     CHECK IF DEBUG OUTPUT IS NEEDED.
      IBUG=0
      IF (IDBALL.GT.0) IBUG=1
      IF (NDEBUG.EQ.0) GO TO 101
      DO 10 I=1,NDEBUG
      IF (IDEBUG(I).EQ.2) GO TO 11
   10 CONTINUE
      GO TO 101
   11 IBUG=1
C.......................................
C     INITIAL VALUES
  101 ISKIPT=0
C.......................................
C     CHECK TO SEE IF THE DISCHARGE TIME SERIES
C        HAS BEEN USED PREVIOUSLY OR NEEDS TO BE CLEARED.
      IDT=PO(20)
      CALL FINDTS(PO(17),PO(19),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF (ICK.GT.0) GO TO 102
C
C     DISCHARGE TIME SERIES NEEDS TO BE CLEARED.
      CALL CLEAR(PO(17),PO(19),IDT,LD,LTS,TO,LEFT,ISKIPT,NXT,TS,MTS)
C.......................................
C     CHECK TO SEE IF 8 SPACES ARE AVAILABLE IN T().
  102 IF (LEFT.GE.8) GO TO 100
      WRITE (IPR,901)
  901 FORMAT (1H0,10X,75H**ERROR** THIS OPERATION NEEDS MORE SPACE THAN
     1IS AVAILABLE IN THE T ARRAY.)
      CALL ERROR
      IUSET=ISKIPT
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO ().
  100 TO(ISKIPT+1)=2
      TO(ISKIPT+2)=NXT+ISKIPT+8
      TO(ISKIPT+3)=LPO
      NEEDC=PO(21)
      IF (NEEDC.GT.0) GO TO 104
      TO(ISKIPT+4)=0
      GO TO 105
  104 TO(ISKIPT+4)=LCO
C
C     STORE LOCATION OF DISCHARGE TIME SERIES.
  105 TO(ISKIPT+6)=LD
      IDTQ=IDT
C
C     STORE LOCATION OF RUNOFF TIME SERIES.
      IDT=PO(16)
      CALL FINDTS(PO(13),PO(15),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF (ICK.GT.0) GO TO 103
      WRITE (IPR,902) (PO(I),I=13,15),IDT
  902 FORMAT (1H0,10X,74H**ERROR** NO VALUES HAVE PREVIOUSLY BEEN ASSIGN
     1ED TO THE INPUT TIME SERIES,/16X,7H( I.D.=,2A4,3X,5HTYPE=,A4,3X,
     23HDT=,I2,1X,6HHOURS),1X,19HFOR THIS OPERATION.)
      CALL ERROR
  103 TO(ISKIPT+5)=LD
C
C     STORE LOCATION OF WORKING SPACE AND COMPUTE LENGTH.
      TO(ISKIPT+7)=NWORK
      LWORK=NDD*(24/IDTQ)
      NCO=PO(21)
      TO(ISKIPT+8)=NWORK+LWORK
      LWORK=LWORK+NCO
C
      IUSET=ISKIPT+8
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,903) (TO(ISKIPT+I),I=1,8)
  903 FORMAT (1H0,30HUNIT-HG DEBUG--CONTENTS OF TO=,8I6)
C.......................................
C     THE TO ARRAY ENTRIES FOR THE UNIT HYDROGRAPH OPERATION
C        ARE AS FOLLOWS. . .
C        POSITION                   CONTENTS
C          1.          I.D. NUMBER FOR THE OPERATION=2
C          2.          LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C          3.          LOCATION OF PARAMETERS IN THE P ARRAY=LPO
C          4.          LOCATION OF CARRYOVER IN THE C ARRAY=LCO
C                           =0 IF NONE NEEDED.
C          5           LOCATION OF RUNOFF DATA IN THE D ARRAY.
C          6.          LOCATION OF DISCHARGE DATA IN THE D ARRAY.
C          7.          LOCATION OF WORKING SPACE FOR Q VALUES.
C          8.          LOCATION OF WORKING SPACE FOR TEMPORARY
C                           CARRYOVER VALUES.
C.......................................
  199 CONTINUE
      RETURN
      END
