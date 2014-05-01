C MEMBER TAB6
C  (from old member FCTAB6)
C
      SUBROUTINE TAB6(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C     MEAN DISCHARGE (MEAN-Q) OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   OCTOBER 1979
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab6.f,v $
     . $',                                                             '
     .$Id: tab6.f,v 1.1 1995/09/17 18:49:28 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,15H** TAB6 ENTERED)
C.......................................
C     CHECK IF DEBUG OUTPUT IS NEEDED.
      IBUG=0
      IF(IDBALL.GT.0) IBUG=1
      IF(NDEBUG.EQ.0) GO TO 101
      DO 10 I=1,NDEBUG
      IF(IDEBUG(I).EQ.6) GO TO 11
   10 CONTINUE
      GO TO 101
   11 IBUG=1
C.......................................
C     CHECK TO SEE IF 6 SPACES ARE AVAILABLE FOR TO().
  101 IF(LEFT.GE.6) GO TO 100
      WRITE(IPR,901)
  901 FORMAT(1H0,10X,75H**ERROR** THIS OPERATION NEEDS MORE SPACE THAN I
     1S AVAILABLE IN THE T ARRAY.)
      CALL ERROR
      IUSET=0
      RETURN
C.......................................
C     SPACE IS AVAILABEL--MAKE ENTRIES INTO TO().
  100 TO(1)=6
      TO(2)=NXT+6
      TO(3)=LPO
      TO(4)=LCO
C
C     FIND LOCATION OF INSTANTANEOUS Q TIME SERIES.
      IDT=PO(5)
      CALL FINDTS(PO(2),PO(4),IDT,LD,LTS,DIM)
      ICK=TS(LTS+8)
      IF(ICK.GT.0) GO TO 102
      WRITE(IPR,902) (PO(I),I=2,4),IDT
  902 FORMAT(1H0,10X,74H**ERROR** NO VALUES HAVE PREVIOUSLY BEEN ASSIGNE
     1D TO THE INPUT TIME SERIES,/16X,7H( I.D.=,2A4,3X,5HTYPE=,A4,3X,3HD
     2T=,I2,1X,6HHOURS),1X,19HFOR THIS OPERATION.)
      CALL ERROR
  102 TO(5)=LD
C
C     MEAN Q TIME SERIES
      IDT=PO(9)
      CALL FINDTS(PO(6),PO(8),IDT,LD,LTS,DIM)
      TO(6)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
      IUSET=6
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,903) (TO(I),I=1,6)
  903 FORMAT(1H0,30HMEAN-Q DEBUG--T ARRAY ENTRIES=,6I6)
C.......................................
C     THE T ARRAY ENTRIES FOR MEAN-Q OPERATION ARE.
C     POSITION                   DESCRIPTION
C        1.      I.D. NUMBER FOR THE OPERATION=6
C        2.      LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.      LOCATION OF PARAMETERS IN THE P ARRAY=LPO
C        4.      LOCATION OF CARRYOVER IN THE C ARRAY=LCO
C        5.      LOCATION OF INSTANTANEOUS Q DATA IN THE D ARRAY.
C        6.      LOCATION OF MEAN Q DATA IN THE D ARRAY.
C.......................................
  199 CONTINUE
      RETURN
      END
