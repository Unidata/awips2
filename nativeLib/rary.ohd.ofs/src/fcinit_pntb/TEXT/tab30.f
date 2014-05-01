C MEMBER TAB30
C  (from old member FCTAB30)
C
      SUBROUTINE TAB30 (TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C        'MERGE-TS' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   JANUARY 1991
C.......................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab30.f,v $
     . $',                                                             '
     .$Id: tab30.f,v 1.1 1995/09/17 18:49:15 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H0   /
C.......................................
C     TRACE LEVEL =1 -- DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,30,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES AND SET INITIAL VALUES.
      NIN=PO(2)
      LWORK=0
      IDT=0
      LENGTH=NIN+4
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO()
  100 TO(1)=30
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      IDT=PO(6)
C
C     LOCATIONS OF INPUT TIME SERIES.
      I=13
      DO 110 N=1,NIN
      CALL CKINPT(PO(I),PO(I+2),IDT,LD,TS,MTS,IERR)
      TO(4+N)=LD
      I=I+4
  110 CONTINUE
C
C     LOCATION OF OUTPUT TIME SERIES.
      CALL FINDTS(PO(3),PO(5),IDT,LD,LTS,DIM)
      IF(LTS.GT.0) TS(LTS+8)=1.01
      TO(4)=LD
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)IDT
  900 FORMAT(1H0,33HMERGE-TS DEBUG--COMPUTATIONAL DT=,I3,/6X,
     120HCONTENTS OF TO ARRAY)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)
C.......................................
C     CONTENTS OF THE TO ARRAY
C     POSITION             CONTENTS
C        1.        OPERATION NUMBER=30
C        2.        LOCATION OF NEXT OPERATION IN T ARRAY.
C        3.        LOCATION OF PARAMETERS IN P ARRAY-LPO
C        4.        LOCATION OF OUTPUT TIME SERIES IN D ARRAY.
C     5 TO         LOCATION OF EACH INPUT TIME SERIES
C     4 + NIN           IN THE D ARRAY.
C.......................................
  199 CONTINUE
      RETURN
      END
