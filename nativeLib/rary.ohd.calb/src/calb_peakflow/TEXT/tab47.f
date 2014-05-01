C MODULE TAB47
C
      SUBROUTINE TAB47(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,
     & NWORK,NDD,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE 'PEAKFLOW'
C        OPERATION.
C
C     ROUTINE INITIALLY WRITTEN BY:
C        BRYCE FINNERTY - HRL   JANUARY 1995
C.......................................
      INTEGER TO(*)
      DIMENSION TS(MTS),PO(*)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/tab47.f,v $
     . $',                                                             '
     .$Id: tab47.f,v 1.3 1998/04/09 10:21:18 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB4,4H7   /
C
C
C     TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
c
C     OBTAIN CONTROL VARIABLES.
      IPKSTOR=PO(14)
      IDISP=PO(15)
      MAGFLG=PO(16)
      LENGTH=7
C
C     CHECK IF ENOUGH SPACE AVAILABLE IN T( ).
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(1)=47
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C
C     SIMULATED DISCHARGE TIME SERIES.
      IT=PO(10)
      CALL CKINPT(PO(7),PO(9),IT,LD,TS,MTS,IERR)
      TO(5)=LD
C
C     WORK SPACE.  20 for work, 20 for swap for each peak event=40.
      TO(6)=NWORK
      LWORK=40*IPKSTOR
      TO(7)=NWORK+LWORK
      IDT=IT
      IUSET=LENGTH
C
C  CASE OF NO CHRONOLOGICAL SORTING REQUIRED FOR TABLE OR GRAPH
C  OUTPUT.  DIMENSION SWORK TO ZERO.
      IF ((MAGFLG.EQ.0).AND.(IDISP.EQ.0)) THEN
        LWORK=20*IPKSTOR
        TO(7)=0
      END IF
C
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C
C     DEBUG OUTPUT
      IBUG=0
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(I),I=1,IUSET)
  900 FORMAT(1H0,'TAB47 DEBUG--CONTENTS OF TO() ARRAY=',8I6)
C.......................................
C     CONTENTS OF THE TO ARRAY.
C        POSITION               CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=47
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY.
C        4.        LOCATION OF PARAMETERS IN THE C ARRAY.
C        5.        LOCATION OF SIM. Q DATA IN D ARRAY.
C        6.        LOCATION OF WORK SPACE IN D ARRAY.
C        7.        LOCATION OF SWORK SPACE IN D ARRAY.
C.......................................
  199 CONTINUE
C
C     ITRACE=1
      IF (ITRACE.EQ.1) WRITE(IPR,400)
 400  FORMAT(10X,'** EXIT TAB47 ROUTINE **')
C
      RETURN
      END
