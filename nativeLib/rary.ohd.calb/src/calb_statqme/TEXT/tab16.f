C MEMBER TAB16
C  (from old member MCTAB16)
C
      SUBROUTINE TAB16(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'STAT-QME'
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980
C.......................................
      INTEGER TO(1)
      DIMENSION TS(MTS),PO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_statqme/RCS/tab16.f,v $
     . $',                                                             '
     .$Id: tab16.f,v 1.2 1996/07/11 19:46:09 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H6   /
C.......................................
C     TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,16,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      IXCEED=PO(19)
      NW=PO(20)
      LENGTH=7
C.......................................
C     CHECK IF ENOUGH SPACE AVAILABLE IN T( ).
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(1)=16
      TO(2)=NXT+LENGTH
      TO(3)=LPO
C
C     SIMULATED DISCHARGE TIME SERIES.
      IT=PO(11)
      CALL CKINPT(PO(8),PO(10),IT,LD,TS,MTS,IERR)
      TO(4)=LD
C
C     OBSERVED DISCHARGE TIME SERIES
      IT=PO(15)
      CALL CKINPT(PO(12),PO(14),IT,LD,TS,MTS,IERR)
      TO(5)=LD
C
C     WORK SPACE.
      TO(6)=NWORK
      LWX=0
      IF(IXCEED.GT.0) LWX=NWORK+NW
      TO(7)=LWX
      LWORK=NW
      IF(IXCEED.GT.0) LWORK=LWORK+100
      IDT=0
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TO(I),I=1,IUSET)
  900 FORMAT(1H0,34HTAB16 DEBUG--CONTENTS OF TO ARRAY=,10I6)
C.......................................
C     CONTENTS OF THE TO ARRAY.
C        POSITION               CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=16
C        2.        LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN THE P ARRAY.
C        4.        LOCATION OF SIM.Q DATA IN D ARRAY.
C        5.        LOCATION OF OBS. Q DATA IN D ARRAY.
C        6.        LOCATION OF WORK SPACE IN D ARRAY.
C        7.        LOCATION OF WORK SPACE FOR EXCEEDENCE TABLE
C.......................................
  199 CONTINUE
      RETURN
      END
