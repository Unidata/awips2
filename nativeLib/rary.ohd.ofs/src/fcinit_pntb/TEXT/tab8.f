C MEMBER TAB8
C  (from old member FCTAB8)
C
      SUBROUTINE TAB8(TLOSS,LEFT,IUSET,NXT,LPLOSS,PLOSS,LCLOSS,TS,MTS,
     1IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR CHANNEL LOSS
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   NOV. 1979
C.......................................
      DIMENSION PLOSS(1),TS(MTS)
      INTEGER TLOSS(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab8.f,v $
     . $',                                                             '
     .$Id: tab8.f,v 1.1 1995/09/17 18:49:29 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB8,4H    /
C.......................................
C     TRACE LEVEL = 1--DEBUG SEITCH=IBUG
      CALL FPRBUG(SNAME,1,8,IBUG)
C.......................................
C     CHECK TO SEE IF 6 SPACES ARE AVAILABLE IN T().
      CALL CHECKT(6,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TLOSS().
  100 TLOSS(1)=8
      TLOSS(2)=NXT+6
      TLOSS(3)=LPLOSS
C
C     CHECK DISCHARGE TIME SERIES AND GET LOCATION IN D ARRAY.
      IDT=PLOSS(10)
      CALL CKINPT(PLOSS(7),PLOSS(9),IDT,LD,TS,MTS,IERR)
      TLOSS(5)=LD
C
C     CHECK IF PE DATA NEEDED - IF SO, GET LOCATION.
      IF(PLOSS(11).GT.0.0) GO TO 105
      GO TO 106
  105 IPE=PLOSS(12)
      IF(IPE.EQ.1) GO TO 110
C     NO PE TIME SERIES USED
  106 TLOSS(6)=0
      TLOSS(4)=0
      GO TO 120
C     PE TIME SERIES USED.
  110 JDT=PLOSS(16)
      CALL CKINPT(PLOSS(13),PLOSS(15),JDT,LD,TS,MTS,IERR)
      TLOSS(6)=LD
      TLOSS(4)=LCLOSS
  120 IUSET=6
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TLOSS(I),I=1,IUSET)
  900 FORMAT(1H0,34HCHANLOSS DEBUG--CONTENTS OF TLOSS=,10I6)
C.......................................
C     THE TLOSS ARRAY ENTRIES ARE AS FOLLOWS.
C     POSITION            CONTENTS
C     1.      I.D. NUMBER OF THE OPERATION=8
C     2.      LOCATION OF THE NEXT OPERATION IN THE T ARRAY.
C     3.      LOCATION OF THE PARAMETERS IN THE P ARRAY=LPLOSS
C     4.      LOCATION OF CARRYOVER IN THE C ARRAY.
C                 =0 IF NO CARRYOVER.
C     5.      LOCATION OF DISCHARGE TIME SERIES IN D ARRAY.
C     6.      LOCATION OF PE TIME SERIES IN D ARRAY
C                 =0 IF NO PE DATA.
C.......................................
  199 CONTINUE
      RETURN
      END
