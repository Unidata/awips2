C MEMBER TAB36
C  (from old member FCTAB36)
C
      SUBROUTINE TAB36(TL,LEFT,IUSET,NXT,LPL,PL,LCL,TS,MTS,NWORK,
     1LWORK,JDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE XINANJIANG
C        BASIN MODEL OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON  - HRL   JULY  1988
C.......................................
      DIMENSION PL(1),TS(MTS)
      INTEGER TL(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab36.f,v $
     . $',                                                             '
     .$Id: tab36.f,v 1.1 1995/09/17 18:49:19 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H6   /
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
C     CHECK IF DEBUG OUTPUT IS NEEDED.
      CALL FPRBUG(SNAME,1,36,IBUG)
C.......................................
C     GET CONTROL VARIABLES.
      IDT=PL(2)
      NUNIT=PL(3)
      ISUM=PL(9)
      IET=PL(11)
      IPXRO=PL(15)
      NWORD=PL(16)
      LENGTH=9+2*NUNIT
C.......................................
C     CHECK TO SEE IF SPACE IS AVAILABLE IN T( ).
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      LWORK=0
      JDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TL.
  100 TL(1)=36
      TL(2)=NXT+LENGTH
      TL(3)=LPL
      TL(4)=LCL
C
C     CHECK IF PE USED   -- STORE LOCATION
      JDT=24
      IF(IET.EQ.0) GO TO 105
      CALL CKINPT(PL(12),PL(14),JDT,LD,TS,MTS,IERR)
      TL(5)=LD
      GO TO 110
  105 TL(5)=0
C     WORK SPACE -- LOCATION AND AMOUNT NEEDED.
  110 TL(6)=NWORK
      TL(7)=NWORK+7*NUNIT
      TL(8)=NWORK+14*NUNIT
      TL(9)=NWORK+19*NUNIT
      IF(MAINUM.NE.1) TL(9)=0
      LWORK=19*NUNIT
      IF(ISUM.GE.1) LWORK=LWORK+5*NUNIT
      IF(MAINUM.EQ.1) LWORK=LWORK+NUNIT
C     PRECIPITATION AND CHANNEL INFLOW FOR EACH UNIT
      DO 130 I=1,NUNIT
      LOC=IPXRO+(I-1)*NWORD
      J=10+(I-1)*2
      CALL CKINPT(PL(LOC),PL(LOC+2),IDT,LD,TS,MTS,IERR)
      TL(J)=LD
      CALL FINDTS(PL(LOC+3),PL(LOC+5),IDT,LD,LTS,DIM)
      TL(J+1)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
  130 CONTINUE
      IUSET=LENGTH
C     ALL ENTRIES INTO TL() HAVE BEEN MADE
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) JDT,LWORK
  900 FORMAT(1H0,41HXINANJIANG MODEL DEBUG -- COMPUTATION DT=,I3,5X,
     16HLWORK=,I6,/6X,20HCONTENTS OF TL ARRAY )
      WRITE(IODBUG,901) (TL(I),I=1,IUSET)
  901 FORMAT(1H0,20I6)
C.......................................
C     THE CONTENTS OF THE TO ARRAY ARE AS FOLLOWS.
C        POSITION               CONTENTS
C        1.      I.D. NUMBER FOR THE OPERATION=36
C        2.      LOCATION OF THE NEXT OPERATION IN THE T ARRAY
C        3.      LOCATION OF PL IN THE P ARRAY
C        4.      LOCATION OF CL IN C ARRAY
C        5.       LOCATION OF PE IN D ARRAY
C                 =0    IF PE NOT USED
C        6.      LOCATION OF CARRYOVER WORKING SPACE IN D ARRAY
C        7.      LOCATION OF INITIAL CO WORKING SPACE IN DARRAY.
C        8.      LOCATION OF WORKING SPACE TO STORE EXECUTION PERIOD
C                SUMS IN D ARRAY.
C        9.      LOCATION OF WORKING SPACE TO STORE WATER BALANCE
C                ADJUSTMENTS.
C                =0 IF NOT NEEDED
C        10 TO         FOR EACH UNIT THE FOLLOWING TWO
C        9+NUNIT*2       VALUE ARE STORED
C                     - LOCATION OF PRECIPITATION IN D ARRAY.
C                     - LOCATION TO PUT CHANNEL INFLOW IN D ARRAY.
C.......................................
  199 CONTINUE
      RETURN
      END

