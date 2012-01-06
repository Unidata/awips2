C MEMBER TAB1
C  (from old member FCTAB1)
C
      SUBROUTINE TAB1(TL,LEFT,IUSET,NXT,LPL,PL,LCL,TS,MTS)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE SACRAMENTO
C        SOIL-MOISTURE ACCOUNTING OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY. . .
C            ERIC ANDERSON - HRL     JUNE 1979
C.......................................
      DIMENSION PL(1),TS(MTS)
      INTEGER TL(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab1.f,v $
     . $',                                                             '
     .$Id: tab1.f,v 1.1 1995/09/17 18:49:01 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB1,4H    /
      DATA FGIX/4HFGIX/
C.......................................
C     TRACE LEVEL FOR THIS SUBROUTINE=1.
C     CHECK IF DEBUG OUTPUT IS NEEDED.
      CALL FPRBUG(SNAME,1,1,IBUG)
C.......................................
C     CHECK TO SEE IF SPACE IS AVAILABLE IN T( ).
      LENGTH=13
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TL.
  100 TL(1)=1
      TL(2)=NXT+LENGTH
      TL(3)=LPL
      TL(4)=LCL
C
C     FIND LOCATION OF RAIN+MELT AND RUNOFF TIME SERIES IN THE
C        D ARRAY AND STORE IN TL.
      IDT=PL(2)
      CALL CKINPT(PL(8),PL(10),IDT,LD,TS,MTS,IERR)
      TL(5)=LD
      CALL FINDTS(PL(11),PL(13),IDT,LD,LTS,DIM)
      TL(6)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
C
C     FIND THE 4 OPTIONAL TIME SERIES (IF THEY EXIST) AND
C        STORE LOCATION OF DATA IN TL.
      DO 110 I=1,4
      K=I+13
      J=I+6
      LOC=PL(K)
      IF (LOC.GT.0) GO TO 105
      TL(J)=0
      GO TO 110
  105 IF (I.GT.2) GO TO 106
      IT=24
      IF (I.EQ.2) IT=PL(LOC+3)
      CALL CKINPT(PL(LOC),PL(LOC+2),IT,LD,TS,MTS,IERR)
      TL(J)=LD
      GO TO 110
  106 IT=PL(LOC+3)
      CALL FINDTS(PL(LOC),PL(LOC+2),IT,LD,LTS,DIM)
      TL(J)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
  110 CONTINUE
      IFRZE=PL(24)
      IF(IFRZE.GT.0) GO TO 115
      TL(11)=0
      TL(12)=0
      TL(13)=0
      GO TO 120
C
C     FROZEN GROUND TIME SERIES.
  115 IT=PL(IFRZE+3)
      CALL CKINPT(PL(IFRZE), PL(IFRZE+2), IT,LD,TS,MTS,IERR)
      TL(11)=LD
      LWE=PL(IFRZE+4)
      IF(LWE.GT.0) GO TO 116
      TL(12)=0
      GO TO 117
  116 J=IFRZE+LWE-1
      IT=PL(J+3)
      CALL CKINPT(PL(J), PL(J+2),IT,LD,TS,MTS,IERR)
      TL(12)=LD
  117 LFI=PL(IFRZE+5)
      IF(LFI.GT.0) GO TO 118
      TL(13)=0
      GO TO 120
  118 J=IFRZE+LFI-1
      IT=PL(J+2)
      CALL FINDTS(PL(J),FGIX,IT,LD,LTS,DIM)
      TL(13)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
  120 CONTINUE
      IUSET=LENGTH
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE (IODBUG,902) (TL(I),I=1,LENGTH)
  902 FORMAT (1H0,15HCONTENTS OF TL=,16I6)
C.......................................
C     THE TL ARRAY ENTRIES FOR THE SACRAMENTO SOIL MOISTURE
C        ACCOUNTING MODEL OPERATION IS AS FOLLOWS.
C        POSITION                CONTENTS
C           1.     I.D. NUMBER FOR THE OPERATION=1
C           2.     LOCATION OF THE NEXT OPERATION IN THE T ARRAY
C           3.     LOCATION OF PARAMETERS IN THE P ARRAY=LPL
C           4.     LOCATION OF CARRYOVER IN THE C ARRAY=LCL
C           5.     LOCATION OF RAIN+MELT DATA IN THE D ARRAY
C           6.     LOCATION TO PUT RUNOFF VALUES IN D().
C           7.     LOCATION OF PE DATA IN D(), =0 IF NONE USED.
C           8.     LOCATION OF AREAL SNOW COVER DATA IN D(), =0 IF NONE.
C           9.     LOCATION TO PUT RUNOFF COMPONENTS IN D(), =0 IF NONE.
C          10.     LOCATION TO PUT MOISTURE STORAGES IN D(), =0 IF NONE.
C          11.     LOCATION OF TEMPERATURE DATA IN D(), =0 IF NONE
C          12.     LOCATION OF WE DATA IN D(),  =0 IF NONE
C          13.     LOCATION TO PUT FROST INDEX DATA IN D(),  =0 IF NONE
C.......................................
  199 CONTINUE
      RETURN
      END
