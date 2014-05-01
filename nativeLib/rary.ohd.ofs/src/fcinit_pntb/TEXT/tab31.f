C     MODULE TAB31
C
      SUBROUTINE TAB31(TO,LEFT,IUSET,NXT,LPS,PS,LCS,TS,MTS,NWORK,
     1   LWORK,IDT)
C.......................................
C     THIS IS THE OPERATION TABLE ENTRY ROUTINE FOR THE 'SNOW-43 '
C     OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR SNOW-17 operation
C     Modified by...
C        Nalneesh Gaur - RTi., June 1995 for the Snow-43 operation      
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      TO            O          Integer array used to store
C                               operations table entry
C      LEFT          I          Remaining space in the T array
C      IUSET         O          Space used by this operation
C      NXT           I          Starting location of TO array in T
C      LPO           I          starting location of PO array in P
C      PO            I          Parameter array
C      LCO           I          starting location of CO array in C
C      TS            I          Array containing information on 
C                               all time series
C      MTS           I          Dimension of TS array
C      NWORK         I          Location of space in D array
C                               availabale for working space
C      LWORK         O          Amount of working space needed
C                               by this operation
C      IDT           O          Computational time interval for
C                               this operation
C.......................................
      implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real    ps, ts
      integer to, left, iuset, nxt, lps, lcs, mts,
     1        nwork, lwork, idt
C     --- L O C A L ---
      integer it, lts, i, laec, loc,
     1        ierr, length, ibug, ndt, ld, 
     2        itpx
      real    dim, sname
      integer itowe, itswe
C     --- C O M M O N  B L O C K  V A R S ---
      integer iodbug, itrace, idball, ndebug, idebug
      integer mainum, ndd
      real    vers, vdate, pname
C
C----- D I M E N S I O N --------------------------------
      dimension TO(*)
      DIMENSION TS(MTS),PS(*)
      DIMENSION SNAME(2)
C
C----- C O M M O N  B L O C K S -------------------------
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab31.f,v $
     . $',                                                             '
     .$Id: tab31.f,v 1.1 1996/05/07 10:52:09 page Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA SNAME/4HTAB3,4H1   /
C.......................................
C     TRACE LEVEL=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,31,IBUG)
      IF(ITRACE.GE.1) WRITE(IODBUG,899)
  899 FORMAT(1H0,16H** TAB31 ENTERED)
C.......................................
C     INITIAL VALUES
      LENGTH=16
C.......................................
C     CHECK IF SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(1)=31
      TO(2)=NXT+LENGTH
      TO(3)=LPS
      TO(4)=LCS
C
C
C     PRECIPITATION TIME SERIES.
      ITPX=PS(10)
      CALL CKINPT(PS(7),PS(9),ITPX,LD,TS,MTS,IERR)
      TO(5)=LD
C
C     AIR TEMPERATURE TIME SERIES.
      IDT=PS(14)
      CALL CKINPT(PS(11),PS(13),IDT,LD,TS,MTS,IERR)
      TO(6)=LD
      NDT=IDT/ITPX
C
C     RAIN+MELT TIME SERIES
      LOC=PS(17)
      IF (LOC.GT.0) GO TO 96
      TO(7)=0
      GO TO 95
   96 CALL FINDTS(PS(LOC),PS(LOC+2),ITPX,LD,LTS,DIM)
      TO(7)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C
C     PERCENT SNOWFALL TIME SERIES
   95 LOC=PS(18)
      IF(LOC.GT.0) GO TO 101
      TO(8)=0
      GO TO 105
  101 CALL CKINPT(PS(LOC),PS(LOC+2),ITPX,LD,TS,MTS,IERR)
      TO(8)=LD
C
C     OBSERVED WATER-EQUIVALENT TIME SERIES.
  105 LOC=PS(19)
      IF(LOC.GT.0) GO TO 106
      TO(9)=0
      GO TO 110
  106 IT=PS(LOC+3)
      itowe = it
      CALL CKINPT(PS(LOC),PS(LOC+2),IT,LD,TS,MTS,IERR)
      TO(9)=LD
C
C     RAIN-SNOW ELEVATION TIME SERIES
  110 LAEC=PS(30)
      IF (LAEC.GT.0) GO TO 111
      TO(10)=0
      GO TO 115
  111 LOC=LAEC+2
      CALL CKINPT(PS(LOC),PS(LOC+2),IDT,LD,TS,MTS,IERR)
      TO(10)=LD
C
C     SIMULATED WATER-EQUIVALENT TIME SERIES.
  115 LOC=PS(20)
      IF(LOC.GT.0) GO TO 116
      TO(11)=0
      GO TO 120
  116 IT=PS(LOC+3)
      itswe = it
      CALL FINDTS(PS(LOC),PS(LOC+2),IT,LD,LTS,DIM)
      TO(11)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C
C     OBSERVED AREAL SNOW COVER TIME SERIES.
  120 LOC=PS(21)
      IF(LOC.GT.0) GO TO 121
      TO(12)=0
      GO TO 125
  121 IT=PS(LOC+3)
      CALL CKINPT(PS(LOC),PS(LOC+2),IT,LD,TS,MTS,IERR)
      TO(12)=LD
C
C     SIMULATED AREAL SNOW COVER TIME SERIES.
  125 LOC=PS(22)
      IF(LOC.GT.0) GO TO 126
      TO(13)=0
      GO TO 130
  126 IT=PS(LOC+3)
      CALL FINDTS(PS(LOC),PS(LOC+2),IT,LD,LTS,DIM)
      TO(13)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C
C     variance of observed water equivalent time series.
  130 loc=ps(31)
      if(loc.gt.0) go to 131
      to(14)=0
      go to 135
  131 it=itowe
      call ckinpt(ps(loc), ps(loc+2), it, ld, ts, mts, ierr)
      to(14)=ld
C
C     variance of simulated water equivalent time series.
  135 loc=ps(32)
      if(loc.gt.0) go to 136
      to(15)=0
      go to 140
  136 it=itswe    
      call findts(ps(loc), ps(loc+2), it, ld, lts, dim)
      to(15)=ld
      if(lts.gt.0) ts(lts+8)=1.01
C
C     WORKING SPACE
  140 TO(16)=NWORK
      LWORK=3*NDT+24/IDT
      IF (MAINUM.NE.1) GO TO 145
      I=(24/ITPX)*NDD
      IF (I.GT.LWORK) LWORK=I
  145 IUSET=LENGTH
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) IDT,LWORK
  900 FORMAT(1H0,24HSNOW-43--CONTENTS OF TO.,5X,3HDT=,I3,5X,6HLWORK=,I6)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)                                                  
C.......................................
C     CONTENTS OF THE TO ARRAY.
C     POSITION                    CONTENTS
C        1.        I.D. NUMBER FOR THE OPERATION=31
C        2.        LOCATION OF NEXT OPERATION IN THE T ARRAY.
C        3.        LOCATION OF PARAMETERS IN P ARRAY=LPS
C        4.        LOCATION OF CARRYOVER IN C ARRAY=LCS
C        5.        LOCATION OF PRECIPITATION DATA IN THE D ARRAY
C        6.        LOCATION OF TEMPERATURE DATA IN THE D ARRAY
C        7.        LOCATION TO PUT RAIN+MELT DATA IN D().
C        8.        LOCATION OF PERCENT SNOWFALL DATA IN D(),=0 IF NONE
C        9.        LOCATION OF OBS.W.E. DATA IN D(),=0 IF NONE
C       10.        LOCATION OF RAIN-SNOW ELEV DATA IN D(), =0 IF NONE
C       11.        LOCATION TO PUT SIM.W.E. DATA IN D(),=0 IF NONE
C       12.        LOCATION OF OBS. PCT.COVER DATA IN D(),=0 IF NONE
C       13.        LOCATION TO PUT SIM. PCT. COVER DATA IN D(),=0 IF NONE
C       14.        LOCATION OF VARIANCE OF OBS WATER EQUIV. IN D(),=0 if none
C       15.        LOCATION OF VARIANCE OF SIM WATER EQUIV. IN D(),=0 if none
C       16.        LOCATION OF WORKING SPACE IN THE D ARRAY.
C.......................................
  199 CONTINUE
      IF(ITRACE.GE.1) WRITE(IODBUG,912)
  912 FORMAT(1H0,13H** EXIT TAB31)
      RETURN
      END
