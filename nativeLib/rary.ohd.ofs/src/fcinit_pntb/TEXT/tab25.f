C MEMBER TAB25
C  (from old member FCTAB25)
C
      SUBROUTINE TAB25(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,NDD,LWORK,
     1   IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY ROUTINE FOR THE
C     'PLOT-TUL' OPERATION.
C.......................................
C     INITIALLY WRITTEN BY ... ERIC ANDERSON-HRL  MAY 1982
C.......................................
      DIMENSION TS(MTS),PO(1)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab25.f,v $
     . $',                                                             '
     .$Id: tab25.f,v 1.2 1996/12/06 20:02:26 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB2,4H5   /
      DATA BLANK/4H    /
C.......................................
C     TRACE LEVEL = 1 -- DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,25,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES
      novrsn=po(1)
      NPLOT=PO(9)
      LENGTH=NPLOT+5
      IHYD=PO(2)
      IDT=PO(7)
      NPLTSZ=PO(4)
      NMLIST=PO(10)
C.......................................
C     CHECK TO SEE IF SPACE AVAILBLE IN T( ).
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      LWORK=0
      IDT=0
      RETURN
C.......................................
C     SPACE AVAILABLE-- MAKE ENTRIES IN TO( ).
 100  TO(1)=25
      TO(2)=NXT+LENGTH
      TO(3)= LPO
C
C     RATING CURVE LOCATION = 0 IF NONE USED.
      IF(IHYD.EQ.0) GO TO 101
      IF((PO(21).EQ.BLANK).AND.(PO(22).EQ.BLANK)) GO TO 101
C     RATING CURVE IS USED.
      TO(4)=LPO+20
      GO TO 105
C     RATING CURVE NOT USED
 101  TO(4)=0
C
C     FIND LOCATION OF EACH TIME SERIES
 105  IF(NPLOT.GT.0) GO TO 106
      IUSET=0
      LWORK=0
      IDT=0
      RETURN
 106  L=30
      IF((NPLTSZ.EQ.51).AND.(NMLIST.GT.3)) L=43
      DO 107 N=1,NPLOT
      J=L+(N-1)*12+1
      if (novrsn.eq.2) j=l+(n-1)*18+1
      idttmp=idt
      if(novrsn.eq.2) idttmp=po(j+12)
      call ckinpt(po(j),po(j+2),idttmp,ld,ts,mts,ierr)
c     CALL CKINPT(PO(J),PO(J+2),IDT,LD,TS,MTS,IERR)
      TO(5+N)=LD
 107  CONTINUE
C
C     STORE LOCATION OF WORKING SPACE AND COMPUTE LENGTH.
      TO(5)=NWORK
      LWORK=NPLTSZ
      IUSET=LENGTH
C     ALL ENTRIES INTO TO( ) HAVE BEEN MADE
C.......................................
C     CHECK FOR DEBUG OUTPUT.
      IF (IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)IDT,LWORK
 900  FORMAT(1H0,33HPLOT-TUL DEBUG--COMPUTATIONAL DT=,I3,5X,6HLWORK=,I6,
     1  /6X,20HCONTENTS OF TO ARRAY)
      WRITE(IODBUG,901)(TO(I),I=1,IUSET)
 901  FORMAT(1H ,20I6)
C.......................................
C     CONTENTS OF TO ARRAY
C     POSITION        CONTENTS
C       1.         OPERATION NUMBER=25
C       2.         LOCATION OF NEXT OPERATION IN T ARRAY.
C       3.         LOCATION OF PARAMETERS IN P ARRAY-LPO.
C       4.         LOCATION OF RATING CURVE ID IN P ARRAY.
C       5.         LOCATION OF START OF WORKING SPACE IN D ARRAY.
C       6 TO       LOCATION OF EACH TIME SERIES TO BE PLOTTED
C       NPLOT+5       OR LISTED IN THE D ARRAY.
C.......................................
 199  CONTINUE
      RETURN
      END
