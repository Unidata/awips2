C MEMBER TAB14
C  (from old member FCTAB14)
C
      SUBROUTINE TAB14(TADJ,LEFT,IUSET,NXT,LPADJ,PADJ,LCADJ,TS,MTS,
     1 NWORK,NDD,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'ADJUST-Q',
C        OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY...
C       ERIC ANDERSON-HRL JULY 1980
C.......................................
      INTEGER TADJ(1)
      DIMENSION TS(MTS),PADJ(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCK
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab14.f,v $
     . $',                                                             '
     .$Id: tab14.f,v 1.1 1995/09/17 18:49:05 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H4   /
C.......................................
C     TRACE LEVEL=1,DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,14,IBUG)
C.......................................
C     INITIAL VALUE
      LENGTH= 11
      IOPT=25
      IOI=PADJ(7)
      IOM=PADJ(8)
C.......................................
C     CHECK IF SPACE AVAILABLE IN T( ).
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF (IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE AVAILABLE--MAKE ENTRIES INTO TADJ( ).
  100 TADJ(1)=14
      TADJ(2)=NXT+LENGTH
      TADJ(3)=LPADJ
      TADJ(4)=LCADJ
C
C     OBSERVED INST.DISCHARGE TIME SERIES.
      IF(IOI.EQ.1) GO TO 101
      TADJ(5)=0
      GO TO 105
  101 LOC=IOPT+IOM*4
      IT=PADJ(LOC+3)
      CALL CKINPT(PADJ(LOC),PADJ(LOC+2),IT,LD,TS,MTS,IERR)
      TADJ(5)=LD
C
C     OBSERVED MEAN DAILY TIME SERIES.
  105 IF (IOM.EQ.1) GO TO 106
      TADJ(6)=0
      GO TO 110
  106 LOC=IOPT
      IT=24
      CALL CKINPT(PADJ(LOC+1),PADJ(LOC+3),IT,LD,TS,MTS,IERR)
      TADJ(6)=LD
C
C     SIMULATED INST. TIME SERIES.
  110 IDT=PADJ(12)
      CALL CKINPT(PADJ(9),PADJ(11),IDT,LD,TS,MTS,IERR)
      TADJ(7)=LD
C
C     ADJUSTED INST. TIME SERIES.
      CALL FINDTS(PADJ(13),PADJ(15),IDT,LD,LTS,DIM)
      TADJ(8)=LD
      IF(LTS.GT.0) TS(LTS+8)=1.01
C
C     WORKING SPACE
      TADJ(9)=NWORK
      LW=(24/IDT)*NDD
      TADJ(10)=LW
      IF (IOM.EQ.1) GO TO 111
      TADJ(11)=0
      GO TO 115
  111 TADJ(11)=NDD
  115 LWORK=LW+2*NDD*IOM+LW*IOI
      IUSET=LENGTH
C     ALL ENTRIES MADE
C.......................................
C     DEBUG OUTPUT
      IF (IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900) (TADJ(I),I=1,IUSET)
  900 FORMAT (1H0,27HADJUST-Q--CONTENTS OF TADJ=,15I6)
C.......................................
C     CONTENTS OF THE TADJ ARRAY.
C     POSITION                         CONTENTS
C       1.     ID. NUMBER FOR THE OPERATION=14
C       2.     LOCATION OF NEXT OPERATION IN THE T ARRAY.
C       3.     LOCATION OF PARAMETERS IN P ARRAY=LPADJ
C       4.     LOCATION OF CARRYOVER IN C ARRAY=LCADJ
C       5.     LOCATION OF OBS. INST. Q DATA IN D ARRAY
C                  =0 IF NONE USED
C       6.     LOCATION OF OBS. MEAN DAILY Q DATA IN D ARRAY
C                  =0 IF NONE USED
C       7.     LOCATION OF SIM.INST. Q DATA IN D ARRAY
C       8.     LOCATION TO PUT ADJ.INST. Q DATA IN D ARRAY
C       9.     START OF WORKING SPACE IN D ARRAY
C      10.     LENGTH OF INST. Q WORKING SPACE
C      11.     LENGTH OF DAILY Q WORKING SPACE
C                  =0 IF NONE USED
C.......................................
  199 CONTINUE
      RETURN
      END
