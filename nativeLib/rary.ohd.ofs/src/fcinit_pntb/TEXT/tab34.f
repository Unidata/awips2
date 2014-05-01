C MEMBER TAB34
C  (from old member FCTAB34)
C
      SUBROUTINE TAB34(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE
C       SALT LAKE CITY API OPERATION--'API-SLC'
C.......................................
C     WRITTEN BY -- ERIC ANDERSON -- HRL MARCH 1985
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C     COMMON BLOCKS
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab34.f,v $
     . $',                                                             '
     .$Id: tab34.f,v 1.1 1995/09/17 18:49:17 dws Exp $
     . $' /
C    ===================================================================
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB3,4H4   /
C.......................................
C     CHECK FOR DEBUG OUTPUT--TRACE LEVEL=1
      CALL FPRBUG(SNAME,1,34,IBUG)
C.......................................
C     INITIAL VALUES
      LWORK=0
C.......................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T( )
      LENGTH=8
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0)GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE -- MAKE ENTRIES IN TO( )
 100  TO(1)=34
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C.......................................
C     GET LOCATION OF RAIN+MELT DATA
      IT=PO(11)
      CALL CKINPT(PO(12),PO(14),IT,LD,TS,MTS,IERR)
      TO(5)=LD
      IDT=24
C.......................................
C     LOCATION OF RUNOFF TIME SERIES
      IT=PO(15)
      CALL FINDTS(PO(16),PO(18),IT,LD,LTS,DIM)
      TO(6)=LD
      IF(LTS.GT.0)TS(LTS+8)=1.01
C.......................................
C     LOCATION OF OPTIONAL TIME SERIES
C      AREAL EXTENT OF SNOW
      IT=PO(19)
      IF(IT.EQ.0)GO TO 104
      CALL CKINPT(PO(20),PO(22),IT,LD,TS,MTS,IERR)
      TO(7)=LD
      GO TO 105
  104 TO(7)=0
C      API TIME SERIES
  105 IT=PO(23)
      IF(IT.EQ.0) GO TO 109
      CALL FINDTS(PO(24),PO(26),IT,LD,LTS,DIM)
      TO(8)=LD
      IF(LTS.GT.0)TS(LTS+8)=1.01
      GO TO 110
  109 TO(8)=0
  110 IUSET=LENGTH
C     ALL ENTRIES HAVE BEEN MADE
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0)GO TO 199
      WRITE(IODBUG,900)(TO(I),I=1,IUSET)
  900 FORMAT(1H0,31HTAB34 DEBUG--CONTENTS OF TO( )=,15I6)
C.......................................
C  TO ARRAY ENTRIES FOR THE 'API-SLC' OPERATION.
C   POSITION        CONTENTS
C     1.      ID NUMBER FOR OPERATION=34
C     2.      LOCATION OF NEXT OPERATION IN T( )
C     3.      LOCATION OF PARAMETERS IN P ARRAY-LPO
C     4.      LOCATION OF CARRYOVER IN C ARRAY-LCO
C     5.      LOCATION OF RAIN+MELT IN D ARRAY
C     6.      LOCATION TO PUT RUNOFF IN D ARRAY
C     7.      LOCATION OF AREAL SNOW EXTENT IN D ARRAY
C               (=0 IF NOT USED)
C     8.      LOCATION TO PUT API IN D ARRAY
C               (=0 IF NO API OUTOUT)
C.......................................
  199 CONTINUE
      RETURN
      END
