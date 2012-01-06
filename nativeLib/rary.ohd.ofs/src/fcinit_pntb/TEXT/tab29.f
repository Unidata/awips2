C MEMBER TAB29
C  (from old member FCTAB29)
C
      SUBROUTINE TAB29(TO,LEFT,IUSET,NXT,LPO,PO,LCO,TS,MTS,LWORK,IDT)
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE KANSAS CITY
C        API OPERATION--'API-MKC'
C.......................................
C     WRITTEN BY--ERIC ANDERSON - HRL  JULY 1982
C         MODIFIED FEB. 85 FOR API AND AI TIME SERIES.
C.......................................
      DIMENSION PO(1),TS(MTS)
      INTEGER TO(1)
      DIMENSION SNAME(2)
C
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab29.f,v $
     . $',                                                             '
     .$Id: tab29.f,v 1.1 1995/09/17 18:49:14 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA SNAME/4HTAB2,4H9   /
      DATA BLANK/4H    /
C.......................................
C     CHECK FOR DEBUG OUTPUT--TRACES LEVEL=1.
      CALL FPRBUG(SNAME,1,29,IBUG)
C.......................................
C     INITIAL VALUES.
      LWORK=0
      IT=24
C.......................................
C     CHECK TO SEE IF SPACE AVAILABLE IN T( )
      LENGTH=9
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES IN TO( )
 100  TO(1)=29
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=LCO
C
C     FIND LOCATION OF RAIN+MELT AND WATER-EQUIVALENT DATA.
      IDELT=PO(13)
      CALL CKINPT(PO(14),PO(16),IDELT,LD,TS,MTS,IERR)
      TO(5)=LD
      IDT=24
      CALL CKINPT(PO(20),PO(22),IDT,LD,TS,MTS,IERR)
      TO(6)=LD
C
C     LOCATION OF RUNOFF TIME SERIES.
      CALL FINDTS(PO(17),PO(19),IDELT,LD,LTS,DIM)
      TO(7)=LD
      IF(LTS.GT.0)TS(LTS+8)=1.01
C
C     LOCATION OF API AND AI OPTIONAL TIME SERIES.
      IF (PO(34).EQ.BLANK) GO TO 104
      CALL FINDTS(PO(32),PO(34),IT,LD,LTS,DIM)
      TO(8)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 105
  104 TO(8)=0
  105 IF (PO(37).EQ.BLANK) GO TO 109
      CALL FINDTS(PO(35),PO(37),IT,LD,LTS,DIM)
      TO(9)=LD
      IF (LTS.GT.0) TS(LTS+8)=1.01
      GO TO 110
  109 TO(9)=0
  110 IUSET=LENGTH
C     ALL ENTRIES HAVE BEEN MADE.
C.......................................
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)(TO(I),I=1,IUSET)
 900  FORMAT(1H0,31HTAB29 DEBUG--CONTENTS OF TO( )=,15I6)
C.......................................
C     TO ARRAY ENTRIES FOR THE 'API-MKC' OPERATION.
C     POSITION                      CONTENTS
C        1.                I.D. NUMBER FOR OPERATION=29
C        2.               LOCATION OF NEXT OPERATION IN T( ).
C        3.               LOCATION OF PARAMETERS IN P ARRAY-LPO
C        4.               LOCATION OF CARRYOVER IN C ARRAY-LCO
C        5.               LOCATION OF RAIN+MELT IN D ARRAY
C        6.               LOCATION OF WATER+EQUIVALENT IN D ARRAY
C        7.               LOCATION TO PUT RUNOFF IN D ARRAY
C        8.               LOCATION TO PUT API IN D ARRAY
C                             (=0 IF NO API OUTPUT)
C        9.               LOCATION TO PUT AI IN D ARRAY
C                             (=0 IF NO AI OUTPUT)
C.......................................
 199  CONTINUE
      RETURN
      END
