C MEMBER TAB18
C  (from old member FCTAB18)
C
      SUBROUTINE TAB18(TO,LEFT,IUSET,NXT,LPO,PO,TS,MTS,NWORK,LWORK,IDT)
C                             LAST UPDATE: 02/08/94.09:17:41 BY $WC20SV
C
C.......................................
C     THIS IS THE OPERATIONS TABLE ENTRY SUBROUTINE FOR THE 'PLOT-TS '
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
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/tab18.f,v $
     . $',                                                             '
     .$Id: tab18.f,v 1.1 1995/09/17 18:49:06 dws Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENT
      DATA SNAME/4HTAB1,4H8   /
C.......................................
C     TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG (SNAME,1,18,IBUG)
C.......................................
C     OBTAIN CONTROL VARIABLES.
      NTTS=PO(9)
      NPLOTS=PO(8)
      LENGTH=4+NTTS
C.......................................
C     CHECK TO SEE IF ENOUGH SPACE IS AVAILABLE IN T().
      CALL CHECKT(LENGTH,LEFT,IERR)
      IF(IERR.EQ.0) GO TO 100
      IUSET=0
      IDT=0
      LWORK=0
      RETURN
C.......................................
C     SPACE IS AVAILABLE--MAKE ENTRIES INTO TO().
  100 TO(1)=18
      TO(2)=NXT+LENGTH
      TO(3)=LPO
      TO(4)=NWORK
      J=11+9*NPLOTS
      DO 105 N=1,NTTS
      LOC=J+(N-1)*12+1
      IT=PO(LOC+3)
      CALL CKINPT(PO(LOC),PO(LOC+2),IT,LD,TS,MTS,IERR)
      TO(4+N)=LD
  105 CONTINUE
      LWORK=133+2*NPLOTS+3*NTTS
      IDT=PO(11)
      IUSET=LENGTH
C     ALL ENTRIES INTO TO() HAVE BEEN MADE
C     DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
      WRITE(IODBUG,900)
  900 FORMAT(1H0,34HTAB18 DEBUG--CONTENTS OF TO ARRAY.)
      WRITE(IODBUG,901) (TO(I),I=1,IUSET)
  901 FORMAT(1H ,20I6)
C.......................................
C     THE CONTENTS OF THE TO ARRAY ARE AS FOLLOWS.
C        POSITION               CONTENTS
C        1.      I.D. NUMBER FOR THE OPERATION=18
C        2.      LOCATION OF THE NEXT OPERATION IN THE T ARRAY
C        3.      LOCATION OF PARAMETERS IN THE P ARRAY
C        4.      LOCATION OF WORKING SPACE
C        5 TO    LOCATION OF EACH TIME SERIES TO BE PLOTTED IN THE D
C        NTTS+4       ARRAY (NTTS=NUMBER OF T.S. PLOTTED)
C.......................................
  199 CONTINUE
      RETURN
      END
