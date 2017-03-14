C MEMBER SCRB50
C  (from old member FCEX50)
C
C                             LAST UPDATE: 07/06/95.14:58:38 BY $WC21DT
C
C
C  **********   PRINTING REZULT ROUTINE
C @PROCESS LVL(77)
      SUBROUTINE SCRB50(NB,X,F1,ICS,M,MOPT,DELTF,VALUEF,STORE)
      REAL X(NB)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_assim/RCS/scrb50.f,v $
     . $',                                                             '
     .$Id: scrb50.f,v 1.1 1995/09/17 18:55:45 dws Exp $
     . $' /
C    ===================================================================
C
      IF (ICS.EQ.0) STORE=10
      ICS=ICS+5
      IF (ICS.GT.MOPT) ICS=2
      STORE = 10
      IF (ABS( (STORE-F1)/F1).LT.DELTF) ICS=2
      IF (F1.LT.VALUEF) ICS=2
      STORE=F1
  107 FORMAT(2X,3HF1=,F12.6,2X,2HM=,I5,/,(2X,2HX:,14F9.4))
      RETURN
      END
