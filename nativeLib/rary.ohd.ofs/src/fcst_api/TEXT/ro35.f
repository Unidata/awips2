C MEMBER RO35
C  (from old member FCEX35)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/12/95.09:42:44 BY $WC20SV
C
C  =====================================================================
C  PGM:  ROUTINE RO35 (FI,PCPN,RO)
C
C   IN: FI     .... ANTECEDANT INDEX
C   IN: PCPN   .... STORM TOTAL RAIN
C  OUT: RO     .... STORM TOTAL RUNOFF
C  =====================================================================
C
C @PROCESS LVL(77)
C
      SUBROUTINE RO35 (FI,PCPN,RO)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/ro35.f,v $
     . $',                                                             '
     .$Id: ro35.f,v 1.1 1995/09/17 18:58:38 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................................................
C  THIS  SUBPROGRAM REPRESENTS THE FI-RAINFALL-RUNOFF
C  QUADRANT OF THE MARFC API OPERATION.
C
C.......................................................................
C  CONVERTED FROM A FUNCTION BY TIM SWEENEY, HRL - NOV 1993
C
C.......................................................................
C
      IF(PCPN.LE.0.0) GOTO 60
      D = 3.0+(0.5*FI)
      AN = 1.05+(0.17*(FI**1.7))
      RO = ((PCPN**AN+D**AN)**(1.0/AN))-D
      IF(PCPN.LT.5.0) GOTO 40
      IF(FI.GT.3.85) GOTO 20
      RO = RO+(0.0725*(FI+PCPN))-0.396
      GOTO 40
   20 IF(FI.LE.4.25) GOTO 40
      RO = RO-(0.1385*(FI+PCPN))+1.405
   40 IF(RO.GE.0.0) GOTO 80
   60 RO = 0.0
   80 RETURN
      END
