C MEMBER RO29
C  (from old member FCEX29)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/12/95.09:41:58 BY $WC20SV
C
C  ===================================================================
C  PGM:  ROUTINE RO29 (AI,PCPN,RO)
C
C   IN: AI     .... ANTECEDENT INDEX
C   IN: PCPN   .... STORM TOTAL RAIN
C  OUT: RO     .... STORM TOTAL RUNOFF
C  ===================================================================
C @PROCESS LVL(77)
      SUBROUTINE RO29(AI,PCPN,RO)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_api/RCS/ro29.f,v $
     . $',                                                             '
     .$Id: ro29.f,v 1.1 1995/09/17 18:58:35 dws Exp $
     . $' /
C    ===================================================================
C
C.....................................................................
C  THIS ROUTINE IS THE ANTECEDENT INDEX/RUNOFF RELATIONSHIP
C  FOR THE MISSOURI BASIN RFC (MBRFC) AND THE NORTH CENTRAL RFC
C  (NCRFC).
C
C.....................................................................
C  ROUTINE INITIALLY WRITTEN BY
C        TIM SWEENEY  -  OH, HYDROLOGIC SERVICES BRANCH       NOV 1991
C.....................................................................
C
      IF(AI.LE.0.0) THEN
        EN = 0.89
        D = 0.0
      ELSE
        EN = 0.89+(0.63*AI)
        D = 0.306*(AI**1.865)
      ENDIF
      RO = (((PCPN**EN)+(D**EN))**(1.0/EN))-D
      IF(RO.LT.0.0) RO = 0.0
      RETURN
      END
