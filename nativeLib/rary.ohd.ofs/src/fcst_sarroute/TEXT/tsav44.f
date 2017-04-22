C MEMBER TSAV44
C  (from old member FCEX44)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/95.16:29:02 BY $WC30RE
C
C @PROCESS LVL(77)
C
      FUNCTION TSAV44(QBEG,QEND,P)
C       FUNCTION TO COMPUTE AVERAGE FOR PERIOD
C       ROUTING TIME OF STORAGE FROM BEGIN AND END PERIOD Q
C       USING A TABLE OR EQUATION.

C     REQUIRED INPUT:
C       FUNCTION ARGUMENTS
C         QBEG  = FLOW IN CFS AT BEGINNING OF PERIOD.
C         QEND  = FLOW IN CFS AT END OF PERIOD.
C         P = CHARACTERISTIC ARRAY, CONTAINING EQUATION OR TABLE SPECS.
C     OUTPUT
C       FUNCTION TSAVG = AVERAGE FOR PERIOD ROUTING TIME OF STORAGE.

      REAL*4 LBQ
      DIMENSION P(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sarroute/RCS/tsav44.f,v $
     . $',                                                             '
     .$Id: tsav44.f,v 1.2 1996/03/21 14:22:39 page Exp $
     . $' /
C    ===================================================================
C

      XN=P(35)
      XKTS=P(36)
      I=NINT(P(37))     ! NUMBER OF POINTS IN Q-VS-TS TABLE

      IF(XN.EQ.0.) THEN ! N OF Q**N FORMULA IS 0, LOOK FOR TS IN TABLE.
C       TABLE IS SPECIFIED.
        IF(I.LE.0) THEN ! THERE IS NO TABLE
          TSAV44=0.
        ELSE
C           TABLE LOOKUP OF AVERAGE PERIOD TS.
          CALL STLU44(P,((QBEG+QEND)/2.),TSAV44)
        ENDIF
      ELSE
C       EQUATION IS SPECIFIED.
        LBQ = .00001
        IF(QBEG.GT.LBQ) THEN
          O1 = QBEG
        ELSE
C         LIMIT LOWER BOUNDS OF QBEG TO LARGE TS
          O1 = LBQ
        ENDIF
        IF(QEND.GT.LBQ) THEN
          O2 = QEND
        ELSE
C         LIMIT LOWER BOUNDS OF QEND TO LARGE TS
          O2 = LBQ
        ENDIF
        IF(ABS(O1-O2).LT.ABS(0.0001*O1)) THEN
C         NO CHANGE IN TS, NORMAL TS VS Q EQUATION.
          TSAV44 = XKTS/(O1**XN)
        ELSEIF(XN.EQ.1.) THEN
C         LINEAR Q VS TS RELATIONSHIP
          TSAV44 = (XKTS/2.)*((1./O1)+(1./O2))
        ELSE
C         NON-LINEAR RELATIONSHIP
          DS = XKTS*((O2**(1.-XN))-(O1**(1.-XN)))/(1.-XN)
          TSAV44 = DS/(O2-O1)
        ENDIF
      ENDIF
      RETURN
      END
