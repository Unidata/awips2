C MEMBER TSTR44
C  (from old member FCEX44)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 06/23/95.16:29:02 BY $WC30RE
C
C @PROCESS LVL(77)
C
      FUNCTION TSTR44(Q,P)
C     FUNCTION TO COMPUTE ROUTING TIME OF STORAGE FROM Q
C     USING A TABLE OR EQUATION.

C     REQUIRED INPUT:
C       FUNCTION ARGUMENTS
C         Q  = FLOW IN CFS.
C         P  = PARAMETER ARRAY, CONTAINING EQUATION OR TABLE SPECS.
C     OUTPUT
C       FUNCTION TSTR44 = ROUTING TIME OF STORAGE.

      DIMENSION P(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_sarroute/RCS/tstr44.f,v $
     . $',                                                             '
     .$Id: tstr44.f,v 1.2 1996/03/21 14:23:30 page Exp $
     . $' /
C    ===================================================================
C
      I=NINT(P(37)) ! NUMBER OF POINTS IN TABLE
      IF(P(35).EQ.0.) THEN ! N-VALUE OF EQUATION KTS/Q**N IS 0.
C       TABLE IS SPECIFIED.
        IF(I.LE.0) THEN
C         CHARACTERISTIC POINTER INCORRECT.
          TSTR44 = 0.
        ELSE
C           TABLE LOOKUP OF TS.
          CALL STLU44(P,Q,TS)
          TSTR44 = TS
        ENDIF
      ELSE
C           EQUATION IS SPECIFIED.
        XN = P(35)
        XKTS = P(36)
        IF(Q.EQ.0.) THEN
C         LIMIT LOWER BOUNDS TO A LARGE TS.
          TSTR44 = XKTS/(.00001**XN)
        ELSE
C         NORMAL TS VS Q EQUATION.
          TSTR44 = XKTS/(ABS(Q)**XN)
        ENDIF
      ENDIF
      RETURN
      END
