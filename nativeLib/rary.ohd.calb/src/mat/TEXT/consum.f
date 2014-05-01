C MODULE CONSUM
C-----------------------------------------------------------------------
C
      SUBROUTINE CONSUM (MONUM,NSTA,MXSTA,MXMON,
     *   AMAX,AMIN,NPG,NPLUS,IGS,NG,
     *   PH,PL,DMAX,DMIN,GAMAX,GAMIN,GAMAX_ARRAY,GAMIN_ARRAY)
C     
C  ROUTINE TO CALCULATE VALUES FOR MAT PROGRAM CONSISTENCY CHECK.    
C
      DIMENSION AMAX(MXSTA),AMIN(MXSTA)
      DIMENSION IGS(5,MXSTA),GAMAX(5),GAMIN(5),NPG(5),NPLUS(5)
      DIMENSION PH(MXSTA,2),PL(MXSTA,2)
      DIMENSION DMAX(MXSTA,MXMON),DMIN(MXSTA,MXMON)
      DIMENSION GAMAX_ARRAY(MXSTA,MXMON),GAMIN_ARRAY(MXSTA,MXMON)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/consum.f,v $
     . $',                                                             '
     .$Id: consum.f,v 1.4 2000/12/18 23:11:12 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (MONUM.GT.1) GO TO 20
C      
      DO 15 IRG=1,NSTA
         DO 10 I=1,2
            PH(IRG,I)=0.0
            PL(IRG,I)=0.0
10          CONTINUE
15       CONTINUE
C
20    MO=MONUM
C
C  COMPUTE ACCUMULATED MEAN FOR EACH GROUP
      DO 40 IG=1,NG
         GAMAX(IG)=0.0
         GAMIN(IG)=0.0
         N=NPG(IG)
         NP=NPLUS(IG)
         DO 30 J=1,N
            IF (IGS(IG,J).LT.0) GO TO 30
            IRG=IGS(IG,J)
            GAMAX(IG)=GAMAX(IG)+AMAX(IRG)/NP
            GAMIN(IG)=GAMIN(IG)+AMIN(IRG)/NP
30          CONTINUE
40       CONTINUE
C  COMPUTE DEVIATION OF EACH STATION FROM ITS GROUP BASE
      DO 80 IG=1,NG
         N=NPG(IG)
         P=NPLUS(IG)
         DO 70 J=1,N
            IRG=IGS(IG,J)
            IF (IRG.GT.0) GO TO 50
               IRG=-1*IRG
               DMAX(IRG,MO)=AMAX(IRG)-GAMAX(IG)
               DMIN(IRG,MO)=AMIN(IRG)-GAMIN(IG)
               GAMAX_ARRAY(IRG,MO)=GAMAX(IG)
               GAMIN_ARRAY(IRG,MO)=GAMIN(IG)
               GO TO 60
50          DMAX(IRG,MO)=AMAX(IRG)-(GAMAX(IG)-AMAX(IRG)/P)*(P/(P-1.0))
            DMIN(IRG,MO)=AMIN(IRG)-(GAMIN(IG)-AMIN(IRG)/P)*(P/(P-1.0))
            GAMAX_ARRAY(IRG,MO)= (GAMAX(IG)-AMAX(IRG)/P)*(P/(P-1.0))
            GAMIN_ARRAY(IRG,MO)= (GAMIN(IG)-AMIN(IRG)/P)*(P/(P-1.0))
60          IF (DMAX(IRG,MO).GT.PH(IRG,1))PH(IRG,1)=DMAX(IRG,MO)
            IF (DMAX(IRG,MO).LT.PL(IRG,1))PL(IRG,1)=DMAX(IRG,MO)
            IF (DMIN(IRG,MO).GT.PH(IRG,2))PH(IRG,2)=DMIN(IRG,MO)
            IF (DMIN(IRG,MO).LT.PL(IRG,2))PL(IRG,2)=DMIN(IRG,MO)
70          CONTINUE
80       CONTINUE
C
      RETURN
C
      END
