      SUBROUTINE CCLPAR(MIX,P,HT,T,NLVLS,PCCL,TCCL,HCCL)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes the pressure, height, and temperature of the
C convective condensation level (CCL) from a sounding.
C
C History.
C --------                    
C Don Baker      15 Jun 85    Original version.
C Dale Perry        Sep 96    Adapted code for WFO
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C MIX         Real          Mixing ratio used to intersect the sounding (g/kg).
C P           Real Array    Sounding pressures (mb).
C HT          Real Array    Sounding heights (m asl).
C T           Real Array    Sounding temperatures (K).
C NLVLS       Integer       Number of sounding levels passed.
C
C On output:
C ----------               
C PCCL        Pressure of the convective condensation level (mb).
C TCCL        Temperature of the convective condensation level (K).
C HCCL        Height of the convective condensation level (m asl).
C
C User notes:
C -----------
C 1) The low level mean mixing ratio is input to this routine...
C    computed outside.
C 2) On days with a strong low level inversion, the convective
C    temperature may seem low because the strict definition is
C    used in the computation (i.e., where the low level mixing
C    ratio line first intersects the sounding).
C
C
C Input arguments.
C
      INTEGER NLVLS
      REAL MIX,P(NLVLS),HT(NLVLS),T(NLVLS)
C
C Output arguments.
C
      REAL PCCL,TCCL,HCCL
C
C Internal variables.
C
      REAL PB,PM,PT,T1,T2,PLOG1,PLOG2,PLOG3
      INTEGER I,COUNT
C
C External functions.
C
      REAL INTERP1
C
C Subroutine constants.
C
      REAL TOLER,FLAG
      PARAMETER (TOLER=0.05,FLAG=99999.)
C
C Loop up through sounding until mixing ratio line crosses environmental
C sounding line. Initially set the CCL parameters to FLAG values in case
C no CCL is found.
C
      PCCL=FLAG
      HCCL=FLAG
      TCCL=FLAG
      DO 100 I=1,NLVLS
         CALL TEMP_MIXRATIO(P(I),MIX,T1)
         T2=T(I)
         IF (T1.GE.T2) GO TO 102
 100  CONTINUE
 102  CONTINUE
      IF (I.EQ.1) THEN   !CCL at the surface
         PCCL=P(1)
         HCCL=HT(1)
         TCCL=T(1)
         GO TO 999
      END IF
      PT=P(I)
      PB=P(I-1)
      PLOG1=ALOG(P(I))
      PLOG3=ALOG(P(I-1))
C
C Iterate to find the CCL.  Keep cutting level in half until the
C point of intersection is found.
C
      DO 105 COUNT=1,100
         PM=0.5*(PT+PB)
         PLOG2=ALOG(PM)
         CALL TEMP_MIXRATIO(PM,MIX,T1)
         T2=INTERP1(T(I),T(I-1),PLOG1,PLOG2,PLOG3)
         IF (ABS(T1-T2).LE.TOLER) THEN
            PCCL=PM
            TCCL=T1
            HCCL=INTERP1(HT(I),HT(I-1),PLOG1,ALOG(PCCL),PLOG3)
            GO TO 999
         ENDIF
         IF (T1-T2.GT.TOLER) PT=PM
         IF (T2-T1.GT.TOLER) PB=PM
 105  CONTINUE
C
C Exit.
C
 999  CONTINUE
      RETURN
      END
