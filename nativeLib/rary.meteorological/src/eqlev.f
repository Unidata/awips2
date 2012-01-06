      SUBROUTINE EQLEV(P,HT,TP,TE,PLFC,EPTPAR,NPAR,PEQLEV,HEQLEV,TEQLEV)
      IMPLICIT NONE

C Statement of purpose.
C ---------------------
C This routine computes the pressure, height, and temperature of the
C equilibrium level, defined as the level where a parcel, rising
C from the level of free convection, intersects the sounding and becomes
C negatively buoyant.
C
C History.
C --------
C D. Baker       01 Jul 84    Original version.
C D. Perry       08 Oct 96    Adapted code for WFO;added temp_of_te routine
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C P           Real Array    Pressure levels of lifted parcel (mb).
C HT          Real Array    Heights of levels of lifted parcel (m asl).
C TP          Real Array    Lifted parcel temperatures at parcel levels (K).
C TE          Real Array    Environmental temperatures at parcel levels (K).
C PLFC        Real          Pressure of the LFC (mb).
C EPTPAR      Real          Equivalent potential temperature corresponding to
C                           the moist adiabat along which the saturated parcel
C                           rises (K).
C NPAR        Integer       Number of parcel levels passed.
C
C On output:
C ----------
C PEQLEV      Real          Equilibrium level pressure (mb).
C HEQLEV      Real          Equilibrium level height (m asl).
C TEQLEV      Real          Equilibrium level temperature (K).
C
C User note
C ---------
C 1) Refer to SUBROUTINE PARCEL for information on how parcel level
C    arrays are derived.
C

C---- Input arguments.

      INTEGER NPAR
      REAL P(NPAR),HT(NPAR),TP(NPAR),TE(NPAR),PLFC,EPTPAR

C---- Output arguments.

      REAL PEQLEV,HEQLEV,TEQLEV

C---- Internal variables.

      REAL TEM,TPM,ETPAR,PM,PB,PT,PLOG1,PLOG2,PLOG3
      INTEGER II,I,J,COUNT

C---- External functions.

      REAL INTERP1,TEMP_OF_TE

C---- Subroutine constants.

      REAL TOLER,FLG,FLAG
      PARAMETER (TOLER=0.1,FLG=99998.,FLAG=99999.)

C---- Check and see if the parcel is warmer than the sounding at the top
C---- of the sounding.  If so, assign appropriate flag value and exit.

      IF (TE(NPAR).LE.TP(NPAR)) THEN
       PEQLEV=FLAG
       HEQLEV=FLAG
       TEQLEV=FLAG
       GO TO 9999
      END IF

C---- Find the level of free convection to define end of search.  II
C---- denotes the array index for this level.
      ii=0
      DO I=1,NPAR
       IF (P(I).LE.PLFC) THEN
        II=I
        IF (II.EQ.NPAR) GO TO 9999
        GO TO 101
       END IF
      END DO
101   CONTINUE

C---- Beginning at the top of the sounding, move downward until the parcel
C---- temperature first becomes warmer than the environment.  Perform
C---- iteration to get the exact level.  This represents the highest
C---- equilibrium level in the sounding.

      DO J=NPAR-1,II,-1
       IF (TE(J).LE.TP(J)) THEN
        PT=P(J+1)
        PB=P(J)
        PLOG1=ALOG(PT)
        PLOG3=ALOG(PB)
        DO COUNT=1,100
         PM=0.5*(PT+PB)
         PLOG2=ALOG(PM)
         TEM=INTERP1(TE(J+1),TE(J),PLOG1,PLOG2,PLOG3)
         ETPAR=EPTPAR*((PM/1000.0)**0.286)
         TPM=TEMP_OF_TE(ETPAR,PM)
         IF (ABS(TPM-TEM).LT.TOLER) THEN
          PEQLEV=PM
          HEQLEV=INTERP1(HT(J+1),HT(J),PLOG1,PLOG2,PLOG3)
          TEQLEV=TEM
          GO TO 9999
         END IF
         IF ((TPM-TEM).GT.TOLER) PB=PM
         IF ((TEM-TPM).GT.TOLER) PT=PM
        END DO
       END IF
      END DO

C---- Exit.

9999  CONTINUE
      RETURN
      END
