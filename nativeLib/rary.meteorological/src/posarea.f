      SUBROUTINE POSAREA(PLFC,PEQLEV,TLFC,TEQLEV,HLFC,HEQLEV,EPTPAR,
     +                   P,HT,TE,TP,NPAR,
     +                   BUOY,CIN)
      IMPLICIT NONE
C
C This routine computes the positive buoyant energy between the level of
C free convection and equilibrium level in a sounding.
C
C Don Baker   10 Jun 85    Original version.
C Dale Perry     Sep 96    Adapted code to work with WFO.  Removed the 
C                          interpolation routine, since not needed with
C                          WFO interpolated data sets. 
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C PLFC       Real       Pressure of the level of free convection (mb).
C PEQLEV     Real       Pressure of the equilibrium level (mb).
C TLFC       Real       Temperature of the LFC (C).
C TEQLEV     Real       Temperature of the EL (C).
C HLFC       Real       Height of the LFC (m asl).
C HEQLEV     Real       Height of the EL (m asl).
C EPTPAR     Real       Equivalent potential temperature of the moist
C                       adiabat above the lifted condensation level, along
C                       which the saturated parcel rises (C).
C P        Real Array   Lifted parcel pressure levels (mb).
C HT       Real Array   Lifted parcel heights (m asl).
C TE       Real Array   Sounding temperatures at parcel pressures (C).
C TP       Real Array   Parcel temperatures at parcel pressures (C).
C NPAR      Integer     Number of levels of lifted parcel data.
C
C On output:
C ----------
C BUOY       Real       Positive buoyant energy between the LFC and EL in
C                       joules per kilogram.
C NEGBUOY    Real       Negative energy between the level of the initial
C                       parcel and the equilibrium level.
C
C User notes:
C -----------
C 1) This subroutine is called only if there exists a level of free convection.
C    This is determined in the calling module.
C 2) Lifted parcel arrays contain parcel and environmental parameters at
C    the same levels.  The arrays begin at the lifted condensation level of 
C    the sounding and end at the top of the sounding.  The parcel arrays
C    are filled in a separate module.
C
C
C Input arguments.
C
      INTEGER NPAR
      REAL P(NPAR),HT(NPAR),TE(NPAR),TP(NPAR)
      REAL PLFC,PEQLEV,TLFC,TEQLEV,HLFC,HEQLEV,EPTPAR,PEQ,HEQ,TEQ
C
C Output arguments.
C
      REAL BUOY, CIN
C
C Internal parameters.
C
      INTEGER NL
      REAL FLG
      PARAMETER (NL=500,FLG=-99998.)
      REAL ETPAR,PARCEL,ENVMNT,SUM,DB,CINSUM,DB2
      REAL PL(NL),HTL(NL),TEL(NL),TPL,THEL(NL),THPL(NL)
      INTEGER NPARL,J,IDX1,IDX2,I
C
C External functions.
C
      REAL TEMP_OF_TE
C
C Subroutine constants.
C
      REAL GE
      PARAMETER (GE=9.80)
C
C Find the sounding level just below the LFC.
C
c   initialization of return value and idx
      CIN=0.0
      BUOY=FLG
      idx1=0

      DO 100 I=1,NPAR
         IF (P(I).LT.PLFC) THEN
            IDX1=I-1
            GO TO 10
         ENDIF
 100  CONTINUE
C
C Find the sounding level just above the EL.
C
10    PEQ=PEQLEV
      HEQ=HEQLEV
      TEQ=TEQLEV
      IF (PEQLEV.GT.-FLG) THEN
         PEQ=P(NPAR)
         HEQ=HT(NPAR)
         TEQ=TE(NPAR)
      END IF

      DO 101 J=I,NPAR
         IF (P(J).LT.PEQ) THEN
            IDX2=J
            GO TO 20
         ENDIF
 101  CONTINUE
      IDX2=NPAR      
 20   CONTINUE
C
C Initialize local parcel arrays to the LFC.
C
      PL(1) =PLFC
      HTL(1)=HLFC
      TEL(1)=TLFC
C
C Assign remaining local sounding arrays to sounding levels between the
C LFC and EL.
C
      NPARL=1
      DO 105 I=IDX1+1,IDX2-1
         NPARL=NPARL+1
         PL(NPARL)=P(I)
         HTL(NPARL)=HT(I)
         TEL(NPARL)=TE(I)
 105  CONTINUE
C
C Assign the last element of the local sounding arrays to the EL.
C
      NPARL=NPARL+1
      PL(NPARL)=PEQ
      HTL(NPARL)=HEQ
      TEL(NPARL)=TEQ
C
C Compute resolution to which to interpolate local sounding pressures 
C and temperatures.  This is 1/25th of the thickness between the
C LFC and EL.
C
C      VDIF=(HEQ-HLFC)/25
C
C Interpolate data between the LFC and EL such that the thickness between
C any two levels is no greater than the resolution computed above.
C
C      CALL INTPOS(VDIF,HTL,PL,TEL,NPARL)
C
C Compute lifted parcel potential temperature and sounding potential
C temperature at each level between the LFC and EL.
C
      DO 110 I=1,NPARL
         ETPAR=EPTPAR*((PL(I)/1000.0)**0.286)
         TPL=TEMP_OF_TE(ETPAR,PL(I))
         THEL(I)=TEL(I)*((1000.0/PL(I))**0.286)
         THPL(I)=TPL*((1000.0/PL(I))**0.286)
 110  CONTINUE
C
C Sum over all levels between the LFC and EL and compute positive buoyant
C energy.
C
      
      SUM=0.
      CINSUM=0.
      DO 120 I=2,NPARL
         PARCEL=0.5*(THPL(I-1)+THPL(I))
         ENVMNT=0.5*(THEL(I-1)+THEL(I))
         IF (PARCEL.GT.ENVMNT) THEN
            DB=((PARCEL-ENVMNT)/ENVMNT)*(HTL(I)-HTL(I-1))
            SUM=SUM+DB
         ELSE
            DB2=((PARCEL-ENVMNT)/ENVMNT)*(HTL(I)-HTL(I-1))
            CINSUM=CINSUM+DB2            
         ENDIF
 120  CONTINUE
      BUOY=GE*SUM
      CIN=GE*CINSUM
C
C Exit.
C
      RETURN
      END
