      SUBROUTINE NEGAREA(PCB,TCB,HCB,PLFC,HLFC,TLFC,THDPAR,EPTPAR,
     +                   P,HT,TE,TP,NPAR,CINFRMCAPE,NEGBUOY)
      IMPLICIT NONE

C Statement of purpose:
C ---------------------
C This routine computes the negative buoyant energy between the surface
C and level of free convection in a sounding.
C
C History:
C --------
C D. Baker   12 Dec 85    Original version.
C D. Perry      Oct 96    Adapted code for WFO applications
C
C Description of input and output.
C --------------------------------
C On input:
C ---------
C PLFC       Real       Pressure of the level of free convection (mb).
C TLFC       Real       Temperature of the LFC (C).
C HLFC       Real       Height of the LFC (m asl).
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
C NEGBUOY    Real       Negative energy between the level of the initial
C                       parcel and the level of free convection.
C
C User notes:
C -----------
C 1) This subroutine is called only if there exists a level of free convection.
C    This is determined in the calling module.
C 2) Lifted parcel arrays contain parcel and environmental parameters at
C    the same levels.  

C---- Input arguments.

      INTEGER NPAR
      REAL P(NPAR),HT(NPAR),TE(NPAR),TP(NPAR)
      REAL PCB,PLFC,TCB,TLFC,HCB,HLFC,EPTPAR,THDPAR
      REAL CINFRMCAPE

C---- Output arguments.

      REAL NEGBUOY

C---- Internal parameters.

      INTEGER NL
      PARAMETER (NL=500)
      REAL PARCEL,ENVMNT,SUM,DB,VDIF,NEG1,NEG2,ETPAR
      REAL PL(NL),HTL(NL),TEL(NL),TPL,THEL(NL),THPL(NL)
      INTEGER NPARL,I,IDXLCL

C---- External functions.

      REAL TEMP_OF_TE

C---- Subroutine constants.

      REAL GE
      PARAMETER (GE=9.80)

C---- Fill local arrays from first parcel level up to the LFC.

      NPARL=0
      DO I=1,NPAR
       IF (P(I).GT.PLFC) THEN
        PL(I)=P(I)
        HTL(I)=HT(I)
        TEL(I)=TE(I)
       ELSE
        PL(I)=PLFC
        HTL(I)=HLFC
        TEL(I)=TLFC
        NPARL=I
        GO TO 101
       ENDIF
      END DO
101   CONTINUE

C---- Compute resolution to which to interpolate local sounding pressures 
C---- and temperatures.  This is 1/25th of the thickness between the
C---- level of the parcel and its LFC.

      VDIF=(HLFC-HT(1))/25
      if (VDIF.lt.35) VDIF=35

C---- Interpolate data between the LFC and EL such that the thickness between
C---- any two levels is no greater than the resolution computed above.

      IF (NPARL.GT.1 .AND. HLFC-HT(1).GT.0)
     &    CALL INTPOS(VDIF,HTL,PL,TEL,NPARL)

C---- Find the index corresponding to the LCL.

      IDXLCL=0
      DO I=1,NPARL
       IF (ABS(PL(I)-PCB).LT.0.1) THEN
        IDXLCL=I
        GO TO 209
       END IF
      END DO
209   CONTINUE

C---- Compute potential temperatures of lifted parcel and environment at each
C---- level up to and including the LCL.  Compute this portion of the negative
C---- energy.

      SUM=0.0
      THEL(1)=TEL(1)*((1000.0/PL(1))**0.286)
      DO I=2,IDXLCL             
       THEL(I)=TEL(I)*((1000.0/PL(I))**0.286)
       ENVMNT=0.5*(THEL(I-1)+THEL(I))
       IF (THDPAR.LT.ENVMNT) THEN
        DB=((THDPAR-ENVMNT)/ENVMNT)*(HTL(I)-HTL(I-1))
        SUM=SUM+DB
       END IF
      END DO
      NEG1=GE*SUM

C---- Compute potential temperatures of lifted parcel and environment at each
C---- level between the LCL and the LFC.  Compute this portion of the negative
C---- energy.

      SUM=0.0
      TPL=THDPAR*((PL(IDXLCL)/1000.0)**0.286)
      THEL(IDXLCL)=TEL(IDXLCL)*((1000.0/PL(IDXLCL))**0.286)
      THPL(IDXLCL)=TPL*((1000.0/PL(IDXLCL))**0.286)
      DO I=IDXLCL+1,NPARL
       ETPAR=EPTPAR*((PL(I)/1000.0)**0.286)
       TPL=TEMP_OF_TE(ETPAR,PL(I))
       THEL(I)=TEL(I)*((1000.0/PL(I))**0.286)
       THPL(I)=TPL*((1000.0/PL(I))**0.286)
       PARCEL=0.5*(THPL(I-1)+THPL(I))
       ENVMNT=0.5*(THEL(I-1)+THEL(I))
       IF (PARCEL.LT.ENVMNT) THEN
        DB=((PARCEL-ENVMNT)/ENVMNT)*(HTL(I)-HTL(I-1))
        SUM=SUM+DB
       END IF
      END DO
      NEG2=GE*SUM

C---- Compute total negative buoyancy below the LFC.

      NEGBUOY=NEG1+NEG2+(CINFRMCAPE)

C---- Exit.

      RETURN
      END
