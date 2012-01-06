      SUBROUTINE LCLPAR(MEANMIX,TS,P,HT,T,TD,NLVLS,PLCL,TLCL,HLCL)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes the pressure, height, and temperature of the
C lifting condensation level (LCL) from a sounding.
C
C History.
C --------                    
C Dale Perry   20 Sep 96    Bootlegged version of cclpar.f modified for
C                           determining the LCL.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C MEANMIX      Real         Mixing ratio used to intersect the sounding (g/kg).
C TS           Real         Surface temp (12Z-forecast max temp;00Z-sfc temp) (K). 
C P            Real Array   Sounding pressures (mb).
C HT           Real Array   Sounding heights (m asl).
C T            Real Array   Sounding temperatures (K).
C TD           Real Array   Sounding dewpoint temperatures (K).
C NLVLS        Integer      Number of sounding levels passed.
C
C On output:
C ----------               
C PLCL         Pressure of the lifting condensation level (mb).
C TLCL         Temperature of the lifting condensation level (K).
C HTLCL        Height of the lifting condensation level (m asl).
C
C User notes:
C -----------
C The low level mean mixing ratio is input to this routine...
C computed outside.
C
C
C Input arguments.
C
      INTEGER NLVLS
      REAL MEANMIX,TS,P(NLVLS),HT(NLVLS),T(NLVLS),TD(NLVLS)
C
C Output arguments.
C
      REAL PLCL,TLCL,HLCL
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
C Loop up through sounding until mixing ratio line crosses the
C dry adiabat through the surface temperature.  Initially set
C the LCL parameters to FLAG values in case no LCL is found.
C
      PLCL=FLAG
      HLCL=FLAG
      TLCL=FLAG
      T2=TS*((1000.0/P(1))**0.286)
      DO 100 I=1,NLVLS
         CALL TEMP_MIXRATIO(P(I),MEANMIX,T1)
         T1=T1*((1000.0/P(I))**0.286)
         IF (T1.GE.T2) GO TO 102
 100  CONTINUE
 102  CONTINUE
      IF (I.EQ.1) THEN   !LCL at the surface
         PLCL=P(1)
         HLCL=HT(1)
         TLCL=T(1)
         GO TO 999
      END IF
C We were at the top of the sounding, but 'I' got incremented one
C more beyond.  Reset it to the top of sounding index 'NLVLS'.
      IF (I.GT.NLVLS) I=NLVLS-1
      PT=P(I)
      PB=P(I-1)
      PLOG1=ALOG(P(I))
      PLOG3=ALOG(P(I-1))
C
C Iterate to find the LCL.  Keep cutting level in half until the
C point of intersection is found.
C
      DO 105 COUNT=1,100
         PM=0.5*(PT+PB)
         PLOG2=ALOG(PM)
         CALL TEMP_MIXRATIO(PM,MEANMIX,T1)
         T1=T1*((1000.0/PM)**0.286)
         IF (ABS(T1-T2).LE.TOLER) THEN
            PLCL=PM
            TLCL=T1*((PLCL/1000.0)**0.286)
            HLCL=INTERP1(HT(I),HT(I-1),PLOG1,ALOG(PLCL),PLOG3)
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
