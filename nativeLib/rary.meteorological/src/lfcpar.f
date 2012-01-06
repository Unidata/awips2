      SUBROUTINE LFCPAR(EPTPAR,PCB,TCB,HCB,T1,T2,P1,HT1,NPAR,
     +                  PLFC1,HLFC1,TLFC1,PLFC2,HLFC2,TLFC2)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C This routine computes the level of free convection of a rising parcel.
C
C History.
C --------                    
C Don Baker      01 Jun 85    Original version.
C Dale Perry        Oct 96    Adapted code to work with WFO
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C EPTPAR      Real          Moist adiabat along which parcel rises above
C                           the LCL (K).
C PCB         Real          LCL pressure (mb).
C TCB         Real          LCL temperature (K).
C HCB         Real          LCL height (m asl).
C T1          Real Array    Parcel temperatures at lifted parcel levels (K).
C T2          Real Array    Sounding temperatures at parcel levels (K).
C P1          Real Array    Lifted parcel pressure levels (mb).
C HT1         Real Array    Lifted parcel level heights (m asl).
C NPAR        Integer       Number of lifted parcel levels passed.
C
C On output:
C ----------               
C PLFC        Real          Level of free convection pressure (mb).
C HLFC        Real          Level of free convection height (m asl).
C TLFC        Real          Level of free convection temperature (K).
C
C
C Input arguments.
C
      INTEGER NPAR
      REAL EPTPAR,PCB,HCB,TCB,T1(NPAR),T2(NPAR),P1(NPAR),HT1(NPAR)
C
C Output arguments.
C
C      REAL PLFC,HLFC,TLFC
      REAL PLFC1,HLFC1,TLFC1
      REAL PLFC2,HLFC2,TLFC2
C
C Internal variables.
C
      REAL PM,PT,PB,ETPAR,T1M,T2M,PLOG1,PLOG2,PLOG3
      INTEGER I,J,K, COUNT,II
C
C External functions.
C
      REAL TEMP_OF_TE,INTERP1
C
C Subroutine constants.
C
      REAL TOLER,FLAG
      PARAMETER (TOLER=0.05,FLAG=99999.)
C
C Find the location in the parcel arrays that corresponds to the LCL.
C
      i=0
      DO 100 II=1,NPAR
         I=II
         IF (ABS(P1(I)-PCB).LT.0.1) GO TO 101
 100  CONTINUE
 101  IF (I.EQ.NPAR) GO TO 999
C
C Initially assign flag values to the LFC in case no buoyancy exists.
C
      PLFC1=FLAG
      HLFC1=FLAG
      TLFC1=FLAG
      PLFC2=FLAG
      HLFC2=FLAG
      TLFC2=FLAG
C
C Check and see if parcel is positively buoyant at the LCL already.  If
C this is true, then the LFC is coincident with the LCL.  This may be
C the case in 00Z soundings when a super-adiabatic layer exists near
C the surface.
C
      IF (T1(I).GE.T2(I)) THEN
         PLFC1=PCB
         HLFC1=HCB
         TLFC1=TCB
         GO TO 999
      ENDIF
C
C Loop upward from the LCL until the parcel temperature becomes warmer
C than the environment.  If this does not occur, no positive buoyancy
C exists and the routine exits with whatever flag value was assigned to
C the level of free convection.
C
C To prevent a stack out of bounds error when I=1, set it equal to the
C next level if I=1.
C
      IF (I.EQ.1) I=2
      DO 200 J=I,NPAR
         IF (T1(J).GE.T2(J)) THEN
            PT=P1(J)
            PB=P1(J-1)
            PLOG1=ALOG(P1(J))
            PLOG3=ALOG(P1(J-1))
            DO 102 COUNT=1,100
               PM=0.5*(PB+PT)
               PLOG2=ALOG(PM)
               ETPAR=EPTPAR*((PM/1000.0)**0.286)
               T1M=TEMP_OF_TE(ETPAR,PM)
               T2M=INTERP1(T2(J),T2(J-1),PLOG1,PLOG2,PLOG3)
               IF (ABS(T1M-T2M).LE.TOLER) THEN
                  PLFC1=PM
                  HLFC1=INTERP1(HT1(J),HT1(J-1),PLOG1,ALOG(PLFC1),PLOG3)
                  TLFC1=T1M
C                  GO TO 999
                  GO TO 300
               ENDIF
               IF ((T1M-T2M).GT.TOLER) PT=PM
               IF ((T2M-T1M).GT.TOLER) PB=PM
 102        CONTINUE
         ENDIF
 200  CONTINUE

C
C Continue looping to find a possible second LFC per conditions
C above rules.
C
 300  J=J+1
      DO 400 K=J,NPAR
         IF (T1(K).GE.T2(K)) THEN
            PT=P1(K)
            PB=P1(K-1)
            PLOG1=ALOG(P1(K))
            PLOG3=ALOG(P1(K-1))
            DO 302 COUNT=1,100
               PM=0.5*(PB+PT)
               PLOG2=ALOG(PM)
               ETPAR=EPTPAR*((PM/1000.0)**0.286)
               T1M=TEMP_OF_TE(ETPAR,PM)
               T2M=INTERP1(T2(K),T2(K-1),PLOG1,PLOG2,PLOG3)
               IF (ABS(T1M-T2M).LE.TOLER) THEN
                  PLFC2=PM
                  HLFC2=INTERP1(HT1(K),HT1(K-1),PLOG1,ALOG(PLFC2),PLOG3)
                  TLFC2=T1M
                  GO TO 999
               ENDIF
               IF ((T1M-T2M).GT.TOLER) PT=PM
               IF ((T2M-T1M).GT.TOLER) PB=PM
 302        CONTINUE
         ENDIF
 400  CONTINUE
         
C
C Exit.
C
 999  CONTINUE
      RETURN
      END
