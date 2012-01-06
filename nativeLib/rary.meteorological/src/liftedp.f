      SUBROUTINE LIFTEDP(P,T,HT,TVIR,NLVLS,NPAR,PCB,HCB,TCB,WCB,
     +                   THDPAR,EPTPAR,PL,TL,PP,HTP,TP,TVIRP,TE,
     +                   TVIRE,NPARCEL)
      IMPLICIT NONE
C
C Statement of purpose.
C ---------------------
C Fill up lifted parcel arrays and corresponding environmental arrays
C such that an exact one-to-one correspondence exists between lifted
C parcel parameters and environmental parameters.
C
C History.
C --------                    
C Don Baker      10 May 85    Original version (parcel.f).
C Dale Perry        Sep 96    Adapted code to work with WFO, taking out
C                             extra output vars in parcel.f not needed
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C P           Real Array    Sounding pressure levels (mb).
C T           Real Array    Sounding temperatures (K).
C HT          Real Array    Sounding heights (m asl).
C TVIR        Real Array    Sounding virtual temperatures (K).
C NLVLS       Integer       Number of sounding levels passed.
C NPAR        Integer       Number of parcel levels to calculate
C PCB         Real          LCL pressure (mb).
C HCB         Real          LCL height (m asl).
C TCB         Real          LCL temperature (K).
C WCB         Real          LCL mixing ration (g/kg).
C THDPAR      Real          LCL potential temperature (K).
C EPTPAR      Real          LCL equivalent potential temperature (K).
C PL          Real          Initial parcel pressure (mb).
C TL          Real          Initial parcel temperature (mb).
C
C On output:
C ----------               
C PP          Real Array    Lifted parcel pressures (mb).
C HTP         Real Array    Lifted parcel heights (m asl).
C TP          Real Array    Lifted parcel temperatures (K).
C TVIRP       Real Array    Lifted parcel virtual temperatures (K).
C TE          Real Array    Sounding temperatures corresponding to lifted
C                           parcel temperatures (K).
C TVIRE       Real Array    Sounding virtual temperatures corresponding
C                           to lifted parcel virtual temperatures (K).
C NPARCEL     Integer       Actual number of lifted parcel lvls returned.
C
C
C Input arguments.
C
      INTEGER NLVLS, NPAR
      REAL P(NLVLS),T(NLVLS),HT(NLVLS),TVIR(NLVLS),PCB,HCB,TCB,WCB
      REAL THDPAR,EPTPAR
      REAL PL,TL
C
C Output arguments.
C
      REAL PP(NPAR),TP(NPAR),HTP(NPAR),TVIRP(NPAR)
      REAL TE(NPAR),TVIRE(NPAR)
      INTEGER NPARCEL
C
C Internal variables.
C
      REAL PLOG1,PLOG2,PLOG3,TDP,ETPAR
      INTEGER J,K,IK
C
C External functions.
C
      EXTERNAL INTERP1, VIRTTEMP, TEMP_MIXRATIO, TEMP_OF_TE
      REAL     INTERP1, VIRTTEMP,                TEMP_OF_TE

C
C Step 1.  Assign first element of arrays to initial parcel values.
C this may involve interpolating environmental sounding data slightly
C to get data at mean parcel pressure.
C
C Note:  Subscript J refers to lifted parcel comparison arrays.
C        Subscript K refers to environmental sounding arrays.
C
      J=0
C
C Step 1.  Compute corresponding arrays along the dry adiabat up to the LCL.
C
      IF (PCB   .GT. 99998.0) GO TO 300
      
      k=1
      DO 3 IK=1,NLVLS
         K=IK
         IF (P(K).GT.PL)  GO TO 3
         IF (P(K).LE.PCB) GO TO 4
         J=J+1
         PP(J)=P(K)
         HTP(J)=HT(K)
         TP(J)=THDPAR*((PP(J)/1000.0)**0.286)
         CALL TEMP_MIXRATIO(PP(J),WCB,TDP)
         TVIRP(J) =VIRTTEMP(TP(J), TDP, PP(J))
         TE(J)=T(K)
         TVIRE(J)=TVIR(K)
 3    CONTINUE
 4    CONTINUE
      IF (ABS(P(1)-PCB).LT.0.1) GO TO 211
C
C Step 2.  Compute array values at the LCL.
C
      J=J+1
      PP(J)=PCB
      HTP(J)=HCB
      TP(J)=TCB
      TVIRP(J)=VIRTTEMP(TP(J),TP(J),PP(J))

C
C Case where the LCL is at the surface
C
      IF (J.EQ.1) THEN
         TE(J)= T(K)
         TVIRE(J)= TVIR(K)
C
C LCL is above the surface
C
      else
         PLOG1=ALOG(P(K-1))
         PLOG2=ALOG(PCB)
         PLOG3=ALOG(P(K))
         TE(J)=INTERP1(T(K-1),T(K),PLOG1,PLOG2,PLOG3)
         TVIRE(J)=INTERP1(TVIR(K-1),TVIR(K),PLOG1,PLOG2,PLOG3)
      END IF

      IF (P(K).EQ.PCB) K=K+1
C
C Step 3.  Compute corresponding array values from the LCL up to the
C top of the sounding.
C

 211  CONTINUE
      DO 5 K=K,NLVLS
         J=J+1
         PP(J)=P(K)
         HTP(J)=HT(K)
         ETPAR=EPTPAR*((PP(J)/1000.0)**0.286)
         TP(J)=TEMP_OF_TE(ETPAR,PP(J))
         TVIRP(J)=VIRTTEMP(TP(J),TP(J),PP(J))
         TE(J)=T(K)
         TVIRE(J)=TVIR(K)
 5    CONTINUE
C
C Set number of parcel levels.
C
 300  NPARCEL=J
C
C Exit.
C
      RETURN
      END





