      SUBROUTINE PARCEL(P,T,HT,TVIR,THETA,NLVLS,PCB,HCB,TCB,WCB,
     +                  THDPAR,EPTPAR,PL,TL,
     +                  PP,HTP,TP,THETAP,TVIRP,TE,THETAE,TVIRE,
     +                  NPAR)
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
C Don Baker      10 May 85    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C P           Real Array    Sounding pressure levels (mb).
C HT          Real Array    Sounding heights (m asl).
C T           Real Array    Sounding temperatures (C).
C TVIR        Real Array    Sounding virtual temperatures (C).
C THETA       Real Array    Sounding potential temperatures (C).
C NLVLS       Integer       Number of sounding levels passed.
C PCB         Real          LCL pressure (mb).
C HCB         Real          LCL height (m asl).
C TCB         Real          LCL temperature (C).
C WCB         Real          LCL mixing ration (g/kg).
C THDPAR      Real          LCL potential temperature (C).
C EPTPAR      Real          LCL equivalent potential temperature (C).
C PL          Real          Initial parcel pressure (mb).
C TL          Real          Initial parcel temperature (mb).
C
C On output:
C ----------               
C PP          Real Array    Lifted parcel pressures (mb).
C HTP         Real Array    Lifted parcel heights (m asl).
C TP          Real Array    Lifted parcel temperatures (C).
C THETAP      Real Array    Lifted parcel potential temperatures (C).
C TVIRP       Real Array    Lifted parcel virtual temperatures (C).
C TE          Real Array    Sounding temperatures corresponding to lifted
C                           parcel temperatures (C).
C THETAE      Real Array    Sounding potential temperatures corresponding
C                           to lifted parcel potential temperatures (C).
C TVIRE       Real Array    Sounding virtual temperatures corresponding
C                           to parcel virtual temperatures (C).
C NPAR        Integer       Number of lifted parcel levels returned.
C
C
C Input arguments.
C
      REAL P(1),T(1),HT(1),TVIR(1),THETA(1),PCB,HCB,TCB,WCB
      REAL THDPAR,EPTPAR
      REAL PL,TL
      INTEGER NLVLS
C
C Output arguments.
C
      REAL PP(1),TP(1),THETAP(1),TVIRP(1),HTP(1)
      REAL TE(1),THETAE(1),TVIRE(1)
      INTEGER NPAR
C
C Internal variables.
C
      REAL PLOG1,PLOG2,PLOG3,TD
      INTEGER J,K
C
C External functions.
C
      REAL INTERP,TV,TMR,O,TSA,TDA
C
C Step 1.  Assign first element of arrays to initial parcel values.
C this may involve interpolating environmental sounding data slightly
C to get data at mean parcel pressure.
C
C Note:  Subscript J refers to lifted parcel comparison arrays.
C        Subscript K refers to environmental sounding arrays.
C
      J=1
      PP(J)=PL
      TP(J)=TL
      THETAP(J)=THDPAR
      TD=TMR(WCB,PP(J))
      TVIRP(J)=TV(TP(J),TD,PP(J))
      DO 1 K=2,NLVLS
         IF (P(K).LE.PL) THEN
            PLOG1=ALOG(P(K-1))
            PLOG2=ALOG(PL)
            PLOG3=ALOG(P(K))
            TE(J)=INTERP(T(K-1),T(K),PLOG1,PLOG2,PLOG3)
            HTP(J)=INTERP(HT(K-1),HT(K),PLOG1,PLOG2,PLOG3)
            THETAE(J)=O(TE(J),PP(J))
            TVIRE(J)=INTERP(TVIR(K-1),TVIR(K),PLOG1,PLOG2,PLOG3)
            GO TO 2
         ENDIF
 1    CONTINUE
 2    IF (P(K).EQ.PL) K=K+1
C
C Step 2.  Compute coresponding arrays along the dry adiabat up to the LCL.
C
      DO 3 K=K,NLVLS
         IF (P(K).LE.PCB) GO TO 4
         J=J+1
         PP(J)=P(K)
         HTP(J)=HT(K)
         TP(J)=TDA(THDPAR,PP(J))
         THETAP(J)=THDPAR
         TD=TMR(WCB,PP(J))
         TVIRP(J)=TV(TP(J),TD,PP(J))
         TE(J)=T(K)
         THETAE(J)=THETA(K)
         TVIRE(J)=TVIR(K)
 3    CONTINUE
 4    CONTINUE
      IF (ABS(P(1)-PCB).LT.0.1) GO TO 211
C
C Step 3.  Compute corresponding array values at the LCL.
C
      J=J+1
      PP(J)=PCB
      HTP(J)=HCB
      TP(J)=TCB
      THETAP(J)=THDPAR
      TVIRP(J)=TV(TP(J),TP(J),PP(J))
      PLOG1=ALOG(P(K-1))
      PLOG2=ALOG(PCB)
      PLOG3=ALOG(P(K))
      TE(J)=INTERP(T(K-1),T(K),PLOG1,PLOG2,PLOG3)
      THETAE(J)=O(TE(J),PP(J))
      TVIRE(J)=INTERP(TVIR(K-1),TVIR(K),PLOG1,PLOG2,PLOG3)
      IF (P(K).EQ.PCB) K=K+1
C
C Step 4.  Compute corresponding array values from the LCL up to the
C top of the sounding.
C
211   DO 5 K=K,NLVLS
         J=J+1
         PP(J)=P(K)
         HTP(J)=HT(K)
         TP(J)=TSA(EPTPAR,PP(J))
         THETAP(J)=O(TP(J),PP(J))
         TVIRP(J)=TV(TP(J),TP(J),PP(J))
         TE(J)=T(K)
         THETAE(J)=THETA(K)
         TVIRE(J)=TVIR(K)
 5    CONTINUE
C
C Set number of parcel levels.
C
      NPAR=J
C
C Exit.
C
      RETURN
      END
