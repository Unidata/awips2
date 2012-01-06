      SUBROUTINE EQP(DELTAP,P,HT,T,TD,N,PP,HTT,TT,TTD,NN)
      IMPLICIT NONE
C---------------------------------------------------------------------------
C Statement of purpose.
C ---------------------
C This routine derives sounding temperature and dew point data at pressure
C intervals given by DELTAP.
C
C History.
C --------                    
C D. Baker       27 Dec 85    Original version.
C
C Description of input and output.
C --------------------------------
C On input:
C ---------                
C DELTAP      Real          Pressure interval at which to compute T/Td.
C P           Real Array    Sounding pressures levels (mb).
C T           Real Array    Sounding temperatures (C).
C TD          Real Array    Sounding dew points (C).
C N           Integer       Number of uninterpolated sounding levels input.
C
C On output:
C ----------               
C PP          Real Array    New array of sounding pressures at DELTAP mb.
C TT          Real Array    New array of sounding temperatures at DELTAP mb.
C TTD         Real Array    New array of sounding dewpoints at DELTAP mb.
C NN          Integer       Number of new sounding levels returned.
C---------------------------------------------------------------------------
      REAL PP(1),P(1),HT(1),T(1),TD(1),HTT(1),TT(1),TTD(1)
      REAL P1,P2,P3,DELTAP
      REAL INTERP1
      INTEGER N,NN,I,J,K,PBOT,PTOP,PRES,PINC
      REAL FLAG
      PARAMETER (FLAG=99999.0)
C
C Construct PP output array every 10 mb.
C
      If (Mod(Nint(P(1)),Nint(deltap)).eq.0) Then
       pbot=Nint(P(1)-deltap)
      Else
       PBOT=NINT(P(1)-(MOD(NINT(P(1)),NINT(DELTAP))))
      End If
      PTOP=NINT(P(N))
      PINC=NINT(DELTAP)
      PP(1)=P(1)   !Initialize output pressure array to surface pressure.
      NN=1
      DO 300 PRES=PBOT,PTOP,-PINC
         NN=NN+1
         PP(NN)=FLOAT(PRES)
300   CONTINUE
C      DO I=1,NN
C       WRITE(6,*) I,PP(I)
C      END DO
C
C Derive temperature and dew point at the DELTAP mb intervals.
C
      J=2
      DO 200 K=1,NN
         DO 100 I=J,N
            IF (P(I).LE.PP(K) .AND. P(I-1).GE.PP(K)) THEN
               P1=ALOG(P(I-1))
               P2=ALOG(PP(K))
               P3=ALOG(P(I))
               HTT(K)=INTERP1(HT(I-1),HT(I),P1,P2,P3)
               TT(K)=INTERP1(T(I-1),T(I),P1,P2,P3)
               TTD(K)=INTERP1(TD(I-1),TD(I),P1,P2,P3)
               J=I
               GO TO 200
            END IF
100      CONTINUE
200   CONTINUE
      RETURN
      END
