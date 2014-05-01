C-----------------------------------------------------------------------
C   ONE OF THE PROGRAMS FOR KALMAN FILTER
C   IT DETERMINES JACOBIAN MATRIX D=[-DF/DX(j-1)]
C   F1 --- WEIGHTING FACTOR  0.5--1.0
C   NS --- NUMBER OF CROSS-SECTIONS
C   DT --- TIME STEP FOR j AND j-1
C   V  --- STATE Y(j-1) RELATED PROPERTIES AT EVERY CRESS SECTIONS
C-----------------------------------------------------------------------
         SUBROUTINE JAGD55(NS,Y,V,F1,DT,D,XX,K1,K16,JR,NUMLAD,LAD)
         DIMENSION D(2*NS,2*NS),Y(2*NS),V(10,NS),XX(NS),
     .    NUMLAD(K1),LAD(K16,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/jagd55.f,v $
     . $',                                                             '
     .$Id: jagd55.f,v 1.2 2004/02/02 21:50:00 jgofus Exp $
     . $' /
C    ===================================================================
C
         G=32.25
         DO 100 I=1,2*NS
         DO 100 J=1,2*NS
100      D(I,J)=0.0
         DO 200 L=1,NS-1
         DX=ABS(XX(L)-XX(L+1))*5279.0
         SE=V(7,L)*((Y(2*L+1)/V(2,L+1))**2-(Y(2*L-1)/V(2,L))**2)/
     *      (2.0*G*DX)     
         AA=0.5*(V(2,L)+V(2,L+1)) 

         D(2*L,2*L-1)=1.0-F1
         D(2*L,2*L)=0.5*DX*(V(8,L)+V(1,L))/DT
         D(2*L,2*L+1)=F1-1.0
         D(2*L,2*L+2)=0.5*DX*(V(8,L+1)+V(1,L+1))/DT
C------------------  LATERAL INFLOW RELATED  ---------------------------
         DLQ1=0.0
         DLQ2=0.0
         DLH1=0.0
         DLH2=0.0
C----------------------  SF  SE  RELATED  ------------------------------
         DSFQ=2.0*V(6,L)/(Y(2*L-1)+Y(2*L+1))
         DSFH=2.0*V(3,L)/3.0/(V(1,L+1)+V(1,L))-5.0*V(1,L)/6.0/AA
         DSFH=2.0*V(6,L)*DSFH
         DSEQ1=-V(7,L)*Y(2*L-1)/(G*DX*V(2,L)**2)
         DSEQ2=V(7,L)*Y(2*L+1)/(G*DX*V(2,L+1)**2)
         DSEH1=V(7,L)*Y(2*L-1)**2*V(1,L)/(G*DX*V(2,L)**3)
         DSEH2=-V(7,L)*Y(2*L+1)**2*V(1,L+1)/(G*DX*V(2,L+1)**3)
C-----------------------------------------------------------------------
         D(2*L+1,2*L-1)=0.5*DX/DT+(F1-1.0)*(-2.0*Y(2*L-1)/V(2,L)+G*AA*
     .   DX*(DSFQ+DSEQ1)+DX*DLQ1)
         D(2*L+1,2*L+1)=0.5*DX/DT+(F1-1.0)*(2.0*Y(2*L+1)/V(2,L+1)+G*AA*
     .   DX*(DSFQ+DSEQ2)+DX*DLQ2)
         D(2*L+1,2*L)=V(1,L)*(Y(2*L-1)/V(2,L))**2+0.5*G*V(1,L)*
     .   (Y(2*L+2)-Y(2*L)+DX*(V(6,L)+SE))+G*AA*(-1.0+DX*(DSFH+DSEH1))+
     .    DX*DLH1
         D(2*L+1,2*L)=(F1-1.0)*D(2*L+1,2*L)
         D(2*L+1,2*L+2)=-V(1,L+1)*(Y(2*L+1)/V(2,L+1))**2+0.5*G*V(1,L+1)*
     .   (Y(2*L+2)-Y(2*L)+DX*(V(6,L)+SE))+G*AA*(1.0+DX*(DSFH+DSEH2))+
     .    DX*DLH2
         D(2*L+1,2*L+2)=(F1-1.0)*D(2*L+1,2*L+2)
200      CONTINUE
C------------------   INTERNAL BOUNDARY  -------------------------------
      IF (NUMLAD(JR).EQ.0) GOTO 999
      DO 300 L=1,NUMLAD(JR)
      NSINTER=LAD(L,JR) 
      LL1=2*NSINTER
      LL2=2*NSINTER+1
      DO 300 I=1,2*NS
      D(LL1,I)=0.0
      D(LL2,I)=0.0
300   CONTINUE
C-----------------------------------------------------------------------
999      RETURN
         END
