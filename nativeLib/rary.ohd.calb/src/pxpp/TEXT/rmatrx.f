      SUBROUTINE RMATRX(NSTA,NMO,IMO,PX,CHAR,NFLAG,NSKIP,CM,IGM)
c
c specify the maximum number of stations allowed
c
      PARAMETER (NSX=500,MaxMonth=1200)
      DIMENSION PX(nsx,MaxMonth),CHAR(nsx,12),IG(nsx),R(nsx)
      DIMENSION  CM(nsx,nsx),IGM(nsx,nsx),NFLAG(MaxMonth)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/rmatrx.f,v $
     . $',                                                             '
     .$Id: rmatrx.f,v 1.3 2002/05/15 18:52:02 dws Exp $
     . $' /
C    ===================================================================
C
C     COMPUTE CORRELATION MATRIX AFTER ADJUSTING DATA FOR STATION
C          CHARACTERISTICS.
      DO 20 N=1,NSTA
      CM(N,N)=1.0
      IGM(N,N)=0
      J=N+1
      IF(J.GT.NSTA)GO TO 20
      DO 21 I=J,NSTA
      NC=0
      SN=0.0
      SI=0.0
      SN2=0.0
      SI2=0.0
      SNI=0.0
      DO 22 M=1,NMO
      IF(NFLAG(M).EQ.NSKIP)GO TO 22
      IF((PX(N,M).LT.0.0).OR.(PX(I,M).LT.0.0))GO TO 22
      JYR=(M-1)/12
      MO=M-JYR*12
      MO=IMO+MO-1
      IF(MO.GT.12)MO=MO-12
      PXN=PX(N,M)/CHAR(N,MO)
      PXI=PX(I,M)/CHAR(I,MO)
      NC=NC+1
      SN=SN+PXN
      SI=SI+PXI
      SN2=SN2+PXN*PXN
      SI2=SI2+PXI*PXI
      SNI=SNI+PXN*PXI
   22 CONTINUE
      IF(NC.LT.3)GO TO 23
      DEM=SQRT((NC*SN2-SN*SN)*(NC*SI2-SI*SI))
      IF(DEM.LT.0.0001)GO TO 23
      C=(NC*SNI-SN*SI)/DEM
      GO TO 24
   23 C=0.01
   24 IF (C.LT.0.01) C=.01
      CM(N,I)=C
      CM(I,N)=C
      IGM(N,I)=NC
      IGM(I,N)=NC
   21 CONTINUE
   20 CONTINUE
C     COMPUTE MATRIX OF STATION IDENTIFIERS IN ORDER BY DEGREE OF
C          CORRELATION
      DO 30 N=1,NSTA
      DO 31 I=1,NSTA
      IG(I)=0
      R(I)=0.0
   31 CONTINUE
      DO 32 I=1,NSTA
      IF(I.EQ.N)GO TO 32
      DO 33 II=1,NSTA
      IF(CM(N,I).LE.R(II))GO TO 33
      JJ=NSTA-II
C     MOVE ELEMENTS DOWN IN POSITION.
      DO 34 J=1,JJ
      M=NSTA-J
      R(M+1)=R(M)
      IG(M+1)=IG(M)
   34 CONTINUE
      R(II)=CM(N,I)
      IG(II)=I
      GO TO 32
   33 CONTINUE
   32 CONTINUE
      DO 35 I=1,NSTA
      IGM(N,I)=IG(I)
   35 CONTINUE
   30 CONTINUE
      RETURN
      END
