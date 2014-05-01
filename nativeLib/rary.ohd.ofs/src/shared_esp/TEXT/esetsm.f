C MEMBER ESETSM
C  (FROM OLD MEMBER EEDEX02)
C
C                             LAST UPDATE: 06/07/95.09:10:24 BY $WC30EW
C
      SUBROUTINE ESETSM(IPP,IDIST,XSS,DY,A,ILOC,PLOC,NYRS,NBYRS,NVAR,
     X   NUMT,NUM,IDTS)
       DIMENSION A(1),ILOC(6),PLOC(6),Y(50),P(50),NIP(51),
     X  IDTS(1),IPP(6,51,3)
      INCLUDE 'common/evar'
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/esetsm.f,v $
     . $',                                                             '
     .$Id: esetsm.f,v 1.1 1995/09/17 19:19:03 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      DO 2 I=1,51
 2    NIP(I)=0.
      DO 5 I=1,NUMT
      DO 5 J=1,51
      DO 5 K=1,3
 5    IPP(I,J,K)=0.
      DO 100 I=1,NUMT
      NY=NYRS
      IF (IDTS(I).EQ.5) NY=NBYRS
      II=PLOC(I)
      IF (NVPV(NVAR).EQ.2) GO TO 40
      CALL EDSPA(Y,A,NY,II,1,NVAR,IMIS,INOR)
      GO TO 80
 40   CALL EDSPA(Y,A,NY,II,1,NVAR,IMIS,INOR)
 80   CONTINUE
      CALL EFREQ(NY,Y,P)
      DO 90 J=1,NY
      Z=ECDFNI(P(J))
      CALL EIPOS(Z,K)
      L=1
      NL=51
      IF (DY.NE.0) L=(ETRAN(Y(J),IDIST)-XSS)/DY+.49+1
      L=NL-L+1
      IF (L-1) 51,52,52
 52   IF (L-NL) 53,53,54
 51   L=1
      GO TO 53
 54   L=NL
 53   NIP(L)=NIP(L)+1
      KK=NIP(L)
      IF (NIP(L).LE.3) GO TO 65
      NIP(L)=3
      GO TO 90
 65   IPP(I,L,KK)=K
 90   CONTINUE
 100  CONTINUE
      RETURN
      END
