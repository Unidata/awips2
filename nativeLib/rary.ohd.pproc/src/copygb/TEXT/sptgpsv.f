C-----------------------------------------------------------------------
      SUBROUTINE SPTGPSV(IROMB,MAXWV,KMAX,NPS,
     &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                   TRUE,XMESH,ORIENT,WAVED,WAVEZ,UN,VN,US,VS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPSV    TRANSFORM SPECTRAL VECTOR TO POLAR STEREO.
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF DIVERGENCES AND CURLS
C           TO VECTOR FIELDS ON A PAIR OF POLAR STEREOGRAPHIC GRIDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TWO SQUARE POLAR STEREOGRAPHIC GRIDS ARE CENTERED
C           ON THE RESPECTIVE POLES, WITH THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.
C           THE VECTORS ARE AUTOMATICALLY ROTATED TO BE RESOLVED
C           RELATIVE TO THE RESPECTIVE POLAR STEREOGRAPHIC GRIDS.
C
C           THE TRANSFORM IS MADE EFFICIENT             \ 4 | 5 /
C           BY COMBINING POINTS IN EIGHT SECTORS         \  |  /
C           OF EACH POLAR STEREOGRAPHIC GRID,           3 \ | / 6
C           NUMBERED AS IN THE DIAGRAM AT RIGHT.           \|/
C           THE POLE AND THE SECTOR BOUNDARIES          ----+----
C           ARE TREATED SPECIALLY IN THE CODE.             /|\
C           UNFORTUNATELY, THIS APPROACH INDUCES        2 / | \ 7
C           SOME HAIRY INDEXING AND CODE LOQUACITY,      /  |  \
C           FOR WHICH THE DEVELOPER APOLOGIZES.         / 1 | 8 \
C
C           THE TRANSFORMS ARE ALL MULTIPROCESSED OVER SECTOR POINTS.
C           TRANSFORM SEVERAL FIELDS AT A TIME TO IMPROVE VECTORIZATION.
C           SUBPROGRAM CAN BE CALLED FROM A MULTIPROCESSING ENVIRONMENT.
C
C PROGRAM HISTORY LOG:
C   96-02-29  IREDELL
C 1998-12-15  IREDELL  OPENMP DIRECTIVES INSERTED
C
C USAGE:    CALL SPTGPSV(IROMB,MAXWV,KMAX,NPS,
C    &                   KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                   TRUE,XMESH,ORIENT,WAVED,WAVEZ,UN,VN,US,VS)
C   INPUT ARGUMENTS:
C     IROMB    - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     MAXWV    - INTEGER SPECTRAL TRUNCATION
C     KMAX     - INTEGER NUMBER OF FIELDS TO TRANSFORM.
C     NPS      - INTEGER ODD ORDER OF THE POLAR STEREOGRAPHIC GRIDS
C     KWSKIP   - INTEGER SKIP NUMBER BETWEEN WAVE FIELDS
C                (DEFAULTS TO (MAXWV+1)*((IROMB+1)*MAXWV+2) IF KWSKIP=0)
C     KGSKIP   - INTEGER SKIP NUMBER BETWEEN GRID FIELDS
C                (DEFAULTS TO NPS*NPS IF KGSKIP=0)
C     NISKIP   - INTEGER SKIP NUMBER BETWEEN GRID I-POINTS
C                (DEFAULTS TO 1 IF NISKIP=0)
C     NJSKIP   - INTEGER SKIP NUMBER BETWEEN GRID J-POINTS
C                (DEFAULTS TO NPS IF NJSKIP=0)
C     TRUE     - REAL LATITUDE AT WHICH PS GRID IS TRUE (USUALLY 60.)
C     XMESH    - REAL GRID LENGTH AT TRUE LATITUDE (M)
C     ORIENT   - REAL LONGITUDE AT BOTTOM OF NORTHERN PS GRID
C                (SOUTHERN PS GRID WILL HAVE OPPOSITE ORIENTATION.)
C     WAVED    - REAL (*) WAVE DIVERGENCE FIELDS
C     WAVEZ    - REAL (*) WAVE VORTICITY FIELDS
C   OUTPUT ARGUMENTS:
C     UN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC U-WINDS
C     VN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC V-WINDS
C     US       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC U-WINDS
C     VS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC V-WINDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C   SPDZ2UV      COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL WAVED(*),WAVEZ(*),UN(*),VN(*),US(*),VS(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      INTEGER MP(2*KMAX)
      REAL SLON(MAXWV,8),CLON(MAXWV,8),SROT(0:3),CROT(0:3)
      REAL W((MAXWV+1)*((IROMB+1)*MAXWV+2)/2*2+1,2*KMAX)
      REAL WTOP(2*(MAXWV+1),2*KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),PLNTOP(MAXWV+1)
      REAL F(2*MAXWV+3,2,2*KMAX)
      DATA SROT/0.,1.,0.,-1./,CROT/1.,0.,-1.,0./
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      MDIM=2*MX+1
      IDIM=2*MAXWV+3
      KW=KWSKIP
      KG=KGSKIP
      NI=NISKIP
      NJ=NJSKIP
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=NPS*NPS
      IF(NI.EQ.0) NI=1
      IF(NJ.EQ.0) NJ=NPS
      MP=1
      NPH=(NPS-1)/2
      GQ=((1.+SIN(TRUE/DPR))*RERTH/XMESH)**2
      SRH=SQRT(0.5)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE SPECTRAL WINDS
C$OMP PARALLEL DO PRIVATE(KWS)
      DO K=1,KMAX
        KWS=(K-1)*KW
        CALL SPDZ2UV(IROMB,MAXWV,ENN1,ELONN1,EON,EONTOP,
     &               WAVED(KWS+1),WAVEZ(KWS+1),
     &               W(1,K),W(1,KMAX+K),WTOP(1,K),WTOP(1,KMAX+K))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE POLE POINT
      I1=NPH+1
      J1=NPH+1
      IJ1=(I1-1)*NI+(J1-1)*NJ+1
      SLAT1=1.
      CLAT1=0.
      CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &              PLN,PLNTOP)
      CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,MDIM,2*MXTOP,2*KMAX,
     &             CLAT1,PLN,PLNTOP,MP,W,WTOP,F)
      COSO=COS(ORIENT/DPR)
      SINO=SIN(ORIENT/DPR)
CDIR$ IVDEP
      DO K=1,KMAX
        KU=K
        KV=K+KMAX
        IJK1=IJ1+(K-1)*KG
        UN(IJK1)=2*( COSO*F(3,1,KU)+SINO*F(3,1,KV))
        VN(IJK1)=2*(-SINO*F(3,1,KU)+COSO*F(3,1,KV))
        US(IJK1)=2*( COSO*F(3,2,KU)-SINO*F(3,2,KV))
        VS(IJK1)=2*( SINO*F(3,2,KU)+COSO*F(3,2,KV))
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE POINTS ALONG THE ROW AND COLUMN OF THE POLE,
C  STARTING AT THE ORIENTATION LONGITUDE AND GOING CLOCKWISE.
C$OMP PARALLEL DO PRIVATE(I1,J2,I2,J3,I3,J4,I4,J5,I5,J6,I6,J7,I7,J8,I8)
C$OMP&       PRIVATE(IJ1,IJ2,IJ3,IJ4,IJ5,IJ6,IJ7,IJ8)
C$OMP&       PRIVATE(IJK1,IJK2,IJK3,IJK4,IJK5,IJK6,IJK7,IJK8)
C$OMP&       PRIVATE(DJ1,DI1,RQ,RR,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,KU,KV,LR,LI)
      DO J1=1,NPH
        I1=NPH+1
        RADLON=ORIENT/DPR
        J3=NPS+1-I1
        I3=J1
        J5=NPS+1-J1
        I5=NPS+1-I1
        J7=I1
        I7=NPS+1-J1
        IJ1=(I1-1)*NI+(J1-1)*NJ+1
        IJ3=(I3-1)*NI+(J3-1)*NJ+1
        IJ5=(I5-1)*NI+(J5-1)*NJ+1
        IJ7=(I7-1)*NI+(J7-1)*NJ+1
        DI1=I1-NPH-1
        DJ1=J1-NPH-1
        RQ=DI1**2+DJ1**2
        SLAT1=(GQ-RQ)/(GQ+RQ)
        CLAT1=SQRT(1.-SLAT1**2)
        CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &                PLN,PLNTOP)
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,MDIM,2*MXTOP,2*KMAX,
     &               CLAT1,PLN,PLNTOP,MP,W,WTOP,F)
        DO L=1,MAXWV
          SLON(L,1)=SIN(L*RADLON)
          CLON(L,1)=COS(L*RADLON)
          SLON(L,3)=SLON(L,1)*CROT(MOD(1*L,4))
     &             -CLON(L,1)*SROT(MOD(1*L,4))
          CLON(L,3)=CLON(L,1)*CROT(MOD(1*L,4))
     &             +SLON(L,1)*SROT(MOD(1*L,4))
          SLON(L,5)=SLON(L,1)*CROT(MOD(2*L,4))
     &             -CLON(L,1)*SROT(MOD(2*L,4))
          CLON(L,5)=CLON(L,1)*CROT(MOD(2*L,4))
     &             +SLON(L,1)*SROT(MOD(2*L,4))
          SLON(L,7)=SLON(L,1)*CROT(MOD(3*L,4))
     &             -CLON(L,1)*SROT(MOD(3*L,4))
          CLON(L,7)=CLON(L,1)*CROT(MOD(3*L,4))
     &             +SLON(L,1)*SROT(MOD(3*L,4))
        ENDDO
CDIR$ IVDEP
        DO K=1,KMAX
          KU=K
          KV=K+KMAX
          IJK1=IJ1+(K-1)*KG
          IJK3=IJ3+(K-1)*KG
          IJK5=IJ5+(K-1)*KG
          IJK7=IJ7+(K-1)*KG
          UN(IJK1)= F(1,1,KU)
          VN(IJK1)= F(1,1,KV)
          UN(IJK3)= F(1,1,KV)
          VN(IJK3)=-F(1,1,KU)
          UN(IJK5)=-F(1,1,KU)
          VN(IJK5)=-F(1,1,KV)
          UN(IJK7)=-F(1,1,KV)
          VN(IJK7)= F(1,1,KU)
          US(IJK1)=-F(1,2,KU)
          VS(IJK1)=-F(1,2,KV)
          US(IJK3)=-F(1,2,KV)
          VS(IJK3)= F(1,2,KU)
          US(IJK5)= F(1,2,KU)
          VS(IJK5)= F(1,2,KV)
          US(IJK7)= F(1,2,KV)
          VS(IJK7)=-F(1,2,KU)
        ENDDO
        IF(KMAX.EQ.1) THEN
          KU=1
          KV=2
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
            UN(IJ1)=UN(IJ1)+2*(F(LR,1,KU)*CLON(L,1)
     &                        -F(LI,1,KU)*SLON(L,1))
            VN(IJ1)=VN(IJ1)+2*(F(LR,1,KV)*CLON(L,1)
     &                        -F(LI,1,KV)*SLON(L,1))
            UN(IJ3)=UN(IJ3)+2*(F(LR,1,KV)*CLON(L,3)
     &                        -F(LI,1,KV)*SLON(L,3))
            VN(IJ3)=VN(IJ3)-2*(F(LR,1,KU)*CLON(L,3)
     &                        -F(LI,1,KU)*SLON(L,3))
            UN(IJ5)=UN(IJ5)-2*(F(LR,1,KU)*CLON(L,5)
     &                        -F(LI,1,KU)*SLON(L,5))
            VN(IJ5)=VN(IJ5)-2*(F(LR,1,KV)*CLON(L,5)
     &                        -F(LI,1,KV)*SLON(L,5))
            UN(IJ7)=UN(IJ7)-2*(F(LR,1,KV)*CLON(L,7)
     &                        -F(LI,1,KV)*SLON(L,7))
            VN(IJ7)=VN(IJ7)+2*(F(LR,1,KU)*CLON(L,7)
     &                        -F(LI,1,KU)*SLON(L,7))
            US(IJ1)=US(IJ1)-2*(F(LR,2,KU)*CLON(L,5)
     &                        -F(LI,2,KU)*SLON(L,5))
            VS(IJ1)=VS(IJ1)-2*(F(LR,2,KV)*CLON(L,5)
     &                        -F(LI,2,KV)*SLON(L,5))
            US(IJ3)=US(IJ3)-2*(F(LR,2,KV)*CLON(L,3)
     &                        -F(LI,2,KV)*SLON(L,3))
            VS(IJ3)=VS(IJ3)+2*(F(LR,2,KU)*CLON(L,3)
     &                        -F(LI,2,KU)*SLON(L,3))
            US(IJ5)=US(IJ5)+2*(F(LR,2,KU)*CLON(L,1)
     &                        -F(LI,2,KU)*SLON(L,1))
            VS(IJ5)=VS(IJ5)+2*(F(LR,2,KV)*CLON(L,1)
     &                        -F(LI,2,KV)*SLON(L,1))
            US(IJ7)=US(IJ7)+2*(F(LR,2,KV)*CLON(L,7)
     &                        -F(LI,2,KV)*SLON(L,7))
            VS(IJ7)=VS(IJ7)-2*(F(LR,2,KU)*CLON(L,7)
     &                        -F(LI,2,KU)*SLON(L,7))
          ENDDO
        ELSE
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
CDIR$ IVDEP
            DO K=1,KMAX
              KU=K
              KV=K+KMAX
              IJK1=IJ1+(K-1)*KG
              IJK3=IJ3+(K-1)*KG
              IJK5=IJ5+(K-1)*KG
              IJK7=IJ7+(K-1)*KG
              UN(IJK1)=UN(IJK1)+2*(F(LR,1,KU)*CLON(L,1)
     &                            -F(LI,1,KU)*SLON(L,1))
              VN(IJK1)=VN(IJK1)+2*(F(LR,1,KV)*CLON(L,1)
     &                            -F(LI,1,KV)*SLON(L,1))
              UN(IJK3)=UN(IJK3)+2*(F(LR,1,KV)*CLON(L,3)
     &                            -F(LI,1,KV)*SLON(L,3))
              VN(IJK3)=VN(IJK3)-2*(F(LR,1,KU)*CLON(L,3)
     &                            -F(LI,1,KU)*SLON(L,3))
              UN(IJK5)=UN(IJK5)-2*(F(LR,1,KU)*CLON(L,5)
     &                            -F(LI,1,KU)*SLON(L,5))
              VN(IJK5)=VN(IJK5)-2*(F(LR,1,KV)*CLON(L,5)
     &                            -F(LI,1,KV)*SLON(L,5))
              UN(IJK7)=UN(IJK7)-2*(F(LR,1,KV)*CLON(L,7)
     &                            -F(LI,1,KV)*SLON(L,7))
              VN(IJK7)=VN(IJK7)+2*(F(LR,1,KU)*CLON(L,7)
     &                            -F(LI,1,KU)*SLON(L,7))
              US(IJK1)=US(IJK1)-2*(F(LR,2,KU)*CLON(L,5)
     &                            -F(LI,2,KU)*SLON(L,5))
              VS(IJK1)=VS(IJK1)-2*(F(LR,2,KV)*CLON(L,5)
     &                            -F(LI,2,KV)*SLON(L,5))
              US(IJK3)=US(IJK3)-2*(F(LR,2,KV)*CLON(L,3)
     &                            -F(LI,2,KV)*SLON(L,3))
              VS(IJK3)=VS(IJK3)+2*(F(LR,2,KU)*CLON(L,3)
     &                            -F(LI,2,KU)*SLON(L,3))
              US(IJK5)=US(IJK5)+2*(F(LR,2,KU)*CLON(L,1)
     &                            -F(LI,2,KU)*SLON(L,1))
              VS(IJK5)=VS(IJK5)+2*(F(LR,2,KV)*CLON(L,1)
     &                            -F(LI,2,KV)*SLON(L,1))
              US(IJK7)=US(IJK7)+2*(F(LR,2,KV)*CLON(L,7)
     &                            -F(LI,2,KV)*SLON(L,7))
              VS(IJK7)=VS(IJK7)-2*(F(LR,2,KU)*CLON(L,7)
     &                            -F(LI,2,KU)*SLON(L,7))
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE POINTS ON THE MAIN DIAGONALS THROUGH THE POLE,
C  STARTING CLOCKWISE OF THE ORIENTATION LONGITUDE AND GOING CLOCKWISE.
C$OMP PARALLEL DO PRIVATE(I1,J2,I2,J3,I3,J4,I4,J5,I5,J6,I6,J7,I7,J8,I8)
C$OMP&       PRIVATE(IJ1,IJ2,IJ3,IJ4,IJ5,IJ6,IJ7,IJ8)
C$OMP&       PRIVATE(IJK1,IJK2,IJK3,IJK4,IJK5,IJK6,IJK7,IJK8)
C$OMP&       PRIVATE(DJ1,DI1,RQ,RR,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,KU,KV,LR,LI)
      DO J1=1,NPH
        I1=J1
        RADLON=(ORIENT-45)/DPR
        J3=NPS+1-I1
        I3=J1
        J5=NPS+1-J1
        I5=NPS+1-I1
        J7=I1
        I7=NPS+1-J1
        IJ1=(I1-1)*NI+(J1-1)*NJ+1
        IJ3=(I3-1)*NI+(J3-1)*NJ+1
        IJ5=(I5-1)*NI+(J5-1)*NJ+1
        IJ7=(I7-1)*NI+(J7-1)*NJ+1
        DI1=I1-NPH-1
        DJ1=J1-NPH-1
        RQ=DI1**2+DJ1**2
        SLAT1=(GQ-RQ)/(GQ+RQ)
        CLAT1=SQRT(1.-SLAT1**2)
        CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &                PLN,PLNTOP)
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,MDIM,2*MXTOP,2*KMAX,
     &               CLAT1,PLN,PLNTOP,MP,W,WTOP,F)
        DO L=1,MAXWV
          SLON(L,1)=SIN(L*RADLON)
          CLON(L,1)=COS(L*RADLON)
          SLON(L,3)=SLON(L,1)*CROT(MOD(1*L,4))
     &             -CLON(L,1)*SROT(MOD(1*L,4))
          CLON(L,3)=CLON(L,1)*CROT(MOD(1*L,4))
     &             +SLON(L,1)*SROT(MOD(1*L,4))
          SLON(L,5)=SLON(L,1)*CROT(MOD(2*L,4))
     &             -CLON(L,1)*SROT(MOD(2*L,4))
          CLON(L,5)=CLON(L,1)*CROT(MOD(2*L,4))
     &             +SLON(L,1)*SROT(MOD(2*L,4))
          SLON(L,7)=SLON(L,1)*CROT(MOD(3*L,4))
     &             -CLON(L,1)*SROT(MOD(3*L,4))
          CLON(L,7)=CLON(L,1)*CROT(MOD(3*L,4))
     &             +SLON(L,1)*SROT(MOD(3*L,4))
        ENDDO
CDIR$ IVDEP
        DO K=1,KMAX
          KU=K
          KV=K+KMAX
          IJK1=IJ1+(K-1)*KG
          IJK3=IJ3+(K-1)*KG
          IJK5=IJ5+(K-1)*KG
          IJK7=IJ7+(K-1)*KG
          UN(IJK1)=SRH*( F(1,1,KU)+F(1,1,KV))
          VN(IJK1)=SRH*(-F(1,1,KU)+F(1,1,KV))
          UN(IJK3)=SRH*(-F(1,1,KU)+F(1,1,KV))
          VN(IJK3)=SRH*(-F(1,1,KU)-F(1,1,KV))
          UN(IJK5)=SRH*(-F(1,1,KU)-F(1,1,KV))
          VN(IJK5)=SRH*( F(1,1,KU)-F(1,1,KV))
          UN(IJK7)=SRH*( F(1,1,KU)-F(1,1,KV))
          VN(IJK7)=SRH*( F(1,1,KU)+F(1,1,KV))
          US(IJK1)=SRH*(-F(1,2,KU)-F(1,2,KV))
          VS(IJK1)=SRH*( F(1,2,KU)-F(1,2,KV))
          US(IJK3)=SRH*( F(1,2,KU)-F(1,2,KV))
          VS(IJK3)=SRH*( F(1,2,KU)+F(1,2,KV))
          US(IJK5)=SRH*( F(1,2,KU)+F(1,2,KV))
          VS(IJK5)=SRH*(-F(1,2,KU)+F(1,2,KV))
          US(IJK7)=SRH*(-F(1,2,KU)+F(1,2,KV))
          VS(IJK7)=SRH*(-F(1,2,KU)-F(1,2,KV))
        ENDDO
        IF(KMAX.EQ.1) THEN
          KU=1
          KV=2
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
            UN(IJ1)=UN(IJ1)+2*SRH*(( F(LR,1,KU)+F(LR,1,KV))
     &                             *CLON(L,1)
     &                            -( F(LI,1,KU)+F(LI,1,KV))
     &                             *SLON(L,1))
            VN(IJ1)=VN(IJ1)+2*SRH*((-F(LR,1,KU)+F(LR,1,KV))
     &                             *CLON(L,1)
     &                            -(-F(LI,1,KU)+F(LI,1,KV))
     &                             *SLON(L,1))
            UN(IJ3)=UN(IJ3)+2*SRH*((-F(LR,1,KU)+F(LR,1,KV))
     &                             *CLON(L,3)
     &                            -(-F(LI,1,KU)+F(LI,1,KV))
     &                             *SLON(L,3))
            VN(IJ3)=VN(IJ3)+2*SRH*((-F(LR,1,KU)-F(LR,1,KV))
     &                             *CLON(L,3)
     &                            -(-F(LI,1,KU)-F(LI,1,KV))
     &                             *SLON(L,3))
            UN(IJ5)=UN(IJ5)+2*SRH*((-F(LR,1,KU)-F(LR,1,KV))
     &                             *CLON(L,5)
     &                            -(-F(LI,1,KU)-F(LI,1,KV))
     &                             *SLON(L,5))
            VN(IJ5)=VN(IJ5)+2*SRH*(( F(LR,1,KU)-F(LR,1,KV))
     &                             *CLON(L,5)
     &                            -( F(LI,1,KU)-F(LI,1,KV))
     &                             *SLON(L,5))
            UN(IJ7)=UN(IJ7)+2*SRH*(( F(LR,1,KU)-F(LR,1,KV))
     &                             *CLON(L,7)
     &                            -( F(LI,1,KU)-F(LI,1,KV))
     &                             *SLON(L,7))
            VN(IJ7)=VN(IJ7)+2*SRH*(( F(LR,1,KU)+F(LR,1,KV))
     &                             *CLON(L,7)
     &                            -( F(LI,1,KU)+F(LI,1,KV))
     &                             *SLON(L,7))
            US(IJ1)=US(IJ1)+2*SRH*((-F(LR,2,KU)-F(LR,2,KV))
     &                             *CLON(L,3)
     &                            -(-F(LI,2,KU)-F(LI,2,KV))
     &                             *SLON(L,3))
            VS(IJ1)=VS(IJ1)+2*SRH*(( F(LR,2,KU)-F(LR,2,KV))
     &                             *CLON(L,3)
     &                            -( F(LI,2,KU)-F(LI,2,KV))
     &                             *SLON(L,3))
            US(IJ3)=US(IJ3)+2*SRH*(( F(LR,2,KU)-F(LR,2,KV))
     &                             *CLON(L,1)
     &                            -( F(LI,2,KU)-F(LI,2,KV))
     &                             *SLON(L,1))
            VS(IJ3)=VS(IJ3)+2*SRH*(( F(LR,2,KU)+F(LR,2,KV))
     &                             *CLON(L,1)
     &                            -( F(LI,2,KU)+F(LI,2,KV))
     &                             *SLON(L,1))
            US(IJ5)=US(IJ5)+2*SRH*(( F(LR,2,KU)+F(LR,2,KV))
     &                             *CLON(L,7)
     &                            -( F(LI,2,KU)+F(LI,2,KV))
     &                             *SLON(L,7))
            VS(IJ5)=VS(IJ5)+2*SRH*((-F(LR,2,KU)+F(LR,2,KV))
     &                             *CLON(L,7)
     &                            -(-F(LI,2,KU)+F(LI,2,KV))
     &                             *SLON(L,7))
            US(IJ7)=US(IJ7)+2*SRH*((-F(LR,2,KU)+F(LR,2,KV))
     &                             *CLON(L,5)
     &                            -(-F(LI,2,KU)+F(LI,2,KV))
     &                             *SLON(L,5))
            VS(IJ7)=VS(IJ7)+2*SRH*((-F(LR,2,KU)-F(LR,2,KV))
     &                             *CLON(L,5)
     &                            -(-F(LI,2,KU)-F(LI,2,KV))
     &                             *SLON(L,5))
          ENDDO
        ELSE
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
CDIR$ IVDEP
            DO K=1,KMAX
              KU=K
              KV=K+KMAX
              IJK1=IJ1+(K-1)*KG
              IJK3=IJ3+(K-1)*KG
              IJK5=IJ5+(K-1)*KG
              IJK7=IJ7+(K-1)*KG
              UN(IJK1)=UN(IJK1)+2*SRH*(( F(LR,1,KU)+F(LR,1,KV))
     &                                 *CLON(L,1)
     &                                -( F(LI,1,KU)+F(LI,1,KV))
     &                                 *SLON(L,1))
              VN(IJK1)=VN(IJK1)+2*SRH*((-F(LR,1,KU)+F(LR,1,KV))
     &                                 *CLON(L,1)
     &                                -(-F(LI,1,KU)+F(LI,1,KV))
     &                                 *SLON(L,1))
              UN(IJK3)=UN(IJK3)+2*SRH*((-F(LR,1,KU)+F(LR,1,KV))
     &                                 *CLON(L,3)
     &                                -(-F(LI,1,KU)+F(LI,1,KV))
     &                                 *SLON(L,3))
              VN(IJK3)=VN(IJK3)+2*SRH*((-F(LR,1,KU)-F(LR,1,KV))
     &                                 *CLON(L,3)
     &                                -(-F(LI,1,KU)-F(LI,1,KV))
     &                                 *SLON(L,3))
              UN(IJK5)=UN(IJK5)+2*SRH*((-F(LR,1,KU)-F(LR,1,KV))
     &                                 *CLON(L,5)
     &                                -(-F(LI,1,KU)-F(LI,1,KV))
     &                                 *SLON(L,5))
              VN(IJK5)=VN(IJK5)+2*SRH*(( F(LR,1,KU)-F(LR,1,KV))
     &                                 *CLON(L,5)
     &                                -( F(LI,1,KU)-F(LI,1,KV))
     &                                 *SLON(L,5))
              UN(IJK7)=UN(IJK7)+2*SRH*(( F(LR,1,KU)-F(LR,1,KV))
     &                                 *CLON(L,7)
     &                                -( F(LI,1,KU)-F(LI,1,KV))
     &                                 *SLON(L,7))
              VN(IJK7)=VN(IJK7)+2*SRH*(( F(LR,1,KU)+F(LR,1,KV))
     &                                 *CLON(L,7)
     &                                -( F(LI,1,KU)+F(LI,1,KV))
     &                                 *SLON(L,7))
              US(IJK1)=US(IJK1)+2*SRH*((-F(LR,2,KU)-F(LR,2,KV))
     &                                 *CLON(L,3)
     &                                -(-F(LI,2,KU)-F(LI,2,KV))
     &                                 *SLON(L,3))
              VS(IJK1)=VS(IJK1)+2*SRH*(( F(LR,2,KU)-F(LR,2,KV))
     &                                 *CLON(L,3)
     &                                -( F(LI,2,KU)-F(LI,2,KV))
     &                                 *SLON(L,3))
              US(IJK3)=US(IJK3)+2*SRH*(( F(LR,2,KU)-F(LR,2,KV))
     &                                 *CLON(L,1)
     &                                -( F(LI,2,KU)-F(LI,2,KV))
     &                                 *SLON(L,1))
              VS(IJK3)=VS(IJK3)+2*SRH*(( F(LR,2,KU)+F(LR,2,KV))
     &                                 *CLON(L,1)
     &                                -( F(LI,2,KU)+F(LI,2,KV))
     &                                 *SLON(L,1))
              US(IJK5)=US(IJK5)+2*SRH*(( F(LR,2,KU)+F(LR,2,KV))
     &                                 *CLON(L,7)
     &                                -( F(LI,2,KU)+F(LI,2,KV))
     &                                 *SLON(L,7))
              VS(IJK5)=VS(IJK5)+2*SRH*((-F(LR,2,KU)+F(LR,2,KV))
     &                                 *CLON(L,7)
     &                                -(-F(LI,2,KU)+F(LI,2,KV))
     &                                 *SLON(L,7))
              US(IJK7)=US(IJK7)+2*SRH*((-F(LR,2,KU)+F(LR,2,KV))
     &                                 *CLON(L,5)
     &                                -(-F(LI,2,KU)+F(LI,2,KV))
     &                                 *SLON(L,5))
              VS(IJK7)=VS(IJK7)+2*SRH*((-F(LR,2,KU)-F(LR,2,KV))
     &                                 *CLON(L,5)
     &                                -(-F(LI,2,KU)-F(LI,2,KV))
     &                                 *SLON(L,5))
            ENDDO
          ENDDO
        ENDIF
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE THE REMAINDER OF THE POLAR STEREOGRAPHIC DOMAIN,
C  STARTING AT THE SECTOR JUST CLOCKWISE OF THE ORIENTATION LONGITUDE
C  AND GOING CLOCKWISE UNTIL ALL EIGHT SECTORS ARE DONE.
C$OMP PARALLEL DO PRIVATE(I1,J2,I2,J3,I3,J4,I4,J5,I5,J6,I6,J7,I7,J8,I8)
C$OMP&       PRIVATE(IJ1,IJ2,IJ3,IJ4,IJ5,IJ6,IJ7,IJ8)
C$OMP&       PRIVATE(IJK1,IJK2,IJK3,IJK4,IJK5,IJK6,IJK7,IJK8)
C$OMP&       PRIVATE(DJ1,DI1,RQ,RR,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,KU,KV,LR,LI)
      DO J1=1,NPH-1
        DO I1=J1+1,NPH
          J2=I1
          I2=J1
          J3=NPS+1-I1
          I3=J1
          J4=NPS+1-J1
          I4=I1
          J5=NPS+1-J1
          I5=NPS+1-I1
          J6=NPS+1-I1
          I6=NPS+1-J1
          J7=I1
          I7=NPS+1-J1
          J8=J1
          I8=NPS+1-I1
          IJ1=(I1-1)*NI+(J1-1)*NJ+1
          IJ2=(I2-1)*NI+(J2-1)*NJ+1
          IJ3=(I3-1)*NI+(J3-1)*NJ+1
          IJ4=(I4-1)*NI+(J4-1)*NJ+1
          IJ5=(I5-1)*NI+(J5-1)*NJ+1
          IJ6=(I6-1)*NI+(J6-1)*NJ+1
          IJ7=(I7-1)*NI+(J7-1)*NJ+1
          IJ8=(I8-1)*NI+(J8-1)*NJ+1
          DI1=I1-NPH-1
          DJ1=J1-NPH-1
          RQ=DI1**2+DJ1**2
          RR=SQRT(1/RQ)
          SLAT1=(GQ-RQ)/(GQ+RQ)
          CLAT1=SQRT(1.-SLAT1**2)
          RADLON1=ORIENT/DPR+ATAN(-DI1/DJ1)
          RADLON2=(ORIENT-45)/DPR*2-RADLON1
          CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &                  PLN,PLNTOP)
          CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,MDIM,2*MXTOP,2*KMAX,
     &                 CLAT1,PLN,PLNTOP,MP,W,WTOP,F)
          DO L=1,MAXWV
            SLON(L,1)=SIN(L*RADLON1)
            CLON(L,1)=COS(L*RADLON1)
            SLON(L,2)=SIN(L*RADLON2)
            CLON(L,2)=COS(L*RADLON2)
            SLON(L,3)=SLON(L,1)*CROT(MOD(1*L,4))
     &               -CLON(L,1)*SROT(MOD(1*L,4))
            CLON(L,3)=CLON(L,1)*CROT(MOD(1*L,4))
     &               +SLON(L,1)*SROT(MOD(1*L,4))
            SLON(L,4)=SLON(L,2)*CROT(MOD(1*L,4))
     &               -CLON(L,2)*SROT(MOD(1*L,4))
            CLON(L,4)=CLON(L,2)*CROT(MOD(1*L,4))
     &               +SLON(L,2)*SROT(MOD(1*L,4))
            SLON(L,5)=SLON(L,1)*CROT(MOD(2*L,4))
     &               -CLON(L,1)*SROT(MOD(2*L,4))
            CLON(L,5)=CLON(L,1)*CROT(MOD(2*L,4))
     &               +SLON(L,1)*SROT(MOD(2*L,4))
            SLON(L,6)=SLON(L,2)*CROT(MOD(2*L,4))
     &               -CLON(L,2)*SROT(MOD(2*L,4))
            CLON(L,6)=CLON(L,2)*CROT(MOD(2*L,4))
     &               +SLON(L,2)*SROT(MOD(2*L,4))
            SLON(L,7)=SLON(L,1)*CROT(MOD(3*L,4))
     &               -CLON(L,1)*SROT(MOD(3*L,4))
            CLON(L,7)=CLON(L,1)*CROT(MOD(3*L,4))
     &               +SLON(L,1)*SROT(MOD(3*L,4))
            SLON(L,8)=SLON(L,2)*CROT(MOD(3*L,4))
     &               -CLON(L,2)*SROT(MOD(3*L,4))
            CLON(L,8)=CLON(L,2)*CROT(MOD(3*L,4))
     &               +SLON(L,2)*SROT(MOD(3*L,4))
          ENDDO
CDIR$ IVDEP
          DO K=1,KMAX
            KU=K
            KV=K+KMAX
            IJK1=IJ1+(K-1)*KG
            IJK2=IJ2+(K-1)*KG
            IJK3=IJ3+(K-1)*KG
            IJK4=IJ4+(K-1)*KG
            IJK5=IJ5+(K-1)*KG
            IJK6=IJ6+(K-1)*KG
            IJK7=IJ7+(K-1)*KG
            IJK8=IJ8+(K-1)*KG
            UN(IJK1)=RR*(-DJ1*F(1,1,KU)-DI1*F(1,1,KV))
            VN(IJK1)=RR*( DI1*F(1,1,KU)-DJ1*F(1,1,KV))
            UN(IJK2)=RR*(-DI1*F(1,1,KU)-DJ1*F(1,1,KV))
            VN(IJK2)=RR*( DJ1*F(1,1,KU)-DI1*F(1,1,KV))
            UN(IJK3)=RR*( DI1*F(1,1,KU)-DJ1*F(1,1,KV))
            VN(IJK3)=RR*( DJ1*F(1,1,KU)+DI1*F(1,1,KV))
            UN(IJK4)=RR*( DJ1*F(1,1,KU)-DI1*F(1,1,KV))
            VN(IJK4)=RR*( DI1*F(1,1,KU)+DJ1*F(1,1,KV))
            UN(IJK5)=RR*( DJ1*F(1,1,KU)+DI1*F(1,1,KV))
            VN(IJK5)=RR*(-DI1*F(1,1,KU)+DJ1*F(1,1,KV))
            UN(IJK6)=RR*( DI1*F(1,1,KU)+DJ1*F(1,1,KV))
            VN(IJK6)=RR*(-DJ1*F(1,1,KU)+DI1*F(1,1,KV))
            UN(IJK7)=RR*(-DI1*F(1,1,KU)+DJ1*F(1,1,KV))
            VN(IJK7)=RR*(-DJ1*F(1,1,KU)-DI1*F(1,1,KV))
            UN(IJK8)=RR*(-DJ1*F(1,1,KU)+DI1*F(1,1,KV))
            VN(IJK8)=RR*(-DI1*F(1,1,KU)-DJ1*F(1,1,KV))
            US(IJK1)=RR*( DJ1*F(1,2,KU)+DI1*F(1,2,KV))
            VS(IJK1)=RR*(-DI1*F(1,2,KU)+DJ1*F(1,2,KV))
            US(IJK2)=RR*( DI1*F(1,2,KU)+DJ1*F(1,2,KV))
            VS(IJK2)=RR*(-DJ1*F(1,2,KU)+DI1*F(1,2,KV))
            US(IJK3)=RR*(-DI1*F(1,2,KU)+DJ1*F(1,2,KV))
            VS(IJK3)=RR*(-DJ1*F(1,2,KU)-DI1*F(1,2,KV))
            US(IJK4)=RR*(-DJ1*F(1,2,KU)+DI1*F(1,2,KV))
            VS(IJK4)=RR*(-DI1*F(1,2,KU)-DJ1*F(1,2,KV))
            US(IJK5)=RR*(-DJ1*F(1,2,KU)-DI1*F(1,2,KV))
            VS(IJK5)=RR*( DI1*F(1,2,KU)-DJ1*F(1,2,KV))
            US(IJK6)=RR*(-DI1*F(1,2,KU)-DJ1*F(1,2,KV))
            VS(IJK6)=RR*( DJ1*F(1,2,KU)-DI1*F(1,2,KV))
            US(IJK7)=RR*( DI1*F(1,2,KU)-DJ1*F(1,2,KV))
            VS(IJK7)=RR*( DJ1*F(1,2,KU)+DI1*F(1,2,KV))
            US(IJK8)=RR*( DJ1*F(1,2,KU)-DI1*F(1,2,KV))
            VS(IJK8)=RR*( DI1*F(1,2,KU)+DJ1*F(1,2,KV))
          ENDDO
          IF(KMAX.EQ.1) THEN
            KU=1
            KV=2
            DO L=1,MAXWV
              LR=2*L+1
              LI=2*L+2
              UN(IJ1)=UN(IJ1)+2*RR*((-DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                              *CLON(L,1)
     &                             -(-DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                              *SLON(L,1))
              VN(IJ1)=VN(IJ1)+2*RR*(( DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                              *CLON(L,1)
     &                             -( DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                              *SLON(L,1))
              UN(IJ2)=UN(IJ2)+2*RR*((-DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                              *CLON(L,2)
     &                             -(-DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                              *SLON(L,2))
              VN(IJ2)=VN(IJ2)+2*RR*(( DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                              *CLON(L,2)
     &                             -( DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                              *SLON(L,2))
              UN(IJ3)=UN(IJ3)+2*RR*(( DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                              *CLON(L,3)
     &                             -( DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                              *SLON(L,3))
              VN(IJ3)=VN(IJ3)+2*RR*(( DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                              *CLON(L,3)
     &                             -( DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                              *SLON(L,3))
              UN(IJ4)=UN(IJ4)+2*RR*(( DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                              *CLON(L,4)
     &                             -( DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                              *SLON(L,4))
              VN(IJ4)=VN(IJ4)+2*RR*(( DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                              *CLON(L,4)
     &                             -( DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                              *SLON(L,4))
              UN(IJ5)=UN(IJ5)+2*RR*(( DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                              *CLON(L,5)
     &                             -( DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                              *SLON(L,5))
              VN(IJ5)=VN(IJ5)+2*RR*((-DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                              *CLON(L,5)
     &                             -(-DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                              *SLON(L,5))
              UN(IJ6)=UN(IJ6)+2*RR*(( DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                              *CLON(L,6)
     &                             -( DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                              *SLON(L,6))
              VN(IJ6)=VN(IJ6)+2*RR*((-DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                              *CLON(L,6)
     &                             -(-DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                              *SLON(L,6))
              UN(IJ7)=UN(IJ7)+2*RR*((-DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                              *CLON(L,7)
     &                             -(-DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                              *SLON(L,7))
              VN(IJ7)=VN(IJ7)+2*RR*((-DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                              *CLON(L,7)
     &                             -(-DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                              *SLON(L,7))
              UN(IJ8)=UN(IJ8)+2*RR*((-DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                              *CLON(L,8)
     &                             -(-DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                              *SLON(L,8))
              VN(IJ8)=VN(IJ8)+2*RR*((-DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                              *CLON(L,8)
     &                             -(-DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                              *SLON(L,8))
              US(IJ1)=US(IJ1)+2*RR*(( DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                              *CLON(L,4)
     &                             -( DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                              *SLON(L,4))
              VS(IJ1)=VS(IJ1)+2*RR*((-DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                              *CLON(L,4)
     &                             -(-DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                              *SLON(L,4))
              US(IJ2)=US(IJ2)+2*RR*(( DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                              *CLON(L,3)
     &                             -( DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                              *SLON(L,3))
              VS(IJ2)=VS(IJ2)+2*RR*((-DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                              *CLON(L,3)
     &                             -(-DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                              *SLON(L,3))
              US(IJ3)=US(IJ3)+2*RR*((-DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                              *CLON(L,2)
     &                             -(-DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                              *SLON(L,2))
              VS(IJ3)=VS(IJ3)+2*RR*((-DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                              *CLON(L,2)
     &                             -(-DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                              *SLON(L,2))
              US(IJ4)=US(IJ4)+2*RR*((-DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                              *CLON(L,1)
     &                             -(-DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                              *SLON(L,1))
              VS(IJ4)=VS(IJ4)+2*RR*((-DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                              *CLON(L,1)
     &                             -(-DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                              *SLON(L,1))
              US(IJ5)=US(IJ5)+2*RR*((-DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                              *CLON(L,8)
     &                             -(-DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                              *SLON(L,8))
              VS(IJ5)=VS(IJ5)+2*RR*(( DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                              *CLON(L,8)
     &                             -( DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                              *SLON(L,8))
              US(IJ6)=US(IJ6)+2*RR*((-DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                              *CLON(L,7)
     &                             -(-DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                              *SLON(L,7))
              VS(IJ6)=VS(IJ6)+2*RR*(( DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                              *CLON(L,7)
     &                             -( DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                              *SLON(L,7))
              US(IJ7)=US(IJ7)+2*RR*(( DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                              *CLON(L,6)
     &                             -( DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                              *SLON(L,6))
              VS(IJ7)=VS(IJ7)+2*RR*(( DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                              *CLON(L,6)
     &                             -( DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                              *SLON(L,6))
              US(IJ8)=US(IJ8)+2*RR*(( DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                              *CLON(L,5)
     &                             -( DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                              *SLON(L,5))
              VS(IJ8)=VS(IJ8)+2*RR*(( DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                              *CLON(L,5)
     &                             -( DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                              *SLON(L,5))
            ENDDO
          ELSE
            DO L=1,MAXWV
              LR=2*L+1
              LI=2*L+2
CDIR$ IVDEP
              DO K=1,KMAX
                KU=K
                KV=K+KMAX
                IJK1=IJ1+(K-1)*KG
                IJK2=IJ2+(K-1)*KG
                IJK3=IJ3+(K-1)*KG
                IJK4=IJ4+(K-1)*KG
                IJK5=IJ5+(K-1)*KG
                IJK6=IJ6+(K-1)*KG
                IJK7=IJ7+(K-1)*KG
                IJK8=IJ8+(K-1)*KG
                UN(IJK1)=UN(IJK1)+2*RR*((-DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                                  *CLON(L,1)
     &                                 -(-DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                                  *SLON(L,1))
                VN(IJK1)=VN(IJK1)+2*RR*(( DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                                  *CLON(L,1)
     &                                 -( DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                                  *SLON(L,1))
                UN(IJK2)=UN(IJK2)+2*RR*((-DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                                  *CLON(L,2)
     &                                 -(-DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                                  *SLON(L,2))
                VN(IJK2)=VN(IJK2)+2*RR*(( DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                                  *CLON(L,2)
     &                                 -( DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                                  *SLON(L,2))
                UN(IJK3)=UN(IJK3)+2*RR*(( DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                                  *CLON(L,3)
     &                                 -( DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                                  *SLON(L,3))
                VN(IJK3)=VN(IJK3)+2*RR*(( DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                                  *CLON(L,3)
     &                                 -( DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                                  *SLON(L,3))
                UN(IJK4)=UN(IJK4)+2*RR*(( DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                                  *CLON(L,4)
     &                                 -( DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                                  *SLON(L,4))
                VN(IJK4)=VN(IJK4)+2*RR*(( DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                                  *CLON(L,4)
     &                                 -( DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                                  *SLON(L,4))
                UN(IJK5)=UN(IJK5)+2*RR*(( DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                                  *CLON(L,5)
     &                                 -( DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                                  *SLON(L,5))
                VN(IJK5)=VN(IJK5)+2*RR*((-DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                                  *CLON(L,5)
     &                                 -(-DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                                  *SLON(L,5))
                UN(IJK6)=UN(IJK6)+2*RR*(( DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                                  *CLON(L,6)
     &                                 -( DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                                  *SLON(L,6))
                VN(IJK6)=VN(IJK6)+2*RR*((-DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                                  *CLON(L,6)
     &                                 -(-DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                                  *SLON(L,6))
                UN(IJK7)=UN(IJK7)+2*RR*((-DI1*F(LR,1,KU)+DJ1*F(LR,1,KV))
     &                                  *CLON(L,7)
     &                                 -(-DI1*F(LI,1,KU)+DJ1*F(LI,1,KV))
     &                                  *SLON(L,7))
                VN(IJK7)=VN(IJK7)+2*RR*((-DJ1*F(LR,1,KU)-DI1*F(LR,1,KV))
     &                                  *CLON(L,7)
     &                                 -(-DJ1*F(LI,1,KU)-DI1*F(LI,1,KV))
     &                                  *SLON(L,7))
                UN(IJK8)=UN(IJK8)+2*RR*((-DJ1*F(LR,1,KU)+DI1*F(LR,1,KV))
     &                                  *CLON(L,8)
     &                                 -(-DJ1*F(LI,1,KU)+DI1*F(LI,1,KV))
     &                                  *SLON(L,8))
                VN(IJK8)=VN(IJK8)+2*RR*((-DI1*F(LR,1,KU)-DJ1*F(LR,1,KV))
     &                                  *CLON(L,8)
     &                                 -(-DI1*F(LI,1,KU)-DJ1*F(LI,1,KV))
     &                                  *SLON(L,8))
                US(IJK1)=US(IJK1)+2*RR*(( DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                                  *CLON(L,4)
     &                                 -( DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                                  *SLON(L,4))
                VS(IJK1)=VS(IJK1)+2*RR*((-DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                                  *CLON(L,4)
     &                                 -(-DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                                  *SLON(L,4))
                US(IJK2)=US(IJK2)+2*RR*(( DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                                  *CLON(L,3)
     &                                 -( DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                                  *SLON(L,3))
                VS(IJK2)=VS(IJK2)+2*RR*((-DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                                  *CLON(L,3)
     &                                 -(-DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                                  *SLON(L,3))
                US(IJK3)=US(IJK3)+2*RR*((-DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                                  *CLON(L,2)
     &                                 -(-DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                                  *SLON(L,2))
                VS(IJK3)=VS(IJK3)+2*RR*((-DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                                  *CLON(L,2)
     &                                 -(-DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                                  *SLON(L,2))
                US(IJK4)=US(IJK4)+2*RR*((-DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                                  *CLON(L,1)
     &                                 -(-DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                                  *SLON(L,1))
                VS(IJK4)=VS(IJK4)+2*RR*((-DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                                  *CLON(L,1)
     &                                 -(-DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                                  *SLON(L,1))
                US(IJK5)=US(IJK5)+2*RR*((-DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                                  *CLON(L,8)
     &                                 -(-DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                                  *SLON(L,8))
                VS(IJK5)=VS(IJK5)+2*RR*(( DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                                  *CLON(L,8)
     &                                 -( DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                                  *SLON(L,8))
                US(IJK6)=US(IJK6)+2*RR*((-DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                                  *CLON(L,7)
     &                                 -(-DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                                  *SLON(L,7))
                VS(IJK6)=VS(IJK6)+2*RR*(( DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                                  *CLON(L,7)
     &                                 -( DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                                  *SLON(L,7))
                US(IJK7)=US(IJK7)+2*RR*(( DI1*F(LR,2,KU)-DJ1*F(LR,2,KV))
     &                                  *CLON(L,6)
     &                                 -( DI1*F(LI,2,KU)-DJ1*F(LI,2,KV))
     &                                  *SLON(L,6))
                VS(IJK7)=VS(IJK7)+2*RR*(( DJ1*F(LR,2,KU)+DI1*F(LR,2,KV))
     &                                  *CLON(L,6)
     &                                 -( DJ1*F(LI,2,KU)+DI1*F(LI,2,KV))
     &                                  *SLON(L,6))
                US(IJK8)=US(IJK8)+2*RR*(( DJ1*F(LR,2,KU)-DI1*F(LR,2,KV))
     &                                  *CLON(L,5)
     &                                 -( DJ1*F(LI,2,KU)-DI1*F(LI,2,KV))
     &                                  *SLON(L,5))
                VS(IJK8)=VS(IJK8)+2*RR*(( DI1*F(LR,2,KU)+DJ1*F(LR,2,KV))
     &                                  *CLON(L,5)
     &                                 -( DI1*F(LI,2,KU)+DJ1*F(LI,2,KV))
     &                                  *SLON(L,5))
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
