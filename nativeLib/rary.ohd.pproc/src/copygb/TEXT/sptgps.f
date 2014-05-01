C-----------------------------------------------------------------------
      SUBROUTINE SPTGPS(IROMB,MAXWV,KMAX,NPS,
     &                  KWSKIP,KGSKIP,NISKIP,NJSKIP,
     &                  TRUE,XMESH,ORIENT,WAVE,GN,GS)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  SPTGPS     TRANSFORM SPECTRAL SCALAR TO POLAR STEREO.
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-02-29
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS A SPHERICAL TRANSFORM
C           FROM SPECTRAL COEFFICIENTS OF SCALAR QUANTITIES
C           TO SCALAR FIELDS ON A PAIR OF POLAR STEREOGRAPHIC GRIDS.
C           THE WAVE-SPACE CAN BE EITHER TRIANGULAR OR RHOMBOIDAL.
C           THE WAVE AND GRID FIELDS MAY HAVE GENERAL INDEXING,
C           BUT EACH WAVE FIELD IS IN SEQUENTIAL 'IBM ORDER',
C           I.E. WITH ZONAL WAVENUMBER AS THE SLOWER INDEX.
C           THE TWO SQUARE POLAR STEREOGRAPHIC GRIDS ARE CENTERED
C           ON THE RESPECTIVE POLES, WITH THE ORIENTATION LONGITUDE
C           OF THE SOUTHERN HEMISPHERE GRID 180 DEGREES OPPOSITE
C           THAT OF THE NORTHERN HEMISPHERE GRID.
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
C USAGE:    CALL SPTGPS(IROMB,MAXWV,KMAX,NPS,
C    &                  KWSKIP,KGSKIP,NISKIP,NJSKIP,
C    &                  TRUE,XMESH,ORIENT,WAVE,GN,GS)
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
C     WAVE     - REAL (*) WAVE FIELDS
C   OUTPUT ARGUMENTS:
C     GN       - REAL (*) NORTHERN POLAR STEREOGRAPHIC FIELDS
C     GS       - REAL (*) SOUTHERN POLAR STEREOGRAPHIC FIELDS
C
C SUBPROGRAMS CALLED:
C   SPWGET       GET WAVE-SPACE CONSTANTS
C   SPLEGEND     COMPUTE LEGENDRE POLYNOMIALS
C   SPSYNTH      SYNTHESIZE FOURIER FROM SPECTRAL
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
      REAL WAVE(*),GN(*),GS(*)
      REAL EPS((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EPSTOP(MAXWV+1)
      REAL ENN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL ELONN1((MAXWV+1)*((IROMB+1)*MAXWV+2)/2)
      REAL EON((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),EONTOP(MAXWV+1)
      INTEGER MP(KMAX)
      REAL SLON(MAXWV,8),CLON(MAXWV,8),SROT(0:3),CROT(0:3)
      REAL WTOP(2*(MAXWV+1),KMAX)
      REAL PLN((MAXWV+1)*((IROMB+1)*MAXWV+2)/2),PLNTOP(MAXWV+1)
      REAL F(2*MAXWV+3,2,KMAX)
      DATA SROT/0.,1.,0.,-1./,CROT/1.,0.,-1.,0./
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265358979,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE PRELIMINARY CONSTANTS
      CALL SPWGET(IROMB,MAXWV,EPS,EPSTOP,ENN1,ELONN1,EON,EONTOP)
      MX=(MAXWV+1)*((IROMB+1)*MAXWV+2)/2
      MXTOP=MAXWV+1
      IDIM=2*MAXWV+3
      KW=KWSKIP
      KG=KGSKIP
      NI=NISKIP
      NJ=NJSKIP
      IF(KW.EQ.0) KW=2*MX
      IF(KG.EQ.0) KG=NPS*NPS
      IF(NI.EQ.0) NI=1
      IF(NJ.EQ.0) NJ=NPS
      MP=0
      NPH=(NPS-1)/2
      GQ=((1.+SIN(TRUE/DPR))*RERTH/XMESH)**2
C$OMP PARALLEL DO
      DO K=1,KMAX
        WTOP(1:2*MXTOP,K)=0
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
      CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &             CLAT1,PLN,PLNTOP,MP,WAVE,WTOP,F)
CDIR$ IVDEP
      DO K=1,KMAX
        IJK1=IJ1+(K-1)*KG
        GN(IJK1)=F(1,1,K)
        GS(IJK1)=F(1,2,K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  CALCULATE POINTS ALONG THE ROW AND COLUMN OF THE POLE,
C  STARTING AT THE ORIENTATION LONGITUDE AND GOING CLOCKWISE.
C$OMP PARALLEL DO PRIVATE(I1,J2,I2,J3,I3,J4,I4,J5,I5,J6,I6,J7,I7,J8,I8)
C$OMP&       PRIVATE(IJ1,IJ2,IJ3,IJ4,IJ5,IJ6,IJ7,IJ8)
C$OMP&       PRIVATE(IJK1,IJK2,IJK3,IJK4,IJK5,IJK6,IJK7,IJK8)
C$OMP&       PRIVATE(DJ1,DI1,RQ,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,LR,LI)
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
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &               CLAT1,PLN,PLNTOP,MP,WAVE,WTOP,F)
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
          IJK1=IJ1+(K-1)*KG
          IJK3=IJ3+(K-1)*KG
          IJK5=IJ5+(K-1)*KG
          IJK7=IJ7+(K-1)*KG
          GN(IJK1)=F(1,1,K)
          GN(IJK3)=F(1,1,K)
          GN(IJK5)=F(1,1,K)
          GN(IJK7)=F(1,1,K)
          GS(IJK1)=F(1,2,K)
          GS(IJK3)=F(1,2,K)
          GS(IJK5)=F(1,2,K)
          GS(IJK7)=F(1,2,K)
        ENDDO
        IF(KMAX.EQ.1) THEN
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
            GN(IJ1)=GN(IJ1)+2*(F(LR,1,1)*CLON(L,1)
     &                        -F(LI,1,1)*SLON(L,1))
            GN(IJ3)=GN(IJ3)+2*(F(LR,1,1)*CLON(L,3)
     &                        -F(LI,1,1)*SLON(L,3))
            GN(IJ5)=GN(IJ5)+2*(F(LR,1,1)*CLON(L,5)
     &                        -F(LI,1,1)*SLON(L,5))
            GN(IJ7)=GN(IJ7)+2*(F(LR,1,1)*CLON(L,7)
     &                        -F(LI,1,1)*SLON(L,7))
            GS(IJ1)=GS(IJ1)+2*(F(LR,2,1)*CLON(L,5)
     &                        -F(LI,2,1)*SLON(L,5))
            GS(IJ3)=GS(IJ3)+2*(F(LR,2,1)*CLON(L,3)
     &                        -F(LI,2,1)*SLON(L,3))
            GS(IJ5)=GS(IJ5)+2*(F(LR,2,1)*CLON(L,1)
     &                        -F(LI,2,1)*SLON(L,1))
            GS(IJ7)=GS(IJ7)+2*(F(LR,2,1)*CLON(L,7)
     &                        -F(LI,2,1)*SLON(L,7))
          ENDDO
        ELSE
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
CDIR$ IVDEP
            DO K=1,KMAX
              IJK1=IJ1+(K-1)*KG
              IJK3=IJ3+(K-1)*KG
              IJK5=IJ5+(K-1)*KG
              IJK7=IJ7+(K-1)*KG
              GN(IJK1)=GN(IJK1)+2*(F(LR,1,K)*CLON(L,1)
     &                            -F(LI,1,K)*SLON(L,1))
              GN(IJK3)=GN(IJK3)+2*(F(LR,1,K)*CLON(L,3)
     &                            -F(LI,1,K)*SLON(L,3))
              GN(IJK5)=GN(IJK5)+2*(F(LR,1,K)*CLON(L,5)
     &                            -F(LI,1,K)*SLON(L,5))
              GN(IJK7)=GN(IJK7)+2*(F(LR,1,K)*CLON(L,7)
     &                            -F(LI,1,K)*SLON(L,7))
              GS(IJK1)=GS(IJK1)+2*(F(LR,2,K)*CLON(L,5)
     &                            -F(LI,2,K)*SLON(L,5))
              GS(IJK3)=GS(IJK3)+2*(F(LR,2,K)*CLON(L,3)
     &                            -F(LI,2,K)*SLON(L,3))
              GS(IJK5)=GS(IJK5)+2*(F(LR,2,K)*CLON(L,1)
     &                            -F(LI,2,K)*SLON(L,1))
              GS(IJK7)=GS(IJK7)+2*(F(LR,2,K)*CLON(L,7)
     &                            -F(LI,2,K)*SLON(L,7))
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
C$OMP&       PRIVATE(DJ1,DI1,RQ,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,LR,LI)
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
        CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &               CLAT1,PLN,PLNTOP,MP,WAVE,WTOP,F)
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
          IJK1=IJ1+(K-1)*KG
          IJK3=IJ3+(K-1)*KG
          IJK5=IJ5+(K-1)*KG
          IJK7=IJ7+(K-1)*KG
          GN(IJK1)=F(1,1,K)
          GN(IJK3)=F(1,1,K)
          GN(IJK5)=F(1,1,K)
          GN(IJK7)=F(1,1,K)
          GS(IJK1)=F(1,2,K)
          GS(IJK3)=F(1,2,K)
          GS(IJK5)=F(1,2,K)
          GS(IJK7)=F(1,2,K)
        ENDDO
        IF(KMAX.EQ.1) THEN
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
            GN(IJ1)=GN(IJ1)+2*(F(LR,1,1)*CLON(L,1)
     &                        -F(LI,1,1)*SLON(L,1))
            GN(IJ3)=GN(IJ3)+2*(F(LR,1,1)*CLON(L,3)
     &                        -F(LI,1,1)*SLON(L,3))
            GN(IJ5)=GN(IJ5)+2*(F(LR,1,1)*CLON(L,5)
     &                        -F(LI,1,1)*SLON(L,5))
            GN(IJ7)=GN(IJ7)+2*(F(LR,1,1)*CLON(L,7)
     &                        -F(LI,1,1)*SLON(L,7))
            GS(IJ1)=GS(IJ1)+2*(F(LR,2,1)*CLON(L,3)
     &                        -F(LI,2,1)*SLON(L,3))
            GS(IJ3)=GS(IJ3)+2*(F(LR,2,1)*CLON(L,1)
     &                        -F(LI,2,1)*SLON(L,1))
            GS(IJ5)=GS(IJ5)+2*(F(LR,2,1)*CLON(L,7)
     &                        -F(LI,2,1)*SLON(L,7))
            GS(IJ7)=GS(IJ7)+2*(F(LR,2,1)*CLON(L,5)
     &                        -F(LI,2,1)*SLON(L,5))
          ENDDO
        ELSE
          DO L=1,MAXWV
            LR=2*L+1
            LI=2*L+2
CDIR$ IVDEP
            DO K=1,KMAX
              IJK1=IJ1+(K-1)*KG
              IJK3=IJ3+(K-1)*KG
              IJK5=IJ5+(K-1)*KG
              IJK7=IJ7+(K-1)*KG
              GN(IJK1)=GN(IJK1)+2*(F(LR,1,K)*CLON(L,1)
     &                            -F(LI,1,K)*SLON(L,1))
              GN(IJK3)=GN(IJK3)+2*(F(LR,1,K)*CLON(L,3)
     &                            -F(LI,1,K)*SLON(L,3))
              GN(IJK5)=GN(IJK5)+2*(F(LR,1,K)*CLON(L,5)
     &                            -F(LI,1,K)*SLON(L,5))
              GN(IJK7)=GN(IJK7)+2*(F(LR,1,K)*CLON(L,7)
     &                            -F(LI,1,K)*SLON(L,7))
              GS(IJK1)=GS(IJK1)+2*(F(LR,2,K)*CLON(L,3)
     &                            -F(LI,2,K)*SLON(L,3))
              GS(IJK3)=GS(IJK3)+2*(F(LR,2,K)*CLON(L,1)
     &                            -F(LI,2,K)*SLON(L,1))
              GS(IJK5)=GS(IJK5)+2*(F(LR,2,K)*CLON(L,7)
     &                            -F(LI,2,K)*SLON(L,7))
              GS(IJK7)=GS(IJK7)+2*(F(LR,2,K)*CLON(L,5)
     &                            -F(LI,2,K)*SLON(L,5))
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
C$OMP&       PRIVATE(DJ1,DI1,RQ,RADLON,RADLON1,RADLON2,SLAT1,CLAT1)
C$OMP&       PRIVATE(PLN,PLNTOP,F,SLON,CLON,LR,LI)
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
          SLAT1=(GQ-RQ)/(GQ+RQ)
          CLAT1=SQRT(1.-SLAT1**2)
          RADLON1=ORIENT/DPR+ATAN(-DI1/DJ1)
          RADLON2=(ORIENT-45)/DPR*2-RADLON1
          CALL SPLEGEND(IROMB,MAXWV,SLAT1,CLAT1,EPS,EPSTOP,
     &                  PLN,PLNTOP)
          CALL SPSYNTH(IROMB,MAXWV,2*MAXWV,IDIM,KW,2*MXTOP,KMAX,
     &                 CLAT1,PLN,PLNTOP,MP,WAVE,WTOP,F)
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
            IJK1=IJ1+(K-1)*KG
            IJK2=IJ2+(K-1)*KG
            IJK3=IJ3+(K-1)*KG
            IJK4=IJ4+(K-1)*KG
            IJK5=IJ5+(K-1)*KG
            IJK6=IJ6+(K-1)*KG
            IJK7=IJ7+(K-1)*KG
            IJK8=IJ8+(K-1)*KG
            GN(IJK1)=F(1,1,K)
            GN(IJK2)=F(1,1,K)
            GN(IJK3)=F(1,1,K)
            GN(IJK4)=F(1,1,K)
            GN(IJK5)=F(1,1,K)
            GN(IJK6)=F(1,1,K)
            GN(IJK7)=F(1,1,K)
            GN(IJK8)=F(1,1,K)
            GS(IJK1)=F(1,2,K)
            GS(IJK2)=F(1,2,K)
            GS(IJK3)=F(1,2,K)
            GS(IJK4)=F(1,2,K)
            GS(IJK5)=F(1,2,K)
            GS(IJK6)=F(1,2,K)
            GS(IJK7)=F(1,2,K)
            GS(IJK8)=F(1,2,K)
          ENDDO
          IF(KMAX.EQ.1) THEN
            DO L=1,MAXWV
              LR=2*L+1
              LI=2*L+2
              GN(IJ1)=GN(IJ1)+2*(F(LR,1,1)*CLON(L,1)
     &                          -F(LI,1,1)*SLON(L,1))
              GN(IJ2)=GN(IJ2)+2*(F(LR,1,1)*CLON(L,2)
     &                          -F(LI,1,1)*SLON(L,2))
              GN(IJ3)=GN(IJ3)+2*(F(LR,1,1)*CLON(L,3)
     &                          -F(LI,1,1)*SLON(L,3))
              GN(IJ4)=GN(IJ4)+2*(F(LR,1,1)*CLON(L,4)
     &                          -F(LI,1,1)*SLON(L,4))
              GN(IJ5)=GN(IJ5)+2*(F(LR,1,1)*CLON(L,5)
     &                          -F(LI,1,1)*SLON(L,5))
              GN(IJ6)=GN(IJ6)+2*(F(LR,1,1)*CLON(L,6)
     &                          -F(LI,1,1)*SLON(L,6))
              GN(IJ7)=GN(IJ7)+2*(F(LR,1,1)*CLON(L,7)
     &                          -F(LI,1,1)*SLON(L,7))
              GN(IJ8)=GN(IJ8)+2*(F(LR,1,1)*CLON(L,8)
     &                          -F(LI,1,1)*SLON(L,8))
              GS(IJ1)=GS(IJ1)+2*(F(LR,2,1)*CLON(L,4)
     &                          -F(LI,2,1)*SLON(L,4))
              GS(IJ2)=GS(IJ2)+2*(F(LR,2,1)*CLON(L,3)
     &                          -F(LI,2,1)*SLON(L,3))
              GS(IJ3)=GS(IJ3)+2*(F(LR,2,1)*CLON(L,2)
     &                          -F(LI,2,1)*SLON(L,2))
              GS(IJ4)=GS(IJ4)+2*(F(LR,2,1)*CLON(L,1)
     &                          -F(LI,2,1)*SLON(L,1))
              GS(IJ5)=GS(IJ5)+2*(F(LR,2,1)*CLON(L,8)
     &                          -F(LI,2,1)*SLON(L,8))
              GS(IJ6)=GS(IJ6)+2*(F(LR,2,1)*CLON(L,7)
     &                          -F(LI,2,1)*SLON(L,7))
              GS(IJ7)=GS(IJ7)+2*(F(LR,2,1)*CLON(L,6)
     &                          -F(LI,2,1)*SLON(L,6))
              GS(IJ8)=GS(IJ8)+2*(F(LR,2,1)*CLON(L,5)
     &                          -F(LI,2,1)*SLON(L,5))
            ENDDO
          ELSE
            DO L=1,MAXWV
              LR=2*L+1
              LI=2*L+2
CDIR$ IVDEP
              DO K=1,KMAX
                IJK1=IJ1+(K-1)*KG
                IJK2=IJ2+(K-1)*KG
                IJK3=IJ3+(K-1)*KG
                IJK4=IJ4+(K-1)*KG
                IJK5=IJ5+(K-1)*KG
                IJK6=IJ6+(K-1)*KG
                IJK7=IJ7+(K-1)*KG
                IJK8=IJ8+(K-1)*KG
                GN(IJK1)=GN(IJK1)+2*(F(LR,1,K)*CLON(L,1)
     &                              -F(LI,1,K)*SLON(L,1))
                GN(IJK2)=GN(IJK2)+2*(F(LR,1,K)*CLON(L,2)
     &                              -F(LI,1,K)*SLON(L,2))
                GN(IJK3)=GN(IJK3)+2*(F(LR,1,K)*CLON(L,3)
     &                              -F(LI,1,K)*SLON(L,3))
                GN(IJK4)=GN(IJK4)+2*(F(LR,1,K)*CLON(L,4)
     &                              -F(LI,1,K)*SLON(L,4))
                GN(IJK5)=GN(IJK5)+2*(F(LR,1,K)*CLON(L,5)
     &                              -F(LI,1,K)*SLON(L,5))
                GN(IJK6)=GN(IJK6)+2*(F(LR,1,K)*CLON(L,6)
     &                              -F(LI,1,K)*SLON(L,6))
                GN(IJK7)=GN(IJK7)+2*(F(LR,1,K)*CLON(L,7)
     &                              -F(LI,1,K)*SLON(L,7))
                GN(IJK8)=GN(IJK8)+2*(F(LR,1,K)*CLON(L,8)
     &                              -F(LI,1,K)*SLON(L,8))
                GS(IJK1)=GS(IJK1)+2*(F(LR,2,K)*CLON(L,4)
     &                              -F(LI,2,K)*SLON(L,4))
                GS(IJK2)=GS(IJK2)+2*(F(LR,2,K)*CLON(L,3)
     &                              -F(LI,2,K)*SLON(L,3))
                GS(IJK3)=GS(IJK3)+2*(F(LR,2,K)*CLON(L,2)
     &                              -F(LI,2,K)*SLON(L,2))
                GS(IJK4)=GS(IJK4)+2*(F(LR,2,K)*CLON(L,1)
     &                              -F(LI,2,K)*SLON(L,1))
                GS(IJK5)=GS(IJK5)+2*(F(LR,2,K)*CLON(L,8)
     &                              -F(LI,2,K)*SLON(L,8))
                GS(IJK6)=GS(IJK6)+2*(F(LR,2,K)*CLON(L,7)
     &                              -F(LI,2,K)*SLON(L,7))
                GS(IJK7)=GS(IJK7)+2*(F(LR,2,K)*CLON(L,6)
     &                              -F(LI,2,K)*SLON(L,6))
                GS(IJK8)=GS(IJK8)+2*(F(LR,2,K)*CLON(L,5)
     &                              -F(LI,2,K)*SLON(L,5))
              ENDDO
            ENDDO
          ENDIF
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
