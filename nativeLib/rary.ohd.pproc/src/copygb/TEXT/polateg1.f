C-----------------------------------------------------------------------
      SUBROUTINE POLATEG1(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
     &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,XO,YO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLATEG1   INTERPOLATE SCALAR FIELD GRADIENTS (BICUBIC)
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS BICUBIC INTERPOLATION
C           FROM ANY GRID TO ANY GRID FOR SCALAR FIELDS,
C           RETURNING THEIR VECTOR GRADIENTS.
C           BITMAPS ARE NOW ALLOWED, BUT BILINEAR INTERPOLATION IS DONE
C           WHEN ANY INVALID DATA IS WITHIN THE BICUBIC TEMPLATE.
C           OPTIONS ALLOW CHOICES BETWEEN STRAIGHT BICUBIC (IPOPT(1)=0)
C           AND CONSTRAINED BICUBIC (IPOPT(1)=1) WHERE THE VALUE IS
C           CONFINED WITHIN THE RANGE OF THE SURROUNDING 4 POINTS.
C           BILINEAR USED WITHIN ONE GRID LENGTH OF BOUNDARIES.
C           ONLY HORIZONTAL INTERPOLATION IS PERFORMED.
C           THE GRIDS ARE DEFINED BY THEIR GRID DESCRIPTION SECTIONS
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63).
C           THE CURRENT CODE RECOGNIZES THE FOLLOWING PROJECTIONS:
C             (KGDS(1)=000) EQUIDISTANT CYLINDRICAL
C             (KGDS(1)=001) MERCATOR CYLINDRICAL
C             (KGDS(1)=003) LAMBERT CONFORMAL CONICAL
C             (KGDS(1)=004) GAUSSIAN CYLINDRICAL (SPECTRAL NATIVE)
C             (KGDS(1)=005) POLAR STEREOGRAPHIC AZIMUTHAL
C             (KGDS(1)=202) ROTATED EQUIDISTANT CYLINDRICAL (ETA NATIVE)
C           WHERE KGDS COULD BE EITHER INPUT KGDSI OR OUTPUT KGDSO.
C           AS AN ADDED BONUS THE NUMBER OF OUTPUT GRID POINTS
C           AND THEIR LATITUDES AND LONGITUDES ARE ALSO RETURNED.
C           ON THE OTHER HAND, THE OUTPUT CAN BE A SET OF STATION POINTS
C           IF KGDSO(1)<0, IN WHICH CASE THE NUMBER OF POINTS
C           AND THEIR LATITUDES AND LONGITUDES MUST BE INPUT.
C           OUTPUT BITMAPS WILL ONLY BE CREATED WHEN THE OUTPUT GRID
C           EXTENDS OUTSIDE OF THE DOMAIN OF THE INPUT GRID.
C           THE OUTPUT FIELD IS SET TO 0 WHERE THE OUTPUT BITMAP IS OFF.
C        
C PROGRAM HISTORY LOG:
C   96-04-10  IREDELL
C 1999-04-08  IREDELL  SPLIT IJKGDS INTO TWO PIECES
C
C USAGE:    CALL POLATEG1(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
C    &                    NO,RLAT,RLON,CROT,SROT,IBO,LO,XO,YO,IRET)
C
C   INPUT ARGUMENT LIST:
C     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS
C                IPOPT(1)=0 FOR STRAIGHT BICUBIC;
C                IPOPT(1)=1 FOR CONSTRAINED BICUBIC WHERE VALUE IS
C                CONFINED WITHIN THE RANGE OF THE SURROUNDING 4 POINTS.
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
C     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
C     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     IBI      - INTEGER (KM) INPUT BITMAP FLAGS (MUST BE ALL 0)
C     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF SOME IBI(K)=1)
C     GI       - REAL (MI,KM) INPUT FIELDS TO INTERPOLATE
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C     CROT     - REAL (NO) VECTOR ROTATION COSINES (IF KGDSO(1)<0)
C     SROT     - REAL (NO) VECTOR ROTATION SINES (IF KGDSO(1)<0)
C                (UGRID=CROT*UEARTH-SROT*VEARTH;
C                 VGRID=SROT*UEARTH+CROT*VEARTH)
C
C   OUTPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
C     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
C     XO       - REAL (MO,KM) OUTPUT X-GRADIENT FIELDS INTERPOLATED
C     YO       - REAL (MO,KM) OUTPUT Y-GRADIENT FIELDS INTERPOLATED
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C
C SUBPROGRAMS CALLED:
C   GDSWZD       GRID DESCRIPTION SECTION WIZARD
C   IJKGDS0      SET UP PARAMETERS FOR IJKGDS1
C   (IJKGDS1)    RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   POLFIXV      MAKE MULTIPLE POLE VECTOR VALUES CONSISTENT
C
C REMARKS:  THE GRADIENT COMPUTATIONS ARE NOT ROBUST NEAR THE POLES.
C   IN FACT, NO GRADIENTS ARE COMPUTED POLEWARD OF 89 LATITUDE.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
CFPP$ EXPAND(IJKGDS)
      INTEGER IPOPT(20)
      INTEGER KGDSI(200),KGDSO(200)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(MI,KM),LO(MO,KM)
      REAL GI(MI,KM),XO(MO,KM),YO(MO,KM)
      REAL RLAT(MO),RLON(MO)
      REAL CROT(MO),SROT(MO)
      REAL CLAT(MO)
      REAL XPTS(MO),YPTS(MO)
      REAL XLON(MO),XLAT(MO)
      REAL YLON(MO),YLAT(MO)
      INTEGER N11(MO),N21(MO),N12(MO),N22(MO)
      INTEGER NC(MO)
      REAL WX11(MO),WX21(MO),WX12(MO),WX22(MO)
      REAL WY11(MO),WY21(MO),WY12(MO),WY22(MO)
      REAL WX11L(MO),WX21L(MO),WX12L(MO),WX22L(MO)
      REAL WY11L(MO),WY21L(MO),WY12L(MO),WY22L(MO)
      INTEGER IJKGDSA(20)
      PARAMETER(FILL=-9999.)
      PARAMETER(PLAT=89.)
      PARAMETER(RERTH=6.3712E6)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
      IRET=0
      IF(KGDSO(1).GE.0) THEN
        CALL GDSWZD(KGDSO,0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,1,CROT,SROT,
     &              0,XLON,XLAT,YLON,YLAT)
        IF(NO.EQ.0) IRET=3
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS
      DPR=180/ACOS(-1.)
      DO N=1,NO
        CLAT(N)=COS(RLAT(N)/DPR)
      ENDDO
      CALL GDSWZD(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM,
     &            1,XLON,XLAT,YLON,YLAT)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ZERO OUT OUTPUT
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        DO N=1,NO
          LO(N,K)=.FALSE.
          XO(N,K)=0.
          YO(N,K)=0.
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE CORNERS
      IF(IRET.EQ.0) THEN
        CALL IJKGDS0(KGDSI,IJKGDSA)
        DO N=1,NO
          NC(N)=0
          XI=XPTS(N)
          YI=YPTS(N)
          IF(XI.NE.FILL.AND.YI.NE.FILL.AND.ABS(RLAT(N)).LE.PLAT) THEN
            I1=XI-1
            I2=I1+3
            J1=YI-1
            J2=J1+3
            XF=XI-I1-1
            YF=YI-J1-1
            N11(N)=IJKGDS1(I1,J1,IJKGDSA)
            N21(N)=IJKGDS1(I2,J1,IJKGDSA)
            N12(N)=IJKGDS1(I1,J2,IJKGDSA)
            N22(N)=IJKGDS1(I2,J2,IJKGDSA)
            IF(MIN(N11(N),N21(N),N12(N),N22(N)).GT.0) THEN
              NC(N)=1
              FX=DPR/(RERTH*CLAT(N))
              WX11(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &                 (YF*(1-YF)*(2-YF))/36*XLON(N)+
     &                 (XF*(1-XF)*(2-XF))/36*YLON(N)*
     &                 ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FX
              WX21(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &                 (YF*(1-YF)*(2-YF))/36*XLON(N)+
     &                 (XF*(1-XF)*(1+XF))/36*YLON(N)*
     &                 ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FX
              WX12(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &                 (YF*(1-YF)*(1+YF))/36*XLON(N)+
     &                 (XF*(1-XF)*(2-XF))/36*YLON(N)*
     &                 ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FX
              WX22(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &                 (YF*(1-YF)*(1+YF))/36*XLON(N)+
     &                 (XF*(1-XF)*(1+XF))/36*YLON(N)*
     &                 ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FX
              FY=DPR/RERTH
              WY11(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &                 (YF*(1-YF)*(2-YF))/36*XLAT(N)+
     &                 (XF*(1-XF)*(2-XF))/36*YLAT(N)*
     &                 ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FY
              WY21(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &                 (YF*(1-YF)*(2-YF))/36*XLAT(N)+
     &                 (XF*(1-XF)*(1+XF))/36*YLAT(N)*
     &                 ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FY
              WY12(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &                 (YF*(1-YF)*(1+YF))/36*XLAT(N)+
     &                 (XF*(1-XF)*(2-XF))/36*YLAT(N)*
     &                 ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FY
              WY22(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &                 (YF*(1-YF)*(1+YF))/36*XLAT(N)+
     &                 (XF*(1-XF)*(1+XF))/36*YLAT(N)*
     &                 ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FY
            ENDIF
          ENDIF
        ENDDO
CMIC$ DO ALL AUTOSCOPE
        DO K=1,KM
          DO N=1,NO
            IF(NC(N).GT.0) THEN
              LO(N,K)=(IBI(K).EQ.0.OR.
     &                (LI(N11(N),K).AND.LI(N21(N),K).AND.
     &                 LI(N12(N),K).AND.LI(N22(N),K)))
              IF(LO(N,K)) THEN
                G11=GI(N11(N),K)
                G21=GI(N21(N),K)
                G12=GI(N12(N),K)
                G22=GI(N22(N),K)
                XO(N,K)=XO(N,K)+WX11(N)*G11+WX21(N)*G21
     &                         +WX12(N)*G12+WX22(N)*G22
                YO(N,K)=YO(N,K)+WY11(N)*G11+WY21(N)*G21
     &                         +WY12(N)*G12+WY22(N)*G22
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE TOPS AND BOTTOMS
        DO N=1,NO
          IF(NC(N).GT.0) THEN
            XI=XPTS(N)
            YI=YPTS(N)
            I1=XI
            I2=I1+1
            J1=YI-1
            J2=J1+3
            XF=XI-I1
            YF=YI-J1-1
            N11(N)=IJKGDS1(I1,J1,IJKGDSA)
            N21(N)=IJKGDS1(I2,J1,IJKGDSA)
            N12(N)=IJKGDS1(I1,J2,IJKGDSA)
            N22(N)=IJKGDS1(I2,J2,IJKGDSA)
            FX=DPR/(RERTH*CLAT(N))
            WX11(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (YF*(1-YF)*(2-YF))/12*XLON(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/12*YLON(N)*
     &               ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FX
            WX21(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (YF*(1-YF)*(2-YF))/12*XLON(N)+
     &               (-XF*(2-XF)*(1+XF))/12*YLON(N)*
     &               ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FX
            WX12(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (YF*(1-YF)*(1+YF))/12*XLON(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/12*YLON(N)*
     &               ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FX
            WX22(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (YF*(1-YF)*(1+YF))/12*XLON(N)+
     &               (-XF*(2-XF)*(1+XF))/12*YLON(N)*
     &               ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FX
            FY=DPR/RERTH
            WY11(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (YF*(1-YF)*(2-YF))/12*XLAT(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/12*YLAT(N)*
     &               ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FY
            WY21(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (YF*(1-YF)*(2-YF))/12*XLAT(N)+
     &               (-XF*(2-XF)*(1+XF))/12*YLAT(N)*
     &               ((1-YF)*(2-YF)-YF*(2-YF)-YF*(1-YF)))*FY
            WY12(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (YF*(1-YF)*(1+YF))/12*XLAT(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/12*YLAT(N)*
     &               ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FY
            WY22(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (YF*(1-YF)*(1+YF))/12*XLAT(N)+
     &               (-XF*(2-XF)*(1+XF))/12*YLAT(N)*
     &               ((1-YF)*(1+YF)-YF*(1+YF)+YF*(1-YF)))*FY
          ENDIF
        ENDDO
CMIC$ DO ALL AUTOSCOPE
        DO K=1,KM
          DO N=1,NO
            IF(NC(N).GT.0) THEN
              LO(N,K)=LO(N,K).AND.(IBI(K).EQ.0.OR.
     &                (LI(N11(N),K).AND.LI(N21(N),K).AND.
     &                 LI(N12(N),K).AND.LI(N22(N),K)))
              IF(LO(N,K)) THEN
                G11=GI(N11(N),K)
                G21=GI(N21(N),K)
                G12=GI(N12(N),K)
                G22=GI(N22(N),K)
                XO(N,K)=XO(N,K)+WX11(N)*G11+WX21(N)*G21
     &                         +WX12(N)*G12+WX22(N)*G22
                YO(N,K)=YO(N,K)+WY11(N)*G11+WY21(N)*G21
     &                         +WY12(N)*G12+WY22(N)*G22
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE LEFTS AND RIGHTS
        DO N=1,NO
          IF(NC(N).GT.0) THEN
            XI=XPTS(N)
            YI=YPTS(N)
            I1=XI-1
            I2=I1+3
            J1=YI
            J2=J1+1
            XF=XI-I1-1
            YF=YI-J1
            N11(N)=IJKGDS1(I1,J1,IJKGDSA)
            N21(N)=IJKGDS1(I2,J1,IJKGDSA)
            N12(N)=IJKGDS1(I1,J2,IJKGDSA)
            N22(N)=IJKGDS1(I2,J2,IJKGDSA)
            FX=DPR/(RERTH*CLAT(N))
            WX11(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/12*XLON(N)+
     &               (XF*(1-XF)*(2-XF))/12*YLON(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FX
            WX21(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/12*XLON(N)+
     &               (XF*(1-XF)*(1+XF))/12*YLON(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FX
            WX12(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/12*XLON(N)+
     &               (XF*(1-XF)*(2-XF))/12*YLON(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FX
            WX22(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/12*XLON(N)+
     &               (XF*(1-XF)*(1+XF))/12*YLON(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FX
            FY=DPR/RERTH
            WY11(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/12*XLAT(N)+
     &               (XF*(1-XF)*(2-XF))/12*YLAT(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FY
            WY21(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/12*XLAT(N)+
     &               (XF*(1-XF)*(1+XF))/12*YLAT(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FY
            WY12(N)=(((1-XF)*(2-XF)-XF*(2-XF)-XF*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/12*XLAT(N)+
     &               (XF*(1-XF)*(2-XF))/12*YLAT(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FY
            WY22(N)=(((1-XF)*(1+XF)-XF*(1+XF)+XF*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/12*XLAT(N)+
     &               (XF*(1-XF)*(1+XF))/12*YLAT(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FY
          ENDIF
        ENDDO
CMIC$ DO ALL AUTOSCOPE
        DO K=1,KM
          DO N=1,NO
            IF(NC(N).GT.0) THEN
              LO(N,K)=LO(N,K).AND.(IBI(K).EQ.0.OR.
     &                (LI(N11(N),K).AND.LI(N21(N),K).AND.
     &                 LI(N12(N),K).AND.LI(N22(N),K)))
              IF(LO(N,K)) THEN
                G11=GI(N11(N),K)
                G21=GI(N21(N),K)
                G12=GI(N12(N),K)
                G22=GI(N22(N),K)
                XO(N,K)=XO(N,K)+WX11(N)*G11+WX21(N)*G21
     &                         +WX12(N)*G12+WX22(N)*G22
                YO(N,K)=YO(N,K)+WY11(N)*G11+WY21(N)*G21
     &                         +WY12(N)*G12+WY22(N)*G22
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE CENTERS
        DO N=1,NO
          IF(NC(N).GT.0) THEN
            XI=XPTS(N)
            YI=YPTS(N)
            I1=XI
            I2=I1+1
            J1=YI
            J2=J1+1
            XF=XI-I1
            YF=YI-J1
            N11(N)=IJKGDS1(I1,J1,IJKGDSA)
            N21(N)=IJKGDS1(I2,J1,IJKGDSA)
            N12(N)=IJKGDS1(I1,J2,IJKGDSA)
            N22(N)=IJKGDS1(I2,J2,IJKGDSA)
            FX=DPR/(RERTH*CLAT(N))
            WX11(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/4*XLON(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/4*YLON(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FX
            WX21(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/4*XLON(N)+
     &               (-XF*(2-XF)*(1+XF))/4*YLON(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FX
            WX12(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/4*XLON(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/4*YLON(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FX
            WX22(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (-YF*(2-YF)*(1+YF))/4*XLON(N)+
     &               (-XF*(2-XF)*(1+XF))/4*YLON(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FX
            FY=DPR/RERTH
            WY11(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/4*XLAT(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/4*YLAT(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FY
            WY21(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (-(1+YF)*(1-YF)*(2-YF))/4*XLAT(N)+
     &               (-XF*(2-XF)*(1+XF))/4*YLAT(N)*
     &               (-(1-YF)*(2-YF)+(1+YF)*(2-YF)+(1+YF)*(1-YF)))*FY
            WY12(N)=((-(1-XF)*(2-XF)+(1+XF)*(2-XF)+(1+XF)*(1-XF))*
     &               (-YF*(2-YF)*(1+YF))/4*XLAT(N)+
     &               (-(1+XF)*(1-XF)*(2-XF))/4*YLAT(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FY
            WY22(N)=((-(2-XF)*(1+XF)+XF*(1+XF)-XF*(2-XF))*
     &               (-YF*(2-YF)*(1+YF))/4*XLAT(N)+
     &               (-XF*(2-XF)*(1+XF))/4*YLAT(N)*
     &               (-(2-YF)*(1+YF)+YF*(1+YF)-YF*(2-YF)))*FY
          ENDIF
        ENDDO
CMIC$ DO ALL AUTOSCOPE
        DO K=1,KM
          DO N=1,NO
            IF(N11(N).GT.0.AND.(IBI(K).EQ.0.OR.
     &         (LI(N11(N),K).AND.LI(N21(N),K).AND.
     &          LI(N12(N),K).AND.LI(N22(N),K)))) THEN
              G11=GI(N11(N),K)
              G21=GI(N21(N),K)
              G12=GI(N12(N),K)
              G22=GI(N22(N),K)
              IF(LO(N,K)) THEN
                XO(N,K)=XO(N,K)+WX11(N)*G11+WX21(N)*G21
     &                         +WX12(N)*G12+WX22(N)*G22
                YO(N,K)=YO(N,K)+WY11(N)*G11+WY21(N)*G21
     &                         +WY12(N)*G12+WY22(N)*G22
              ELSE
                LO(N,K)=.TRUE.
                XO(N,K)=WX11L(N)*G11+WX21L(N)*G21
     &                  +WX12L(N)*G12+WX22L(N)*G22
                YO(N,K)=WY11L(N)*G11+WY21L(N)*G21
     &                  +WY12L(N)*G12+WY22L(N)*G22
              ENDIF
              XROT=CROT(N)*XO(N,K)-SROT(N)*YO(N,K)
              YROT=SROT(N)*XO(N,K)+CROT(N)*YO(N,K)
              XO(N,K)=XROT
              YO(N,K)=YROT
            ELSE
              IBO(K)=1
              LO(N,K)=.FALSE.
              XO(N,K)=0.
              YO(N,K)=0.
            ENDIF
          ENDDO
        ENDDO
      ENDIF
      IF(KGDSO(1).EQ.0) CALL POLFIXV(NO,MO,KM,RLAT,RLON,IBO,LO,XO,YO)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
