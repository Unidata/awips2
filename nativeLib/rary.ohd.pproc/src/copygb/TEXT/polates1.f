C-----------------------------------------------------------------------
      SUBROUTINE POLATES1(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
     &                    NO,RLAT,RLON,IBO,LO,GO,IRET)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:  POLATES1   INTERPOLATE SCALAR FIELDS (BICUBIC)
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM PERFORMS BICUBIC INTERPOLATION
C           FROM ANY GRID TO ANY GRID FOR SCALAR FIELDS.
C           BITMAPS ARE NOW ALLOWED, BUT BILINEAR INTERPOLATION IS DONE
C           WHEN ANY INVALID DATA IS WITHIN THE BICUBIC TEMPLATE.
C           OPTIONS ALLOW CHOICES BETWEEN STRAIGHT BICUBIC (IPOPT(1)=0)
C           AND CONSTRAINED BICUBIC (IPOPT(1)=1) WHERE THE VALUE IS
C           CONFINED WITHIN THE RANGE OF THE SURROUNDING 4 POINTS.
C           ANOTHER OPTION IS THE MINIMUM PERCENTAGE FOR MASK,
C           I.E. PERCENT VALID INPUT DATA REQUIRED TO MAKE OUTPUT DATA,
C           (IPOPT(2)) WHICH DEFAULTS TO 50 (IF IPOPT(2)=-1).
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
C 2001-06-18  IREDELL  INCLUDE MINIMUM MASK PERCENTAGE OPTION
C
C USAGE:    CALL POLATES1(IPOPT,KGDSI,KGDSO,MI,MO,KM,IBI,LI,GI,
C    &                    NO,RLAT,RLON,IBO,LO,GO,IRET)
C
C   INPUT ARGUMENT LIST:
C     IPOPT    - INTEGER (20) INTERPOLATION OPTIONS
C                IPOPT(1)=0 FOR STRAIGHT BICUBIC;
C                IPOPT(1)=1 FOR CONSTRAINED BICUBIC WHERE VALUE IS
C                CONFINED WITHIN THE RANGE OF THE SURROUNDING 4 POINTS.
C                IPOPT(2) IS MINIMUM PERCENTAGE FOR MASK
C                (DEFAULTS TO 50 IF IPOPT(2)=-1)
C     KGDSI    - INTEGER (200) INPUT GDS PARAMETERS AS DECODED BY W3FI63
C     KGDSO    - INTEGER (200) OUTPUT GDS PARAMETERS
C                (KGDSO(1)<0 IMPLIES RANDOM STATION POINTS)
C     MI       - INTEGER SKIP NUMBER BETWEEN INPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF INPUT GRID FIELDS IF KM=1
C     MO       - INTEGER SKIP NUMBER BETWEEN OUTPUT GRID FIELDS IF KM>1
C                OR DIMENSION OF OUTPUT GRID FIELDS IF KM=1
C     KM       - INTEGER NUMBER OF FIELDS TO INTERPOLATE
C     IBI      - INTEGER (KM) INPUT BITMAP FLAGS
C     LI       - LOGICAL*1 (MI,KM) INPUT BITMAPS (IF SOME IBI(K)=1)
C     GI       - REAL (MI,KM) INPUT FIELDS TO INTERPOLATE
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)<0)
C     RLAT     - REAL (NO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)<0)
C     RLON     - REAL (NO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)<0)
C
C   OUTPUT ARGUMENT LIST:
C     NO       - INTEGER NUMBER OF OUTPUT POINTS (ONLY IF KGDSO(1)>=0)
C     RLAT     - REAL (MO) OUTPUT LATITUDES IN DEGREES (IF KGDSO(1)>=0)
C     RLON     - REAL (MO) OUTPUT LONGITUDES IN DEGREES (IF KGDSO(1)>=0)
C     IBO      - INTEGER (KM) OUTPUT BITMAP FLAGS
C     LO       - LOGICAL*1 (MO,KM) OUTPUT BITMAPS (ALWAYS OUTPUT)
C     GO       - REAL (MO,KM) OUTPUT FIELDS INTERPOLATED
C     IRET     - INTEGER RETURN CODE
C                0    SUCCESSFUL INTERPOLATION
C                2    UNRECOGNIZED INPUT GRID OR NO GRID OVERLAP
C                3    UNRECOGNIZED OUTPUT GRID
C
C SUBPROGRAMS CALLED:
C   GDSWIZ       GRID DESCRIPTION SECTION WIZARD
C   IJKGDS0      SET UP PARAMETERS FOR IJKGDS1
C   (IJKGDS1)    RETURN FIELD POSITION FOR A GIVEN GRID POINT
C   POLFIXS      MAKE MULTIPLE POLE SCALAR VALUES CONSISTENT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C
C$$$
CFPP$ EXPAND(IJKGDS1)
      INTEGER IPOPT(20)
      INTEGER KGDSI(200),KGDSO(200)
      INTEGER IBI(KM),IBO(KM)
      LOGICAL*1 LI(MI,KM),LO(MO,KM)
      REAL GI(MI,KM),GO(MO,KM)
      REAL RLAT(MO),RLON(MO)
      REAL XPTS(MO),YPTS(MO)
      INTEGER N11(MO),N21(MO),N12(MO),N22(MO)
      INTEGER NC(MO)
      REAL W11(MO),W21(MO),W12(MO),W22(MO)
      INTEGER IJKGDSA(20)
      PARAMETER(FILL=-9999.)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE NUMBER OF OUTPUT POINTS AND THEIR LATITUDES AND LONGITUDES.
      IRET=0
      IF(KGDSO(1).GE.0) THEN
        CALL GDSWIZ(KGDSO, 0,MO,FILL,XPTS,YPTS,RLON,RLAT,NO,0,DUM,DUM)
        IF(NO.EQ.0) IRET=3
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SET PARAMETERS
      MCON=IPOPT(1)
      MP=IPOPT(2)
      IF(MP.EQ.-1.OR.MP.EQ.0) MP=50
      IF(MP.LT.0.OR.MP.GT.100) IRET=32
      PMP=MP*0.01
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  LOCATE INPUT POINTS AND COMPUTE THEIR WEIGHTS
      CALL GDSWIZ(KGDSI,-1,NO,FILL,XPTS,YPTS,RLON,RLAT,NV,0,DUM,DUM)
      IF(IRET.EQ.0.AND.NV.EQ.0) IRET=2
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ZERO OUT OUTPUT
CMIC$ DO ALL AUTOSCOPE
      DO K=1,KM
        IBO(K)=IBI(K)
        DO N=1,NO
          LO(N,K)=.FALSE.
          GO(N,K)=0.
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
          IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
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
              W=XF*(1-XF)*YF*(1-YF)/36
              W11(N)=W*(2-XF)*(2-YF)
              W21(N)=W*(1+XF)*(2-YF)
              W12(N)=W*(2-XF)*(1+YF)
              W22(N)=W*(1+XF)*(1+YF)
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
                GO(N,K)=W11(N)*G11+W21(N)*G21
     &                 +W12(N)*G12+W22(N)*G22
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
            W=-(2-XF)*(1+XF)*YF*(1-YF)/12
            W11(N)=W*(1-XF)*(2-YF)
            W21(N)=W*XF*(2-YF)
            W12(N)=W*(1-XF)*(1+YF)
            W22(N)=W*XF*(1+YF)
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
                GO(N,K)=GO(N,K)+W11(N)*G11+W21(N)*G21
     &                         +W12(N)*G12+W22(N)*G22
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
            W=-XF*(1-XF)*(2-YF)*(1+YF)/12
            W11(N)=W*(2-XF)*(1-YF)
            W21(N)=W*(1+XF)*(1-YF)
            W12(N)=W*(2-XF)*YF
            W22(N)=W*(1+XF)*YF
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
                GO(N,K)=GO(N,K)+W11(N)*G11+W21(N)*G21
     &                         +W12(N)*G12+W22(N)*G22
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE CENTERS
        DO N=1,NO
          XI=XPTS(N)
          YI=YPTS(N)
          IF(XI.NE.FILL.AND.YI.NE.FILL) THEN
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
          IF(NC(N).GT.0.OR.MIN(N11(N),N21(N),N12(N),N22(N)).GT.0) THEN
              W=(1+XF)*(2-XF)*(1+YF)*(2-YF)/4
              W11(N)=W*(1-XF)*(1-YF)
              W21(N)=W*XF*(1-YF)
              W12(N)=W*(1-XF)*YF
              W22(N)=W*XF*YF
            ELSE
              N11(N)=0
            ENDIF
          ELSE
            N11(N)=0
          ENDIF
        ENDDO
CMIC$ DO ALL AUTOSCOPE
        DO K=1,KM
          DO N=1,NO
            IF(N11(N).GT.0) THEN
              LO(N,K)=LO(N,K).AND.(IBI(K).EQ.0.OR.
     &                (LI(N11(N),K).AND.LI(N21(N),K).AND.
     &                 LI(N12(N),K).AND.LI(N22(N),K)))
              IF(LO(N,K)) THEN
                G11=GI(N11(N),K)
                G21=GI(N21(N),K)
                G12=GI(N12(N),K)
                G22=GI(N22(N),K)
                GO(N,K)=GO(N,K)+W11(N)*G11+W21(N)*G21
     &                         +W12(N)*G12+W22(N)*G22
                IF(MCON.GT.0) THEN
                  GMIN=MIN(G11,G21,G12,G22)
                  GMAX=MAX(G11,G21,G12,G22)
                  GO(N,K)=MIN(MAX(GO(N,K),GMIN),GMAX)
                ENDIF
              ELSE
                GO(N,K)=0.
                W=0.
                IF(LI(N11(N),K)) THEN
                  GO(N,K)=GO(N,K)+W11(N)*GI(N11(N),K)
                  W=W+W11(N)
                ENDIF
                IF(LI(N21(N),K)) THEN
                  GO(N,K)=GO(N,K)+W21(N)*GI(N21(N),K)
                  W=W+W21(N)
                ENDIF
                IF(LI(N12(N),K)) THEN
                  GO(N,K)=GO(N,K)+W12(N)*GI(N12(N),K)
                  W=W+W12(N)
                ENDIF
                IF(LI(N22(N),K)) THEN
                  GO(N,K)=GO(N,K)+W22(N)*GI(N22(N),K)
                  W=W+W22(N)
                ENDIF
                LO(N,K)=W.GE.PMP*(W11(N)+W21(N)+W12(N)+W22(N))
                IF(LO(N,K)) THEN
                  GO(N,K)=GO(N,K)/W
                ELSE
                  GO(N,K)=0.
                ENDIF
              ENDIF
            ELSE
              IBO(K)=1
              LO(N,K)=.FALSE.
              GO(N,K)=0.
            ENDIF
          ENDDO
        ENDDO
        IF(KGDSO(1).EQ.0) CALL POLFIXS(NO,MO,KM,RLAT,RLON,IBO,LO,GO)
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
