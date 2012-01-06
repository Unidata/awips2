C-----------------------------------------------------------------------
      SUBROUTINE SPSYNTH(I,M,IM,IX,NC,NCTOP,KM,CLAT,PLN,PLNTOP,MP,
     &                   SPC,SPCTOP,F)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPSYNTH     SYNTHESIZE FOURIER FROM SPECTRAL
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: SYNTHESIZES FOURIER COEFFICIENTS FROM SPECTRAL COEFFICIENTS
C           FOR A LATITUDE PAIR (NORTHERN AND SOUTHERN HEMISPHERES).
C           VECTOR COMPONENTS ARE DIVIDED BY COSINE OF LATITUDE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C 1998-12-18  MARK IREDELL  INCLUDE SCALAR AND GRADIENT OPTION
C
C USAGE:    CALL SPSYNTH(I,M,IM,IX,NC,NCTOP,KM,CLAT,PLN,PLNTOP,MP,
C    &                   SPC,SPCTOP,F)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     IM       - INTEGER EVEN NUMBER OF FOURIER COEFFICIENTS
C     IX       - INTEGER DIMENSION OF FOURIER COEFFICIENTS (IX>=IM+2)
C     NC       - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS
C                (NC>=(M+1)*((I+1)*M+2))
C     NCTOP    - INTEGER DIMENSION OF SPECTRAL COEFFICIENTS OVER TOP
C                (NCTOP>=2*(M+1))
C     KM       - INTEGER NUMBER OF FIELDS
C     CLAT     - REAL COSINE OF LATITUDE
C     PLN      - REAL ((M+1)*((I+1)*M+2)/2) LEGENDRE POLYNOMIAL
C     PLNTOP   - REAL (M+1) LEGENDRE POLYNOMIAL OVER TOP
C     SPC      - REAL (NC,KM) SPECTRAL COEFFICIENTS
C     SPCTOP   - REAL (NCTOP,KM) SPECTRAL COEFFICIENTS OVER TOP
C     MP       - INTEGER (KM) IDENTIFIERS (0 FOR SCALAR, 1 FOR VECTOR,
C                OR 10 FOR SCALAR AND GRADIENT)
C
C   OUTPUT ARGUMENT LIST:
C     F        - REAL (IX,2,KM) FOURIER COEFFICIENTS FOR LATITUDE PAIR
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL PLN((M+1)*((I+1)*M+2)/2),PLNTOP(M+1)
      INTEGER MP(KM)
      REAL SPC(NC,KM),SPCTOP(NCTOP,KM)
      REAL F(IX,2,KM)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  ZERO OUT FOURIER COEFFICIENTS.
      DO K=1,KM
        DO L=0,IM/2
          F(2*L+1,1,K)=0.
          F(2*L+2,1,K)=0.
          F(2*L+1,2,K)=0.
          F(2*L+2,2,K)=0.
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SYNTHESIS OVER POLE.
C  INITIALIZE FOURIER COEFFICIENTS WITH TERMS OVER TOP OF THE SPECTRUM.
C  INITIALIZE EVEN AND ODD POLYNOMIALS SEPARATELY.
      IF(CLAT.EQ.0) THEN
        LTOPE=MOD(M+1+I,2)
!C$OMP PARALLEL DO PRIVATE(LB,LE,L,KS,KP,N,F1R,F1I)
        DO K=1,KM
          LB=MP(K)
          LE=MP(K)
          IF(MP(K).EQ.10) THEN
            LB=0
            LE=1
          ENDIF
          L=LB
          IF(L.EQ.1) THEN
            IF(L.EQ.LTOPE) THEN
              F(2*L+1,1,K)=PLNTOP(L+1)*SPCTOP(2*L+1,K)
              F(2*L+2,1,K)=PLNTOP(L+1)*SPCTOP(2*L+2,K)
            ELSE
              F(2*L+1,2,K)=PLNTOP(L+1)*SPCTOP(2*L+1,K)
              F(2*L+2,2,K)=PLNTOP(L+1)*SPCTOP(2*L+2,K)
            ENDIF
          ENDIF
C  FOR EACH ZONAL WAVENUMBER, SYNTHESIZE TERMS OVER TOTAL WAVENUMBER.
C  SYNTHESIZE EVEN AND ODD POLYNOMIALS SEPARATELY.
          DO L=LB,LE
            KS=L*(2*M+(I-1)*(L-1))
            KP=KS/2+1
            DO N=L,I*L+M,2
              F(2*L+1,1,K)=F(2*L+1,1,K)+PLN(KP+N)*SPC(KS+2*N+1,K)
              F(2*L+2,1,K)=F(2*L+2,1,K)+PLN(KP+N)*SPC(KS+2*N+2,K)
            ENDDO
            DO N=L+1,I*L+M,2
              F(2*L+1,2,K)=F(2*L+1,2,K)+PLN(KP+N)*SPC(KS+2*N+1,K)
              F(2*L+2,2,K)=F(2*L+2,2,K)+PLN(KP+N)*SPC(KS+2*N+2,K)
            ENDDO
C  SEPARATE FOURIER COEFFICIENTS FROM EACH HEMISPHERE.
C  ODD POLYNOMIALS CONTRIBUTE NEGATIVELY TO THE SOUTHERN HEMISPHERE.
            F1R=F(2*L+1,1,K)
            F1I=F(2*L+2,1,K)
            F(2*L+1,1,K)=F1R+F(2*L+1,2,K)
            F(2*L+2,1,K)=F1I+F(2*L+2,2,K)
            F(2*L+1,2,K)=F1R-F(2*L+1,2,K)
            F(2*L+2,2,K)=F1I-F(2*L+2,2,K)
          ENDDO
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  SYNTHESIS OVER FINITE LATITUDE.
C  INITIALIZE FOURIER COEFFICIENTS WITH TERMS OVER TOP OF THE SPECTRUM.
C  INITIALIZE EVEN AND ODD POLYNOMIALS SEPARATELY.
      ELSE
        LX=MIN(M,IM/2)
        LTOPE=MOD(M+1,2)
        LTOPO=1-LTOPE
        LE=1+I*LTOPE
        LO=2-I*LTOPO
!C$OMP PARALLEL DO PRIVATE(L,KS,KP,N,F1R,F1I)
        DO K=1,KM
          IF(MP(K).EQ.1) THEN
            DO L=LTOPE,LX,2
              F(2*L+1,LE,K)=PLNTOP(L+1)*SPCTOP(2*L+1,K)
              F(2*L+2,LE,K)=PLNTOP(L+1)*SPCTOP(2*L+2,K)
            ENDDO
            DO L=LTOPO,LX,2
              F(2*L+1,LO,K)=PLNTOP(L+1)*SPCTOP(2*L+1,K)
              F(2*L+2,LO,K)=PLNTOP(L+1)*SPCTOP(2*L+2,K)
            ENDDO
          ENDIF
C  FOR EACH ZONAL WAVENUMBER, SYNTHESIZE TERMS OVER TOTAL WAVENUMBER.
C  SYNTHESIZE EVEN AND ODD POLYNOMIALS SEPARATELY.
          DO L=0,LX
            KS=L*(2*M+(I-1)*(L-1))
            KP=KS/2+1
            DO N=L,I*L+M,2
              F(2*L+1,1,K)=F(2*L+1,1,K)+PLN(KP+N)*SPC(KS+2*N+1,K)
              F(2*L+2,1,K)=F(2*L+2,1,K)+PLN(KP+N)*SPC(KS+2*N+2,K)
            ENDDO
            DO N=L+1,I*L+M,2
              F(2*L+1,2,K)=F(2*L+1,2,K)+PLN(KP+N)*SPC(KS+2*N+1,K)
              F(2*L+2,2,K)=F(2*L+2,2,K)+PLN(KP+N)*SPC(KS+2*N+2,K)
            ENDDO
          ENDDO
C  SEPARATE FOURIER COEFFICIENTS FROM EACH HEMISPHERE.
C  ODD POLYNOMIALS CONTRIBUTE NEGATIVELY TO THE SOUTHERN HEMISPHERE.
C  DIVIDE VECTOR COMPONENTS BY COSINE LATITUDE.
          DO L=0,LX
            F1R=F(2*L+1,1,K)
            F1I=F(2*L+2,1,K)
            F(2*L+1,1,K)=F1R+F(2*L+1,2,K)
            F(2*L+2,1,K)=F1I+F(2*L+2,2,K)
            F(2*L+1,2,K)=F1R-F(2*L+1,2,K)
            F(2*L+2,2,K)=F1I-F(2*L+2,2,K)
          ENDDO
          IF(MP(K).EQ.1) THEN
            DO L=0,LX
              F(2*L+1,1,K)=F(2*L+1,1,K)/CLAT
              F(2*L+2,1,K)=F(2*L+2,1,K)/CLAT
              F(2*L+1,2,K)=F(2*L+1,2,K)/CLAT
              F(2*L+2,2,K)=F(2*L+2,2,K)/CLAT
            ENDDO
          ENDIF
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      END
