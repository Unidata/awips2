C-----------------------------------------------------------------------
      SUBROUTINE SPUV2DZ(I,M,ENN1,ELONN1,EON,EONTOP,U,V,UTOP,VTOP,D,Z)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPUV2DZ     COMPUTE DIVERGENCE AND VORTICITY FROM WINDS
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE DIVERGENCE AND VORTICITY FROM WIND COMPONENTS
C           IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C           THEN THE DIVERGENCE D IS COMPUTED AS
C             D(L,N)=I*L*A*U(L,N)
C                    +EPS(L,N+1)*N*A*V(L,N+1)-EPS(L,N)*(N+1)*A*V(L,N-1)
C           AND THE VORTICITY Z IS COMPUTED AS
C             Z(L,N)=I*L*A*V(L,N)
C                    -EPS(L,N+1)*N*A*U(L,N+1)+EPS(L,N)*(N+1)*A*U(L,N-1)
C           WHERE U IS THE ZONAL WIND AND V IS THE MERIDIONAL WIND.
C           U AND V ARE WEIGHTED BY THE SECANT OF LATITUDE.
C           EXTRA TERMS ARE USED OVER TOP OF THE SPECTRAL DOMAIN.
C           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPUV2DZ(I,M,ENN1,ELONN1,EON,EONTOP,U,V,UTOP,VTOP,D,Z)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C     U        - REAL ((M+1)*((I+1)*M+2)) ZONAL WIND (OVER COSLAT)
C     V        - REAL ((M+1)*((I+1)*M+2)) MERID WIND (OVER COSLAT)
C     UTOP     - REAL (2*(M+1)) ZONAL WIND (OVER COSLAT) OVER TOP
C     VTOP     - REAL (2*(M+1)) MERID WIND (OVER COSLAT) OVER TOP
C
C   OUTPUT ARGUMENT LIST:
C     D        - REAL ((M+1)*((I+1)*M+2)) DIVERGENCE
C     Z        - REAL ((M+1)*((I+1)*M+2)) VORTICITY
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL ENN1((M+1)*((I+1)*M+2)/2),ELONN1((M+1)*((I+1)*M+2)/2)
      REAL EON((M+1)*((I+1)*M+2)/2),EONTOP(M+1)
      REAL U((M+1)*((I+1)*M+2)),V((M+1)*((I+1)*M+2))
      REAL UTOP(2*(M+1)),VTOP(2*(M+1))
      REAL D((M+1)*((I+1)*M+2)),Z((M+1)*((I+1)*M+2))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE TERMS FROM THE SPECTRAL DOMAIN
      K=1
      D(2*K-1)=0.
      D(2*K)=0.
      Z(2*K-1)=0.
      Z(2*K)=0.
      DO K=2,(M+1)*((I+1)*M+2)/2-1
        D(2*K-1)=-ELONN1(K)*U(2*K)+EON(K+1)*V(2*K+1)-EON(K)*V(2*K-3)
        D(2*K)=ELONN1(K)*U(2*K-1)+EON(K+1)*V(2*K+2)-EON(K)*V(2*K-2)
        Z(2*K-1)=-ELONN1(K)*V(2*K)-EON(K+1)*U(2*K+1)+EON(K)*U(2*K-3)
        Z(2*K)=ELONN1(K)*V(2*K-1)-EON(K+1)*U(2*K+2)+EON(K)*U(2*K-2)
      ENDDO
      K=(M+1)*((I+1)*M+2)/2
      D(2*K-1)=-ELONN1(K)*U(2*K)-EON(K)*V(2*K-3)
      D(2*K)=ELONN1(K)*U(2*K-1)-EON(K)*V(2*K-2)
      Z(2*K-1)=-ELONN1(K)*V(2*K)+EON(K)*U(2*K-3)
      Z(2*K)=ELONN1(K)*V(2*K-1)+EON(K)*U(2*K-2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE TERMS FROM OVER TOP OF THE SPECTRAL DOMAIN
CDIR$ IVDEP
      DO L=0,M
        K=L*(2*M+(I-1)*(L-1))/2+I*L+M+1
        D(2*K-1)=D(2*K-1)+EONTOP(L+1)*VTOP(2*L+1)
        D(2*K)=D(2*K)+EONTOP(L+1)*VTOP(2*L+2)
        Z(2*K-1)=Z(2*K-1)-EONTOP(L+1)*UTOP(2*L+1)
        Z(2*K)=Z(2*K)-EONTOP(L+1)*UTOP(2*L+2)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  MULTIPLY BY LAPLACIAN TERM
      DO K=2,(M+1)*((I+1)*M+2)/2
        D(2*K-1)=D(2*K-1)*ENN1(K)
        D(2*K)=D(2*K)*ENN1(K)
        Z(2*K-1)=Z(2*K-1)*ENN1(K)
        Z(2*K)=Z(2*K)*ENN1(K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
