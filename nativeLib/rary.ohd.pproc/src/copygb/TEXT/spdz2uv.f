C-----------------------------------------------------------------------
      SUBROUTINE SPDZ2UV(I,M,ENN1,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPDZ2UV     COMPUTE WINDS FROM DIVERGENCE AND VORTICITY
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE WIND COMPONENTS FROM DIVERGENCE AND VORTICITY
C           IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           IF L IS THE ZONAL WAVENUMBER, N IS THE TOTAL WAVENUMBER,
C           EPS(L,N)=SQRT((N**2-L**2)/(4*N**2-1)) AND A IS EARTH RADIUS,
C           THEN THE ZONAL WIND COMPONENT U IS COMPUTED AS
C             U(L,N)=-I*L/(N*(N+1))*A*D(L,N)
C                    +EPS(L,N+1)/(N+1)*A*Z(L,N+1)-EPS(L,N)/N*A*Z(L,N-1)
C           AND THE MERIDIONAL WIND COMPONENT V IS COMPUTED AS
C             V(L,N)=-I*L/(N*(N+1))*A*Z(L,N)
C                    -EPS(L,N+1)/(N+1)*A*D(L,N+1)+EPS(L,N)/N*A*D(L,N-1)
C           WHERE D IS DIVERGENCE AND Z IS VORTICITY.
C           U AND V ARE WEIGHTED BY THE COSINE OF LATITUDE.
C           EXTRA TERMS ARE COMPUTED OVER TOP OF THE SPECTRAL DOMAIN.
C           ADVANTAGE IS TAKEN OF THE FACT THAT EPS(L,L)=0
C           IN ORDER TO VECTORIZE OVER THE ENTIRE SPECTRAL DOMAIN.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPDZ2UV(I,M,ENN1,ELONN1,EON,EONTOP,D,Z,U,V,UTOP,VTOP)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     ELONN1   - REAL ((M+1)*((I+1)*M+2)/2) L/(N*(N+1))*A
C     EON      - REAL ((M+1)*((I+1)*M+2)/2) EPSILON/N*A
C     EONTOP   - REAL (M+1) EPSILON/N*A OVER TOP
C     D        - REAL ((M+1)*((I+1)*M+2)) DIVERGENCE
C     Z        - REAL ((M+1)*((I+1)*M+2)) VORTICITY
C
C   OUTPUT ARGUMENT LIST:
C     U        - REAL ((M+1)*((I+1)*M+2)) ZONAL WIND (TIMES COSLAT)
C     V        - REAL ((M+1)*((I+1)*M+2)) MERID WIND (TIMES COSLAT)
C     UTOP     - REAL (2*(M+1)) ZONAL WIND (TIMES COSLAT) OVER TOP
C     VTOP     - REAL (2*(M+1)) MERID WIND (TIMES COSLAT) OVER TOP
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL ENN1((M+1)*((I+1)*M+2)/2),ELONN1((M+1)*((I+1)*M+2)/2)
      REAL EON((M+1)*((I+1)*M+2)/2),EONTOP(M+1)
      REAL D((M+1)*((I+1)*M+2)),Z((M+1)*((I+1)*M+2))
      REAL U((M+1)*((I+1)*M+2)),V((M+1)*((I+1)*M+2))
      REAL UTOP(2*(M+1)),VTOP(2*(M+1))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE WINDS IN THE SPECTRAL DOMAIN
      K=1
      U(2*K-1)=EON(K+1)*Z(2*K+1)
      U(2*K)=EON(K+1)*Z(2*K+2)
      V(2*K-1)=-EON(K+1)*D(2*K+1)
      V(2*K)=-EON(K+1)*D(2*K+2)
      DO K=2,(M+1)*((I+1)*M+2)/2-1
        U(2*K-1)=ELONN1(K)*D(2*K)+EON(K+1)*Z(2*K+1)-EON(K)*Z(2*K-3)
        U(2*K)=-ELONN1(K)*D(2*K-1)+EON(K+1)*Z(2*K+2)-EON(K)*Z(2*K-2)
        V(2*K-1)=ELONN1(K)*Z(2*K)-EON(K+1)*D(2*K+1)+EON(K)*D(2*K-3)
        V(2*K)=-ELONN1(K)*Z(2*K-1)-EON(K+1)*D(2*K+2)+EON(K)*D(2*K-2)
      ENDDO
      K=(M+1)*((I+1)*M+2)/2
      U(2*K-1)=ELONN1(K)*D(2*K)-EON(K)*Z(2*K-3)
      U(2*K)=-ELONN1(K)*D(2*K-1)-EON(K)*Z(2*K-2)
      V(2*K-1)=ELONN1(K)*Z(2*K)+EON(K)*D(2*K-3)
      V(2*K)=-ELONN1(K)*Z(2*K-1)+EON(K)*D(2*K-2)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  COMPUTE WINDS OVER TOP OF THE SPECTRAL DOMAIN
      DO L=0,M
        K=L*(2*M+(I-1)*(L-1))/2+I*L+M+1
        UTOP(2*L+1)=-EONTOP(L+1)*Z(2*K-1)
        UTOP(2*L+2)=-EONTOP(L+1)*Z(2*K)
        VTOP(2*L+1)=EONTOP(L+1)*D(2*K-1)
        VTOP(2*L+2)=EONTOP(L+1)*D(2*K)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
