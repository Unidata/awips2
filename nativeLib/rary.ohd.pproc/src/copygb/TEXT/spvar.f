C-----------------------------------------------------------------------
      SUBROUTINE SPVAR(I,M,Q,QVAR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPVAR       COMPUTE VARIANCE BY TOTAL WAVENUMBER
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE VARIANCES BY TOTAL WAVENUMBER
C           OF A SCALAR FIELD IN SPECTRAL SPACE.
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPVAR(I,M,Q,QVAR)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     Q        - REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C
C   OUTPUT ARGUMENT LIST:
C     QVAR     - REAL (0:(I+1)*M) VARIANCES
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL Q((M+1)*((I+1)*M+2))
      REAL QVAR(0:(I+1)*M)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      L=0
      DO N=0,M
        KS=L*(2*M+(I-1)*(L-1))+2*N
        QVAR(N)=0.5*Q(KS+1)**2
      ENDDO
      DO N=M+1,(I+1)*M
        QVAR(N)=0.
      ENDDO
      DO N=0,(I+1)*M
        DO L=MAX(1,N-M),MIN(N,M)
          KS=L*(2*M+(I-1)*(L-1))+2*N
          QVAR(N)=QVAR(N)+Q(KS+1)**2+Q(KS+2)**2
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
