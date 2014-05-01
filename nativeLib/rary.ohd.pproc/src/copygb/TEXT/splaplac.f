C-----------------------------------------------------------------------
      SUBROUTINE SPLAPLAC(I,M,ENN1,Q,QD2,IDIR)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPLAPLAC    COMPUTE LAPLACIAN IN SPECTRAL SPACE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: COMPUTES THE LAPLACIAN OR THE INVERSE LAPLACIAN
C           OF A SCALAR FIELD IN SPECTRAL SPACE.
C           SUBPROGRAM SPEPS SHOULD BE CALLED ALREADY.
C           THE LAPLACIAN OF Q(L,N) IS SIMPLY -N*(N+1)/A**2*Q(L,N)
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPLAPLAC(I,M,ENN1,Q,QD2,IDIR)
C
C   INPUT ARGUMENT LIST:
C     I        - INTEGER SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M        - INTEGER SPECTRAL TRUNCATION
C     ENN1     - REAL ((M+1)*((I+1)*M+2)/2) N*(N+1)/A**2
C     Q        - IF IDIR > 0, REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C     QD2      - IF IDIR < 0, REAL ((M+1)*((I+1)*M+2)) LAPLACIAN
C     IDIR     - INTEGER FLAG
C                IDIR > 0 TO TAKE LAPLACIAN
C                IDIR < 0 TO TAKE INVERSE LAPLACIAN
C
C   OUTPUT ARGUMENT LIST:
C     Q        - IF IDIR < 0, REAL ((M+1)*((I+1)*M+2)) SCALAR FIELD
C                (Q(0,0) IS NOT COMPUTED)
C     QD2      - IF IDIR > 0, REAL ((M+1)*((I+1)*M+2)) LAPLACIAN
C
C ATTRIBUTES:
C   LANGUAGE: CRAY FORTRAN
C
C$$$
      REAL ENN1((M+1)*((I+1)*M+2)/2)
      REAL Q((M+1)*((I+1)*M+2))
      REAL QD2((M+1)*((I+1)*M+2))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TAKE LAPLACIAN
      IF(IDIR.GT.0) THEN
        K=1
        QD2(2*K-1)=0.
        QD2(2*K)=0.
        DO K=2,(M+1)*((I+1)*M+2)/2
          QD2(2*K-1)=Q(2*K-1)*(-ENN1(K))
          QD2(2*K)=Q(2*K)*(-ENN1(K))
        ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  TAKE INVERSE LAPLACIAN
      ELSE
        DO K=2,(M+1)*((I+1)*M+2)/2
          Q(2*K-1)=QD2(2*K-1)/(-ENN1(K))
          Q(2*K)=QD2(2*K)/(-ENN1(K))
        ENDDO
      ENDIF
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
