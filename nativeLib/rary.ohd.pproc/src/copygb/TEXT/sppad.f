C-----------------------------------------------------------------------
      SUBROUTINE SPPAD(I1,M1,Q1,I2,M2,Q2)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM:    SPPAD       PAD OR TRUNCATE A SPECTRAL FIELD
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
C
C ABSTRACT: PAD OR TRUNCATE A SPECTRAL FIELD
C
C PROGRAM HISTORY LOG:
C   91-10-31  MARK IREDELL
C
C USAGE:    CALL SPPAD(I1,M1,Q1,I2,M2,Q2)
C
C   INPUT ARGUMENT LIST:
C     I1       - INTEGER INPUT SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M1       - INTEGER INPUT SPECTRAL TRUNCATION
C     Q1       - REAL ((M+1)*((I+1)*M+2)) INPUT FIELD
C     I2       - INTEGER OUTPUT SPECTRAL DOMAIN SHAPE
C                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)
C     M2       - INTEGER OUTPUT SPECTRAL TRUNCATION
C
C   OUTPUT ARGUMENT LIST:
C     Q2       - REAL ((M+1)*((I+1)*M+2)) OUTPUT FIELD
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      REAL Q1((M1+1)*((I1+1)*M1+2))
      REAL Q2((M2+1)*((I2+1)*M2+2))
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      DO L=0,M2
        DO N=L,I2*L+M2
          KS2=L*(2*M2+(I2-1)*(L-1))+2*N
          IF(L.LE.M1.AND.N.LE.I1*L+M1) THEN
            KS1=L*(2*M1+(I1-1)*(L-1))+2*N
            Q2(KS2+1)=Q1(KS1+1)
            Q2(KS2+2)=Q1(KS1+2)
          ELSE
            Q2(KS2+1)=0
            Q2(KS2+2)=0
          ENDIF
        ENDDO
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
