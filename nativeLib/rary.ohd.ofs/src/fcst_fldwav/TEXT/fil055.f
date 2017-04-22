C-----------------------------------------------------------------------
C     ONE OF THE SUBPROGRAMS FOR KALMAN FILTER
C     IT SETS THE OBSERVATION MATRIX AND NOISE STATISTICS
C     Follwing statisticies can be adjusted by user.
C          R0   ----   Variance of observed stages.
C          Q0   ----   Variance of noiseis of St.Venant Equations.
C          P0H  ----   Variance of initial depths (h)
C          P0Q  ----   Variance of initial discharges (Q)
C-----------------------------------------------------------------------
      SUBROUTINE FIL055(NS,NO,KC,P,R,Q,H,NGS,JR,K1,K4)
      DIMENSION R(NO,NO),Q(2*NS,2*NS),H(NO,2*NS),NGS(K4,K1),P(2*NS,2*NS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/fil055.f,v $
     . $',                                                             '
     .$Id: fil055.f,v 1.1 1999/04/23 18:08:32 dws Exp $
     . $' /
C    ===================================================================
C
              R0=0.04
              Q0=0.15
              P0H=0.01
              P0Q=10**3
      NS2=2*NS
      DO 100 I=1,NO
      DO 100 J=1,NS2
100   H(I,J)=0.0
      DO 120 I=1,NO
      IJK=2*NGS(I,JR)
120   H(I,IJK)=1.0
      DO 150 I=1,NO
      DO 150 J=1,NO
150   R(I,J)=0.0
      DO 200 I=1,NO
200   R(I,I)=R0
      DO 220 I=1,NS2
      DO 220 J=1,NS2
220   Q(I,J)=Q0                              
C-----------------  INITIAL SETTING AND INPUT FOR P  -------------------
              KC=1
              IF (KC .GE. 2) GOTO 1000
              DO 300 I=1,NS2
              DO 300 J=1,NS2
              P(I,J)=0.0
300           CONTINUE
              DO 320 I=1,NS              
              P(2*I,2*I)=P0H
              P(2*I-1,2*I-1)=P0Q
320           CONTINUE
1000  RETURN
      END
