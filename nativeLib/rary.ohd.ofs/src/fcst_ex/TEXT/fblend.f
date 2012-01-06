C MEMBER FBLEND
C  (from old member FCFBLEND)
C.......................................................................
      SUBROUTINE FBLEND(Q,DQ,NBLND,NSTEPS,IB,IE)
C.......................................................................
C     SUBROUTINE ADJUSTS SIMULATED DISCHARGES FOR THE DIFFERENCE
C     BETWEEN SIMULATED AND OBSERVED AT THE LAST OBSERVED ORDINATE
C     BY BLENDING THE DIFFERENCE IN A PRE-DEFINED NUMBER OF STEPS.
C.......................................................................
C     PROGRAMMED BY KAY KROUSE   FEBRUARY 1980
C.......................................................................
C     VARIABLES IN ARGUMENT LIST:
C             Q      -SIMULATED DISCHARGE ARRAY TO BE ADJUSTED
C             DQ     -DIFFERENCE BETWEEN SIMULATED AND OBSERVED
C                     DISCHARGE AT THE LAST OBSERVED ORDINATE
C             NBLND  -NUMBER OF STEPS IN TOTAL BLEND THAT HAVE BEEN
C                     COMPLETED IN A PREVIOUS BLEND
C             NSTEPS -TOTAL NUMBER OF BLENDING STEPS
C             IB     -FIRST ORDINATE OF Q ARRAY TO BE ADJUSTED
C             IE     -LAST AVAILABLE ORDINATE IN Q ARRAY
C.......................................................................
C
      DIMENSION Q(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/fblend.f,v $
     . $',                                                             '
     .$Id: fblend.f,v 1.1 1995/09/17 18:57:31 dws Exp $
     . $' /
C    ===================================================================
C
      LAST=IB+NSTEPS-NBLND-1
      IF(LAST.LE.IE) GO TO 5
      NB=NSTEPS-(LAST-IE)
      LAST=IE
      GO TO 10
 5    NB=0
C
 10   M=NSTEPS-NBLND
      DO 20 I=IB,LAST
      K=I-IB+1
      Q(I)=Q(I)+(DQ/NSTEPS)*(M-K)
      IF(Q(I).LT.0.0)Q(I)=0.0
 20   CONTINUE
      NBLND=NB
      RETURN
      END
