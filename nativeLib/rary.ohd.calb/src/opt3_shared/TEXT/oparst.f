C MEMBER OPARST
C  (from old member OSCEUA)
C
C===================================================================
C @PROCESS LVL(77)
      SUBROUTINE OPARST(NOPT,NPT,BOUND,X,XMAX,XMIN,XMEAN,XNSTD,GNRNG,
     &                  IPCNVG)
C
C  SUBROUTINE COMPUTING POPULATION STATISTICS
C
      integer, parameter::MaxNopt=100
	  integer, parameter::MaxNpg=2*MaxNopt+1,MaxNpt=MaxNopt*MaxNpg
C      DIMENSION X(1980,16),XMAX(16),XMIN(16)
C      DIMENSION XMEAN(16),XNSTD(16),BOUND(16)
      DIMENSION X(MaxNpt,MaxNopt),XMAX(MaxNopt),XMIN(MaxNopt)
      DIMENSION XMEAN(MaxNopt),XNSTD(MaxNopt),BOUND(MaxNopt)
      PARAMETER (DELTA=1.0E-20,PEPS=1.0E-3)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/oparst.f,v $
     . $',                                                             '
     .$Id: oparst.f,v 1.3 2003/08/26 17:43:25 wkwock Exp $
     . $' /
C    ===================================================================
C
C
C  COMPUTE MAXIMUM, MINIMUM AND STANDARD DEVIATION OF PARAMETER VALUES
      GSUM = 0.0
      DO 20 K = 1, NOPT
        XMAX(K) = -1.0E+20
        XMIN(K) = 1.0E+20
        XSUM1 = 0.0
        XSUM2 = 0.0
        DO 10 I = 1, NPT
          XMAX(K) = AMAX1(X(I,K), XMAX(K))
          XMIN(K) = AMIN1(X(I,K), XMIN(K))
          XSUM1 = XSUM1 + X(I,K)
          XSUM2 = XSUM2 + X(I,K)*X(I,K)
   10   CONTINUE
        XMEAN(K) = XSUM1 / FLOAT(NPT)
        XNSTD(K) = (XSUM2/FLOAT(NPT) - XMEAN(K)*XMEAN(K))
        IF (XNSTD(K) .LE. DELTA) XNSTD(K) = DELTA
        XNSTD(K) = SQRT(XNSTD(K))
        XNSTD(K) = XNSTD(K) / BOUND(K)
        GSUM = GSUM + ALOG( DELTA + (XMAX(K)-XMIN(K))/BOUND(K) )
   20 CONTINUE
      GNRNG = EXP(GSUM/FLOAT(NOPT))
C
C  CHECK PARAMETER CONVERGENCE
      IPCNVG = 0
      IF (GNRNG .LE. PEPS) THEN
        IPCNVG = 1
      END IF
C
C  END OF SUBROUTINE RANGE
      RETURN
      END
