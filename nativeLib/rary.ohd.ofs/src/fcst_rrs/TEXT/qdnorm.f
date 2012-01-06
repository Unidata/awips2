C MEMBER  QDNORM
C DESC  DETERMINES NORMALIZING FACTOR FOR THE POWER WAVE DIST FACTORS
C ORIGINALLY BY ED VANBLARGAN - HRL - AUG 85
C
C.......................................................................
C
C THIS ROUTINE DETERMINES THE NORMALIZING FACTOR NEEDED FOR ANY
C GIVEN TIME PERIOD TO MAKE THE POWER WAVE DISTRIBUTION FACTORS
C SUM UP TO 1.0
C
C ARGUMENT LIST
C              IN/
C VARIABLE DIM OUT DESCRIPTION
C -------- --- --- -----------------------------------------------------
C QDIST    24   I  HOURLY DISTRIBUTION FACTORS
C ICOUNT    1   I  NUMBER OF TIME PERIODS EXCLUDED
C IPERD     1   I  NUMBER OF TIME PERIODS (INCL. ICOUNT)
C ISTRT     1   I  INITIAL JULIAN HOUR OF IPERD
C WORK     VAR  I  HOURLY WORK ARRAY
C DNORML    1   O  NORMALIZING FACTOR FOR ALL QDIST VALUES
C.......................................................................
C
      SUBROUTINE QDNORM(QDIST,ICOUNT,IPERD,ISTRT,WORK,DNORML)
C
      DIMENSION QDIST(1),WORK(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_rrs/RCS/qdnorm.f,v $
     . $',                                                             '
     .$Id: qdnorm.f,v 1.1 1995/09/17 19:07:24 dws Exp $
     . $' /
C    ===================================================================
C
      DNORML=1.0
      IF (ICOUNT.EQ.0 .AND. IPERD.EQ.24) GO TO 999
      SUM=0.0
      DO 100 J=1,IPERD
      NHOUR=ISTRT+J
      IF (WORK(NHOUR).GT.-999.) GO TO 100
      IF (NHOUR.GT.24) NHOUR=MOD(NHOUR,24)
      IF (NHOUR.EQ.0) NHOUR=24
      SUM=SUM+QDIST(NHOUR)
100   CONTINUE
      DNORML=0.0
      IF (SUM.GT.0.0001) DNORML=1.0/SUM
999   CONTINUE
      RETURN
      END
