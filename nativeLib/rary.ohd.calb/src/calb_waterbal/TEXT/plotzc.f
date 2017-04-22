C MEMBER PLOTZC
C  (from old member MCEX40)
C
      SUBROUTINE PLOTZC(PO,WORK,MON,I1,K)
C***************************************
C
C     THIS SUBROUTINE GENERATES THE OPTIONAL MULTI-YEAR AVERAGE ZONE
C     CONTENTS DISPLAY.
C
C     THIS SUBROUTINE WAS INITIALLY WRITTEN BY:
C     ROBERT M. HARPER     HRL         MAY 1991
C***************************************
C
      DIMENSION PO(1),WORK(1),MON(12),SNAME(2)
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/calbrt'
      INCLUDE 'common/fengmt'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_waterbal/RCS/plotzc.f,v $
     . $',                                                             '
     .$Id: plotzc.f,v 1.2 1996/07/11 19:35:49 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/'PLOT','ZC  '/
C***************************************
C
C     TRACE LEVEL=1, NO DEBUG OUTPUT
      IF(ITRACE .GE. 1) WRITE(IODBUG,900) SNAME
  900 FORMAT(/,5X,'** ',2A4,' ENTERED')
C***************************************
C
C     CONTROL VARIABLES
      NAM=I1+4
      L=1
      KMO=10
C***************************************
C
C     PRINT TABLE
      IF(K .GT. 1) GO TO 20
      WRITE(IPR,905) (PO(J),J=I1,NAM),IMO,IYR,LMO,LYR
      GO TO 30
   20 WRITE(IPR,910) (PO(J),J=I1,NAM),IMO,IYR,LMO,LYR
   30 WRITE(IPR,920)
      DO 10 N=1,12
        L1=L+14
        IF(METRIC .EQ. 0) GO TO 40
        WRITE(IPR,930) MON(KMO),(WORK(J),J=L,L1)
        GO TO 50
   40   WRITE(IPR,940) MON(KMO),(WORK(J),J=L,L1)
   50   L=L+16
        KMO=KMO+1
        IF(KMO .LT. 13) GO TO 10
        KMO=1
   10 CONTINUE
C***************************************
C
  905 FORMAT(1H1,/38X,'MULTI-YEAR AVERAGE ZONE CONTENTS FOR ',5A4,/48X,'
     &BASED ON THE PERIOD',2X,I2,'/',I4,'-',I2,'/',I4)
  910 FORMAT(///38X,'MULTI-YEAR AVERAGE ZONE CONTENTS FOR ',5A4,/48X,'BA
     &SED ON THE PERIOD',2X,I2,'/',I4,'-',I2,'/',I4)
  920 FORMAT(/8X,'MONTH',12X,'UZTWC',17X,'UZFWC',17X,'LZTWC',17X,'LZFSC'
     &,17X,'LZFPC',/8X,5('-'),5X,18('-'),4X,18('-'),4X,18('-'),4X,18('-'
     &),4X,18('-'),/12X,5(7X,'AVG.',3X,'HI',4X,'LO'))
  930 FORMAT(9X,A4,5(5X,F5.0,1X,F5.0,1X,F5.0))
  940 FORMAT(9X,A4,5(2X,F6.2,1X,F6.2,1X,F6.2))
C
      RETURN
      END
