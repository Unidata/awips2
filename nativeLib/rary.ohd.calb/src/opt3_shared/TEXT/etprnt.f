C MEMBER ETPRNT
C  (from old member OPRINT)
C
      SUBROUTINE ETPRNT(POLD,P,MP)
C..................................
C     THIS SUBROUTINE PRINTS THE OLD AND NEW ET CURVES
C     FOR THE OPERATIONS THAT HAVE BEEN OPTIMIZED.
C..................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   JUNE 1981   VERSION 1
C..................................
C
      DIMENSION P(MP),POLD(MP)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/etprnt.f,v $
     . $',                                                             '
     .$Id: etprnt.f,v 1.1 1996/07/11 20:43:13 dws Exp $
     . $' /
C    ===================================================================
C
C
C     PRINT OLD ET VALUES.
C
      WRITE(IPR,900)
  900 FORMAT(1HO,24HINITIAL ET CURVE VALUES:)
C
      WRITE(IPR,920) (K,K=1,12)
  920 FORMAT(1H0,10X,20H16TH OF MONTH VALUES,12I6)
C
      WRITE(IPR,922) (POLD(K),K=1,12)
  922 FORMAT(1H ,10X,13HPE-ADJUSTMENT,7X,12F6.2)
C
C     PRINT NEW ET CURVE VALUES.
C
      WRITE(IPR,930)
  930 FORMAT(1H0,26HOPTIMIZED ET CURVE VALUES:)
C
      WRITE(IPR,940) (K,K=1,12)
  940 FORMAT(1H0,10X,20H16TH OF MONTH VALUES,12I6)
C
      WRITE(IPR,942) (P(K),K=1,12)
  942 FORMAT(1H ,10X,13HPE-ADJUSTMENT,7X,12F6.2)
C
      RETURN
      END
