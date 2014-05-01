C MEMBER UGPRNT
C  (from old member OPRINT)
C
      SUBROUTINE UGPRNT(NV,POLD,P,MP,LMETR)
C..................................
C     THIS SUBROUTINE PRINTS THE OLD AND NEW UNIT HYDROGRAPHS
C     FOR OPERATIONS THAT HAVE BEEN OPTIMIZED.
C..................................
C
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL   JUNE 1981   VERSION 1
C..................................
C
      DIMENSION P(MP),POLD(MP),PNEW(100)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/ugprnt.f,v $
     . $',                                                             '
     .$Id: ugprnt.f,v 1.1 1996/07/11 21:02:36 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA ENGFAC/896.9925/
C
      SCALE=1.0
      IF(LMETR.EQ.1) SCALE=ENGFAC
C
C     PRINT OLD UNIT GRAPH VALUES.
C
      WRITE(IPR,900)
  900 FORMAT(1H0,31HINITIAL UNIT HYDROGRAPH VALUES:)
C
      DO 100 I=1,NV,15
      J=I+14
      IF(J.GT.NV) J=NV
C
      DO 150 K=I,J
      POLD(K)=POLD(K)*SCALE
  150 CONTINUE
C
      WRITE(IPR,920) (K,K=I,J)
  920 FORMAT(1H0,10X,10HORDINATE  ,I5,14I7)
      IF(LMETR.EQ.0) WRITE(IPR,922) (POLD(K),K=I,J)
      IF(LMETR.EQ.1) WRITE(IPR,921) (POLD(K),K=I,J)
  921 FORMAT(1H ,10X,13HQ (CFS/IN)   ,15F7.0)
  922 FORMAT(1H ,10X,13HQ (CMS/MM)   ,15F7.2)
  100 CONTINUE
C
C     PRINT NEW UNIT GRAPH VALUES.
C
      WRITE(IPR,930)
  930 FORMAT(1H0,28HOPTIMIZED UNIT GRAPH VALUES:)
C
      DO 200 I=1,NV,15
      J=I+14
      IF(J.GT.NV) J=NV
C
      DO 250 K=I,J
      PNEW(K)=P(K)*SCALE
  250 CONTINUE
C
      WRITE(IPR,940) (K,K=I,J)
  940 FORMAT(1H0,10X,10HORDINATE  ,I5,14I7)
      IF(LMETR.EQ.0) WRITE(IPR,942) (PNEW(K),K=I,J)
      IF(LMETR.EQ.1) WRITE(IPR,941) (PNEW(K),K=I,J)
  941 FORMAT(1H ,10X,13HQ (CFS/IN)   ,15F7.0)
  942 FORMAT(1H ,10X,13HQ (CMS/MM)   ,15F7.2)
C
  200 CONTINUE
      RETURN
      END
