      SUBROUTINE CMOUT(NSTA,STNAME,CM,IGM)

C     PRINTS CORRELATION TABLES (ANNUAL OR SEASONAL).
c
c specify the maximum number of stations allowed
c
      parameter (nsx=500)
      DIMENSION STNAME(nsx,5),IGN(4),R(4),SNAME(4,5)
      DIMENSION CM(nsx,nsx),IGM(nsx,nsx)

      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/cmout.f,v $
     . $',                                                             '
     .$Id: cmout.f,v 1.3 2002/05/15 17:29:17 dws Exp $
     . $' /
C    ===================================================================
C

      NLINES=NSTA+1
      WRITE(IPR,900)
  900 FORMAT(1H ,61HESTIMATORS FOR EACH STATION IN ORDER BY DEGREE OF CO
     1RRELATON.,5X,25HR=CORRELATION COEFFICIENT)
      NE=NSTA-1
      IB=1
   12 IL=IB+3
      IF(IL.GT.NSTA)IL=NSTA
      WRITE(IPR,901)((STNAME(I,J),J=1,5),I=IB,IL)
  901 FORMAT(1H0,//,1X,4(1X,1H#,2X,5A4,4X,1HR,4X))
      WRITE(IPR,902)
  902 FORMAT(1H ,4(4X,20H********************,9X))
      DO 10 N=1,NE
      DO 11 I=IB,IL
      II=I-IB+1
      IG=IGM(I,N)
      IGN(II)=IG
      R(II)=CM(I,IG)
      IF(R(II).EQ.0.01) R(II)=-9.999
      DO 11 J=1,5
      SNAME(II,J)=STNAME(IG,J)
   11 CONTINUE
      II=IL-IB+1
      WRITE(IPR,903)(IGN(I),(SNAME(I,J),J=1,5),R(I),I=1,II)
  903 FORMAT(1H ,4(I3,1X,5A4,F6.3,3X))
   10 CONTINUE
      IF(IL.EQ.NSTA)RETURN
      IB=IL+1
      GO TO 12
      END
