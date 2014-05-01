C MEMBER PRC2
C  (from old member FCPRC2)
C
      SUBROUTINE PRC2(PO,CO)
C.......................................................................
C     THIS SUBROUTINE PRINTS THE CARRYOVER VALUES FOR
C        THE UNIT HYDROGRAPH OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL -- HRL     AUGUST 1979     VERSION 1
C.......................................................................
      DIMENSION PO(1),CO(1),C(10)
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FCONIT/IVALUE
      COMMON/FENGMT/METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prc2.f,v $
     . $',                                                             '
     .$Id: prc2.f,v 1.1 1995/09/17 18:49:39 dws Exp $
     . $' /
C    ===================================================================
C
      DATA XMM/4H(MM)/,XIN/4H(IN)/
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** PRC2 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE.
C.......................................................................
C     CHECK FOR NO CARRYOVER
      NRO=PO(21)
      ICAR=PO(11)
      IF(NRO.GT.0) GO TO 101
      WRITE (IPR,910) (PO(I),I=2,6)
  910 FORMAT(1H0,10X,39HNO UNIT HYDROGRAPH CARRYOVER VALUES FOR,1X,5A4)
      GO TO 120
  101 CONTINUE
C.......................................................................
C     PRINT TITLE
      WRITE (IPR,902) (PO(I),I=2,6)
  902 FORMAT (1H0,10X,36HUNIT HYDROGRAPH CARRYOVER VALUES FOR,1X,5A4)
C.......................................................................
C     CHECK FOR INITIAL OR OLD CARRYOVER VALUES
      IF (IVALUE.EQ.0) GO TO 100
C     CHECK FOR DEFAULT INITIAL CARRYOVER VALUES
      IF(ICAR.EQ.1) GO TO 100
      WRITE (IPR,904)
  904 FORMAT (1H0,10X,47HINITIAL CARRYOVER VALUES HAVE BEEN SET TO ZERO.
     1)
      GO TO 120
  100 CONTINUE
      IF(METRIC.EQ.1) LMETR=0
      IF(METRIC.EQ.0) LMETR=1
      IF(METRIC.EQ.-1) LMETR=PO(23)
      UNIT=XMM
      IF (LMETR.EQ.1) UNIT=XIN
      DO 110 I=1,NRO,10
      J=I+9
      IF (J.GT.NRO) J=NRO
      WRITE (IPR,906) (K,K=I,J)
  906 FORMAT (1H0,10X,10HCARRYOVER ,I7,14I9)
      II=0
      IF(LMETR.EQ.1) GO TO 106
      DO 102 L=I,J
      II=II+1
  102 C(II)=CO(L)
      WRITE(IPR,907) UNIT,(C(K),K=1,II)
  907 FORMAT(1H0,10X,3HRO ,A4,3X,10F9.2)
      GO TO 110
  106 DO 107 L=I,J
      II=II+1
  107 C(II)=CO(L)/25.4
      WRITE (IPR,908) UNIT,(C(K),K=1,II)
  908 FORMAT (1H0,10X,3HRO ,A4,3X,10F9.3)
  110 CONTINUE
  120 CONTINUE
C.......................................................................
      RETURN
      END
