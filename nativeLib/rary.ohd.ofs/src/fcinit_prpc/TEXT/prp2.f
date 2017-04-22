C MEMBER PRP2
C  (from old member FCPRP2)
C
      SUBROUTINE PRP2(PO)
C.......................................................................
C     THIS SUBROUTINE PRINTS PARAMETER VALUES FOR
C        THE UNIT HYDROGRAPH OPERATION.
C.......................................................................
C     SUBROUTINE INITIALLY WRITTEN BY
C            LARRY BRAZIL - HRL     AUGUST 1979     VERSION 1
C.......................................................................
      DIMENSION PO(1),XCMS(2),XCFS(2),P(10),SQKM(2),SQMI(2),UNIT2(2)
C     COMMON BLOCKS.
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FENGMT/METRIC
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp2.f,v $
     . $',                                                             '
     .$Id: prp2.f,v 1.1 1995/09/17 18:50:04 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA XCMS/4H(CMS,4H/MM)/,XCFS/4H(CFS,4H/IN)/
      DATA SQKM/4HSQ.K,4HM.  /,SQMI/4HSQ.M,4HI.  /
      DATA CMS/4HCMS /,CFS/4HCFS /
C
C.......................................................................
C     CHECK TRACE LEVEL -- TRACE LEVEL FOR THIS SUBROUTINE=1.
      IF (ITRACE.GE.1) WRITE (IODBUG,900)
  900 FORMAT (1H0,15H** PRP2 ENTERED)
C     NO DEBUG OUTPUT FOR THIS SUBROUTINE
C.......................................................................
C     PRINT TITLE
      WRITE (IPR,903) (PO(I),I=2,6)
  903 FORMAT (1H0,10X,29HUNIT HYDROGRAPH OPERATION FOR, 1X,5A4)
      IDTR=PO(16)
      WRITE (IPR,904) IDTR
  904 FORMAT (1H0,15X,30HCOMPUTATIONAL TIME INTERVAL IS,I3,1X,6HHOURS.)
C.......................................................................
C     PRINT TIME SERIES USED BY THIS OPERATION.
      WRITE (IPR,905)
  905 FORMAT (1H0,20X,35HTIME SERIES USED BY THIS OPERATION.)
      WRITE (IPR,906)
  906 FORMAT (1H0,15X,8HCONTENTS,14X,4HI.D.,7X,4HTYPE,5X,
     113HTIME INTERVAL)
      WRITE (IPR,908) PO(13),PO(14),PO(15), IDTR
  908 FORMAT (1H0,10X,23HCHANNEL INFLOW (RUNOFF),2X,2A4,5X,A4,7X,
     1I2,1X,5HHOURS)
      IDTQ=PO(20)
      WRITE (IPR,910) PO(17),PO(18),PO(19), IDTQ
  910 FORMAT (1H ,10X,23HINSTANTANEOUS DISCHARGE,2X,2A4,5X,A4,7X,
     1I2,1X,5HHOURS)
      NV=PO(10)
C.......................................................................
C     PRINT PARAMETER VALUES.
      WRITE (IPR,912) IDTR,NV,IDTQ
  912 FORMAT (1H0,10X,I2,23H-HOUR UNIT HYDROGRAPH: ,I3,1X,
     121HORDINATES DEFINED AT ,I3,15H-HOUR INTERVALS)
      IF(METRIC.EQ.1) LMETR=0
      IF(METRIC.EQ.0) LMETR=1
      IF(METRIC.EQ.-1) LMETR=PO(23)
      UNIT2(1)=SQKM(1)
      UNIT2(2)=SQKM(2)
      AREA=PO(8)
      IF(LMETR.EQ.0) GO TO 30
      XKM=((.3048*5280.)/1000.)**2
      AREA=PO(8)/XKM
      UNIT2(1)=SQMI(1)
      UNIT2(2)=SQMI(2)
   30 WRITE (IPR,914) AREA,UNIT2
  914 FORMAT (1H0,10X,47HTHE UNIT HYDROGRAPH REPRESENTS AN AREA OF ABOUT
     1,F8.1,1X,2A4)
C.......................................................................
C     PRINT UNIT HYDROGRAPH ORDINATES.
      IORD=PO(22)-1
      IF (LMETR.EQ.1) GO TO 51
      UNIT2(1)=XCMS(1)
      UNIT2(2)=XCMS(2)
      GO TO 50
  51  UNIT2(1)=XCFS(1)
      UNIT2(2)=XCFS(2)
      XPO=((0.3048)**3)/25.4
  50  DO 100 I=1,NV,10
      J=I+9
      IF (J.GT.NV) J=NV
      WRITE (IPR,920) (K,K=I,J)
  920 FORMAT (1H0,10X,10HORDINATE  ,I7,14I9)
      II=0
      IF(LMETR.EQ.1) GO TO 60
      DO 52 L=I,J
      II=II+1
   52 P(II)=PO(IORD+L)
      WRITE(IPR,921) UNIT2,(P(K),K=1,II)
  921 FORMAT(1H0,10X,2HQ ,2A4,10F9.2)
      GO TO 100
  60  DO 62 L=I,J
      II=II+1
   62 P(II)=PO(IORD+L)/XPO
      WRITE(IPR,922) UNIT2,(P(K),K=1,II)
  922 FORMAT (1H0,10X,2HQ ,2A4,10F9.0)
  100 CONTINUE
C.......................................................................
      IF(PO(24).GT.0.01) GO TO 120
      WRITE(IPR,924)
  924 FORMAT(1H0,10X,53HNO BASEFLOW WILL BE ADDED TO THE COMPUTED DISCHA
     *RGES.)
      GO TO 122
  120 UNIT=CMS
      IF(LMETR.EQ.1) UNIT=CFS
      BASE=PO(24)
      IF(LMETR.EQ.1) BASE=BASE/(.3048)**3
      WRITE(IPR,926) BASE,UNIT
  926 FORMAT(1H0,10X,14HA BASEFLOW OF ,F9.2,1X,A4,41HWILL BE ADDED TO TH
     *E COMPUTED DISCHARGES.)
  122 CONTINUE
      RETURN
      END
