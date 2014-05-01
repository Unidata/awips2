      SUBROUTINE WYSUM(N,RO,GNAME,NMO,IMO,IYR,LYR,UNITS)
C     SUBROUTINE TO PRINT MONTHLY AND WATER YEAR PRECIPITATION SUMMARY

      INCLUDE 'common/ionum'
c
c specify the maximum number of stations allowed
c
      PARAMETER (NSX=500,MaxMonth=1200)
      DIMENSION RO(nsx,MaxMonth),GNAME(nsx,5),SUM(12),CM(12),T(12)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/wysum.f,v $
     . $',                                                             '
     .$Id: wysum.f,v 1.3 2002/05/15 18:45:22 dws Exp $
     . $' /
C    ===================================================================
C
C     PRINT HEADING.
      NLINES=LYR-IYR+12
      CALL ULINE(IPR,NLINES)
      write(IPR,900) UNITS
  900 FORMAT(///1X,48HSUMMARY OF MONTHLY AND WATER YEAR PRECIPITATION.,
     16HUNITS=,A4)
      write(IPR,901) (GNAME(N,I),I=1,5)
  901 FORMAT(1H0,10X,5A4)
      write(IPR,902) (I,I=10,12),(I,I=1,9)
  902 FORMAT(1H0,10HWATER YEAR,12I8,5X,5HTOTAL)
      DO 11 MO=1,12
      SUM(MO)=0.0
      CM(MO)=0.0
      T(MO)=-999.0
   11 CONTINUE
      WYT=0.0
      MSG=0
      XWYSUM=0.0
      CWY=0.0
      IF(IMO.LT.10)GO TO 12
      MO=IMO-9
      IWY=IYR+1
      GO TO 13
   12 MO=IMO+3
      IWY=IYR
   13 KWY=IWY
      IF (MO.NE.1) MSG=1
      DO 20 M=1,NMO
      T(MO)=RO(N,M)
      IF((RO(N,M).LT.-998.0).OR.(MO.LT.12.AND.M.EQ.NMO)) MSG=1
      SUM(MO)=SUM(MO)+ABS(T(MO))
      CM(MO)=CM(MO)+1.0
      WYT=WYT+ABS(T(MO))
      IF(M.EQ.NMO)GO TO 25
      IF(MO.NE.12)GO TO 21
C     PRINT WATER YEAR VALUES.
   25 IF (MSG.EQ.1) WYT=-999.0
      write(IPR,903) KWY,T,WYT
  903 FORMAT(1H ,6X,I4,12F8.2,F10.2)
      DO 26 I=1,12
      T(I)=-999.0
   26 CONTINUE
      IF(MSG.EQ.1)GO TO 27
      XWYSUM=XWYSUM+WYT
      CWY=CWY+1
   27 WYT=0.0
      MSG=0
      IF(M.EQ.NMO)GO TO 20
      KWY=KWY+1
      MO=0
   21 MO=MO+1
   20 CONTINUE
C     COMPUTE AND PRINT PERIOD TOTALS.
      WYTP = 0.0
      DO 30 MO=1,12
      T(MO)=-999.0
      IF(CM(MO).LT.0.5)GO TO 30
      T(MO)=SUM(MO)/CM(MO)
      IF(WYTP.GE.0.) THEN
         WYTP = WYTP + T(MO)
      ELSE
         WYTP = -999.0
      END IF
   30 CONTINUE
      WYT=-999.0
      IF(CWY.GT.0.5)WYT=XWYSUM/CWY
      write(IPR,904) T,WYTP
  904 FORMAT(1H0,7HSUMMARY,3X,12F8.2,F10.2)
      write(IPR,905)
  905 FORMAT (1H0,5X,45HMINUS SIGN INDICATES ESTIMATED MONTHLY VALUE.)
      RETURN
      END
