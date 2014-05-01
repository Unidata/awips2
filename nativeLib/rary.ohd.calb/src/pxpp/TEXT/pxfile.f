C$PRAGMA C (GET_APPS_DEFAULTS)
cfC$PRAGMA C (DIRNAME)
C MODULE PXFILE
C-----------------------------------------------------------------------
C
      SUBROUTINE PXFILE (NSTA,IMO,IYR,LMO,LYR,RUNITS,IOBT,PX,NBASE,E)
C
C  ROUTINE PXFILE STORES MONTHLY SUMS OF PRECIPITATION
C  DATA IN THE PX ARRAY FOR ALL STATIONS.  THIS ROUTINE
C  ALSO SETS THE ENTIRE MONTH TO MISSING IF ANY MISSING DATA
C  ARE ENCOUNTERED IN THAT MONTH.
C
C  READ DATA FROM EACH STATION AND STORE IN MONTHLY ARRAY.
C  EACH STATION MUST HAVE SOME DATA DURING THE PERIOD OF INTEREST.
C  BASE STATION IS READ FIRST.
C
      PARAMETER (NSX=500,MaxMonth=1200)
C      
      CHARACTER*4 RUNITS,UNITS
      character tsfilename(nsx)*132
      character*4 chtype
      real        type
      equivalence (type,chtype)
      integer*2 itype(nsx)
      DIMENSION SNAME(5),FILEN(3),P(744),NDAYS(12)
      DIMENSION PX(nsx,MaxMonth),E(20,nsx)
      DIMENSION STAID(3)
      INTEGER FIELD(7)
      character OFORMAT*12,tsfilenam2*268,dirname*132
C    
      INCLUDE 'common/ionum'
      COMMON/BHTIME/LHEAD,KMO,KDA,KYR,KHRMIN,KSEC
      common/pass/tsfilename,itype
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/pxpp/RCS/pxfile.f,v $
     . $',                                                             '
     .$Id: pxfile.f,v 1.7 2002/05/15 18:30:10 dws Exp $
     . $' /
C    ===================================================================
C
      DATA FIELD/1,4,5,6,7,10,15/
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      data OFORMAT/'(6f10.3)    '/
C
C
      call get_apps_defaults('calb_sta_ts_dir',15,dirname,lendr)
C
      INDERR=0      
C
C  READS DATA AND CALCULATES MONTHLY SUMS FOR EACH STATION
C
      DO 110 N=1,NSTA
      IMOS=0
      IYRS=0
      LMOS=0
      LYRS=0

      DO 101 NI=1,3
      LI=FIELD(1)+NI-1
      JI=FIELD(5)+NI-1
      STAID(NI)=E(JI,N)
  101 FILEN(NI)=E(LI,N)

      TYPE=E(FIELD(2),N)
      IT=E(5,N)
      LHEAD=E(6,N)
      KMO=E(10,N)
      KDA=E(11,N)
      KYR=E(12,N)
      KHRMIN=E(13,N)
      KSEC=E(14,N)
      DO 99 J=1,5
      JN=FIELD(7)+J-1
   99 SNAME(J)=E(JN,N)

CCC   WRITE(6,98)FILEN,TYPE,IT,LHEAD,STAID,KMO,KDA,KYR,KHRMIN,KSEC,SNAME
CCC 98 FORMAT(3A4,A4,I4,I6,1X,3A4,3(1X,I2),2(1X,I4),1X,5A4)
CCC   CALL LOCATX (IMOS,IYRS,LMOS,LYRS,RUNITS,FILEN,TYPE,IT,STAID,SNAME,
CCC  1  NEXTRD,IP)
      tsfilenam2=dirname(1:lendr)//'/pcpn/'//tsfilename(n)
      it=itype(n)
      chtype='PTPX'
CCC      write(6,*) 'chtype,type ',chtype,type
      call cardlo(imos,iyrs,lmos,lyrs,im1,iy1,im2,iy2,
     *            runits,units,tsfilenam2,type,it,staid,
     *            sname,iunit,oformat,xfmac,afac,iam1,iam2,
     *            ierror)
      IF (IERROR.NE.0) THEN
         INDERR=1
         GO TO 110
         ENDIF
C     IF STATION RECORD BEGINS PRIOR TO PERIOD OF INTEREST THEN ADVANCE
C     THE READ POSITION.
      IF ((IYRS*12+IMOS).GE.(IYR*12+IMO)) GO TO 111
      NADV=(IYR*12+IMO)-(IYRS*12+IMOS)
      MO=IMOS
      JYR=IYRS
      DO 112 I=1, NADV
CCC   CALL RDFILE(1,FILEN,1,NEXTRD,IP,1,MO,JYR,1,744,P)
      call cardrd(1,iunit,oformat,iam1,iam2,xfmac,afac,it,
     *            mo,jyr,p,1,ierror)
      if (ierror.ne.0) THEN
         INDERR=1
         GO TO 999
         ENDIF
      MO=MO+1
      IF (MO.LT.13) GO TO 112
      MO=1
      JYR=JYR+1
  112 CONTINUE
      IMOS=IMO
      IYRS=IYR
  111 IF ((LYRS*12+LMOS).LE.(LYR*12+LMO)) GO TO 113
      LYRS=LYR
      LMOS=LMO
  113 NMRD=(LYRS*12+LMOS)-(IYRS*12+IMOS)+1
      MO=IMOS
      JYR=IYRS
      MTDP=0
      TP=0.0
      MD=0
      MTD=0
C  PRECIPITATION DATA AND CALCULATE MONTHLY
C  MONTHLY SUMS FOR EACH STATION FOR THE PERIOD OF INTEREST
      DO 114 I=1,NMRD
      NN= (JYR*12+MO)-(IYR*12+IMO)+1
CCC   CALL RDFILE (1,FILEN,1,NEXTRD,IP,1,MO,JYR,1,744,P)
      call cardrd (1,iunit,oformat,iam1,iam2,xfmac,afac,it,
     *            mo,jyr,p,1,ierror)
      if (ierror.ne.0) THEN
         INDERR=1
         GO TO 999
         ENDIF
      NS=1
      IF ((I.GT.1).OR.(IOBT.EQ.24)) GO TO 115
C     FIRST MONTH OF DATA--OBS.TIME .NE. 24.
      IF (IT.EQ.1) GO TO 116
C     DAILY STATION
      NS=2
      IF ((P(1).LT.0.0).AND.(P(1).GT.-998.5)) MTDP=1
      GO TO 115
C     HOURLY STATION
  116 NS= IOBT+1
      IF ((P(IOBT).LT.0.0).AND.(P(IOBT).GT.-998.5)) MTDP=1
  115 ND= NDAYS(MO)
      IF (MO.NE.2) GO TO 117
      IF ((JYR-(JYR/4)*4).EQ.0) ND=ND+1
  117 NE=ND
      IF (IT.GT.1) GO TO 118
      NV= ND*24
      NE=(ND-1)*24+IOBT
  118 DO 120 II=NS,NE
      IF (P(II).LT.-998.5) GO TO 121
      IF (P(II).LT.-997.5) GO TO 122
C     VALID DATA
      IF (P(II).LT.0.0001) GO TO 120
      MTD=0
      TP=TP+P(II)
      GO TO 120
C     MISSING DATA
  121 MD=1
      GO TO 120
C     MISSING TIME DISTRIBUTION.
  122 MTD=1
  120 CONTINUE
C     STORE MONTHLY TOTAL IF VALID DATA.
      IF (MD.GT.0) GO TO 123
      IF (MTD.GT.0) GO TO 123
      IF (MTDP.GT.0) GO TO 123
      PX(N,NN)=TP
  123 MTDP=MTD
      MD=0
      MTD=0
      TP=0.0
      IF (IT.GT.1) GO TO 125
      IF (IOBT.EQ.24) GO TO 125
      NS=NE+1
      DO 124 II=NS,NV
      IF (P(II).LT.-998.5) GO TO 126
      IF (P(II).LT.-997.5) GO TO 127
      IF (P(II).LT.0.0001) GO TO 124
      MTD=0
      TP=TP+P(II)
      GO TO 124
  126 MD=1
      GO TO 124
  127 MTD=1
  124 CONTINUE
  125 MO=MO+1
      IF (MO.LT.13) GO TO 114
      MO=1
      JYR=JYR+1
  114 CONTINUE
C     END OF MONTH LOOP FOR EACH STATION.
      CALL CCLOSL
  110 CONTINUE
C
 999  IF (INDERR.EQ.1) CALL USTOP (IPR,0)
      RETURN
C
      END
