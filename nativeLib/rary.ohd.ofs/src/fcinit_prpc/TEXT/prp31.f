C     MODULE PRP31
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE PRINT PARAMETER ROUTINE FOR THE 'SNOW-43 '
C     OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for the 'SNOW-17' operation.  
C     Modified by...
C        Mark Woodbury - RTi, May 1995 for the 'SNOW-43' operation.
C.......................................
C    Argument       I/O         Description
C      PS            I          P, Parameter array
C.......................................
      SUBROUTINE PRP31(PS)
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PS(*)
      DIMENSION R(11),AE(2,14)
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real ps
C     --- L O C A L ---
      integer irnsnop, iscfltr
      integer idprint, ipprint
C
C----- C O M M O N  B L O C K S -------------------------
C           what's in the common:
C             COMMON/IONUM/IN,IPR,IPU
C             COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C             COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C             COMMON/FENGMT/METRIC
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fengmt'
C----- T Y P E statements for formating -----------------
      EXTERNAL      FMTDIG
      CHARACTER*1   FMTDIG
      CHARACTER*26  FORMAP
      CHARACTER*26  FORMAT
      CHARACTER*30  FORMT
C----- T Y P E statements units ------------------------
      CHARACTER*2  UNIT,CM,CFT


C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_prpc/RCS/prp31.f,v $
     . $',                                                             '
     .$Id: prp31.f,v 1.2 2000/12/19 15:01:09 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA R/0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0/

      DATA   FORMAP / '(20X,''MAP (Cv)     '',F8.0)' /
      DATA   FORMAT / '(20X,''MAT (VAR)    '',F8.0)' /
      DATA   FORMT  / '(33X,F8.0,F8.0,F8.0,F8.0,F8.0)' /

      DATA   CM,CFT / 'CM','FT' /
C.......................................
C     TRACE LEVEL=1,NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PRP31 ENTERED)
C.......................................
C     CONTROL VARIABLES
      IDT=PS(14)
      ITPX=PS(10)
      LPM=PS(25)
      LADC=PS(26)
      LRM=PS(17)
      LPCTS=PS(18)
      LOWE=PS(19)
      lowev=ps(31)
      LSWE=PS(20)
      lswev=ps(32)
      LOSC=PS(21)
      LCOVER=PS(22)
      LSUMS=PS(23)
      LTAPM=PS(27)
      LUPPM=PS(28)
      lkfpm=ps(33)
      LMFV=PS(29)
      LAEC=PS(30)
      IPRINT=PS(24)
C
C     Set up values for IDPRINT and IPPRINT
C
      IPPRINT = (IPRINT/10) * 10
      IDPRINT = IPRINT - IPPRINT
C.......................................
C     PRINT TITLE
      WRITE(IPR,901) (PS(I),I=2,6),PS(LPM+1),PS(LPM+13)
  901 FORMAT(1H0,10X,24HSNOW MODEL OPERATION FOR,1X,5A4,5X,5HELEV=,F5.0,
     11X,1HM,3X,5HLAT.=,F5.1)
      WRITE(IPR,902) IDT
  902 FORMAT(1H0,15X,30HCOMPUTATIONAL TIME INTERVAL IS,I3,1X,6HHOURS.)
C.......................................
C     PRINT TIME SERIES USED BY THE OPERATION.
      WRITE(IPR,903)
  903 FORMAT(1H0,20X,35HTIME SERIES USED BY THIS OPERATION.,//16X,8HCONT
     1ENTS,14X,4HI.D.,7X,4HTYPE,5X,13HTIME INTERVAL,5X,5HOTHER)
C
C     REQUIRED TIME SERIES.
      WRITE(IPR,904) (PS(I),I=7,9),ITPX,PS(LPM)
  904 FORMAT(1H0,10X,13HPRECIPITATION,12X,2A4,5X,A4,7X,I2,1X,5HHOURS,6X,
     16HPXADJ=,F5.2)
      TAELEV=PS(LPM+1)
      IF(LTAPM.GT.0) TAELEV=PS(LTAPM)
      WRITE(IPR,905) (PS(I),I=11,13),IDT,TAELEV
  905 FORMAT(1H ,10X,15HAIR TEMPERATURE,10X,2A4,5X,A4,7X,I2,1X,5HHOURS,
     16X,7HTAELEV=,F5.0)
C
C     OPTIONAL TIME SERIES.
      IF (LRM.GT.0) WRITE(IPR,906) PS(LRM),PS(LRM+1),PS(LRM+2),ITPX
  906 FORMAT(1H ,10X,9HRAIN+MELT,16X,2A4,5X,A4,7X,I2,1X,5HHOURS)
      IF(LPCTS.GT.0) WRITE(IPR,907) PS(LPCTS),PS(LPCTS+1),PS(LPCTS+2),
     1  ITPX
  907 FORMAT(1H ,10X,16HPERCENT SNOWFALL,9X,2A4,5X,A4,7X,I2,1X,5HHOURS)
      IF(LOWE.EQ.0) GO TO 101
      ITOWE=PS(LOWE+3)
      WRITE(IPR,908) PS(LOWE),PS(LOWE+1),PS(LOWE+2),ITOWE
  908 FORMAT(1H ,10X,13HOBSERVED W.E.,12X,2A4,5X,A4,7X,I2,1X,5HHOURS)
      if(lowev.eq.0) go to 101
      write(ipr,930) ps(lowev),ps(lowev+1),ps(lowev+2),itowe
  930 format(1h ,10x,21HVARIANCE OF OBS. W.E.,4x,2a4,5x,a4,7x,i2,1x,
     1 5hHOURS)
  101 IF(LSWE.EQ.0) GO TO 103
      IT=PS(LSWE+3)
      WRITE(IPR,909) PS(LSWE),PS(LSWE+1),PS(LSWE+2),IT
  909 FORMAT(1H ,10X,14HSIMULATED W.E.,11X,2A4,5X,A4,7X,I2,1X,5HHOURS)
      if(lswev.eq.0) go to 103
      write(ipr,931) ps(lswev),ps(lswev+1),ps(lswev+2),it
  931 format(1h ,10x,21HVARIANCE OF SIM. W.E.,4x,2a4,5x,a4,7x,i2,1x,
     1 5hHOURS)
  103 IF(LOSC.EQ.0) GO TO 104
      IT=PS(LOSC+3)
      WRITE(IPR,911) PS(LOSC),PS(LOSC+1),PS(LOSC+2),IT
  911 FORMAT(1H ,10X,16HOBS. AREAL COVER,9X,2A4,5X,A4,7X,I2,1X,5HHOURS)
  104 IF(LCOVER.EQ.0) GO TO 109
      IT=PS(LCOVER+3)
      WRITE(IPR,912) PS(LCOVER),PS(LCOVER+1),PS(LCOVER+2),IT
  912 FORMAT(1H ,10X,16HSIM. AREAL COVER,9X,2A4,5X,A4,7X,I2,1X,5HHOURS)
  109 IF (LAEC.EQ.0) GO TO 105
      LRS=LAEC+2
      WRITE(IPR,910) PS(LRS),PS(LRS+1),PS(LRS+2),IDT
  910 FORMAT(1H ,10X,19HRAIN-SNOW ELEVATION,6X,2A4,5X,A4,7X,I2,1X,
     1  5HHOURS)
C.......................................
C     TEMPERATURE LAPSE RATES
  105 IF(LTAPM.EQ.0) GO TO 106
      WRITE(IPR,913) PS(LTAPM+1),PS(LTAPM+2)
  913 FORMAT(1H0,10X,33HAIR TEMPERATURE LAPSE RATES--MAX=,F5.2,3X,4HMIN=
     1,F5.2,2X,10HDEGC/100 M)
C.......................................
C     WATER BALANCE SUMS
  106 IF(LSUMS.EQ.0) GO TO 107
      WRITE(IPR,914)
  914 FORMAT(1H0,10X,43HSUMS OF WATER BALANCE VARIABLES ARE STORED.)
C.......................................
C     DISPLAY OUTPUT
  107 IF(IDPRINT.EQ.0) GO TO 110
      IF(IDPRINT.EQ.2) GO TO 108
      WRITE(IPR,915)
  915 FORMAT(1H0,10X,41HSNOW COVER VARIABLES DISPLAYED EVERY DAY.)
      GO TO 110
  108 WRITE(IPR,916)
  916 FORMAT(1H0,10X,55HSNOW COVER VARIABLES DISPLAYED ON ALL SIGNIFICAN
     1T DAYS.)
C.......................................
C     PRINT PARAMETER VALUES
  110 WRITE(IPR,917)
  917 FORMAT(1H0,10X,16HPARAMETER VALUES,//16X,16HMAJOR PARAMETERS,4X,3H
     1SCF,2X,5HMFMAX,2X,5HMFMIN,3X,4HUADJ,5X,2HSI)
      WRITE(IPR,918) (PS(LPM+I),I=2,6)
  918 FORMAT(1H ,31X,3F7.2,F7.3,F7.0)
      IF(LMFV.EQ.0) GOTO 111
      WRITE(IPR,925) (I,I=1,12)
  925 FORMAT(1H0,15X,12HMF VARIATION,6X,12I5)
      WRITE(IPR,926) PS(LMFV),(PS(LMFV+I),I=1,11)
  926 FORMAT(1H ,33X,12F5.2)
  111 WRITE(IPR,919)
  919 FORMAT(1H0,15X,16HMINOR PARAMETERS,4X,3HNMF,3X,4HTIPM,2X,5HMBASE,1
     1X,6HPXTEMP,2X,5HPLWHC,2X,5HDAYGM)
      WRITE(IPR,920) (PS(LPM+I),I=7,12)
  920 FORMAT(1H ,31X,2F7.2,2F7.1,2F7.2)
      WRITE(IPR,921) R
  921 FORMAT(1H0,15X,15HDEPLETION CURVE,3X,5HWE/AI,2X,11F5.1)
      WRITE(IPR,922) PS(LADC),(PS(LADC+I),I=1,8)
  922 FORMAT(1H ,33X,5HCOVER,3X,4H0.05,9F5.2,2X,3H1.0)
      IF (LAEC.EQ.0) GO TO 115
      NPTAE=PS(LAEC)
      IAEU=PS(LAEC+1)
      IF (METRIC.EQ.0) GO TO 112
      IF ((METRIC.EQ.-1).AND.(IAEU.EQ.0)) GO TO 112
      UNIT=CM
      CONV=1.0
      GO TO 113
  112 UNIT=CFT
      CONV=1.0/0.3048
  113 WRITE(IPR,927) UNIT
  927 FORMAT(1H0,15X,'AREA-ELEVATION CURVE',5X,'ELEVATION UNITS=',A)
      DO 114 I=1,NPTAE
      J=LAEC+5+(I-1)*2
      AE(1,I)=PS(J)*CONV
      AE(2,I)=PS(J+1)
  114 CONTINUE
      WRITE(IPR,928) (AE(1,I),I=1,NPTAE)
  928 FORMAT(1H0,20X,5HELEV.,14F7.0)
      WRITE(IPR,929) (AE(2,I),I=1,NPTAE)
  929 FORMAT(1H ,20X,5HBELOW,14F7.2)
c
c.......................................
c     print parameters for Kalman Filtering
c
 115  if(lkfpm .eq. 0) go to 116

        FORMAP(25:25) = FMTDIG( ps(lkfpm), 8 )
        FORMAT(25:25) = FMTDIG( ps(lkfpm+3), 8 )

      write(ipr,932)
  932 format('0','          KALMAN FILTERING UPDATING PARAMETERS',/,
     $           '                INPUT ERROR COV. MATRIX',/)
      write(ipr,FORMAP) ps(lkfpm)
      write(ipr,FORMAT) ps(lkfpm+3)
C
      irnsnop = ps(lkfpm+44)
C
      if(laec .ne. 0. ) then
      if( irnsnop .eq. 0) then
          write(ipr, 766) ps(lkfpm+41), ps(lkfpm+42)
  766     format(1h0,15x,17hOPERATION ID. - ',2a4,55h' DOES NOT EXIST. U
     1SING DEFAULT VALUE OF 0.0055 DEGC/M.)        
      endif
      if( irnsnop .eq. 1) then
          write(ipr, 767) ps(lkfpm+43), ps(lkfpm+41), ps(lkfpm+42)
  767     format(1h0,15x,'USING LAPSE RATE OF ',f8.6,' DEGC/M - FROM RSN
     1WELEV OPERATION ',2a4)
      endif
      endif
C
      iscfltr = ps(lkfpm+45)
C
      if(iscfltr .eq. 1) then
          write (ipr, 769)
  769     format(1h0,15x,'SNOW COVER WILL BE UPDATED WITH THE VALUE COMP
     1UTED BY THE KALMAN FILTER WHEN WATER EQUIVALENT IS UPDATED.')
      endif 
C
C     PRINT P array printoptions
C
      if( ipprint .eq. 10. )then
          write(ipr, 770)
 770      format(1h0,15x,'P ARRAY DIAGONAL ELEMENTS WILL BE PRINTED.')
      else if( ipprint .eq. 20. ) then
          write(ipr, 780)
 780      format(1h0,15x,'ALL P ARRAY ELEMENTS WILL BE PRINTED.')
      else if( ipprint .eq. 30. ) then
          write(ipr, 790)
 790      format(1h0,15x,'ALL P ARRAY ELEMENTS WILL BE PRINTED ONCE A M
     1ONTH.')
      endif 


C
      write(ipr,933)
 933  format(16x,43hSYSTEM ERROR COVARIANCE MATRIX FOR STATES: ,/,
     1       16x,33hWE, NEGHS, LIQW, TINDEX AND AESC  )
      do 120 i=1,5

         indx = 4
         do 119 j = 1,5
            indx = indx+5
            FORMT(indx:indx) = FMTDIG( ps(lkfpm+3+i+(j-1)*5), 8 )
 119     continue

         write(ipr,FORMT) (ps(lkfpm+3+i+(j-1)*5), j=1,5)
 120  continue

      write(ipr,935)
      write(ipr,936) (ps(lkfpm+28+i), i=1,12)
 935  format(16x,39hDEFAULT MONTHLY OBSERVED W.E. VARIANCE,/33x,5x,
     1 3hJAN,5x,3hFEB,5x,3hMAR,5x,3hAPR,5x,3hMAY,5x,3hJUN,5x,3hJUL,
     2 5x,3hAUG,5x,3hSEP,5x,3hOCT,5x,3hNOV,5x,3hDEC)
 936  format(33x,12f8.0)
c
c.......................................
c     print other updating parameters
c
 116  IF(LUPPM.EQ.0) GO TO 199
      WRITE(IPR,923)
  923 FORMAT(1H0,15X,19HUPDATING PARAMETERS,5X,10HTOLERANCES,
     1/40X,4HW.E.,3X,5HCOVER)
      WRITE(IPR,924) PS(LUPPM),PS(LUPPM+1)
  924 FORMAT(1H ,39X,F4.2,4X,F4.2)
C     ALL PARAMETERS PRINTED
C.......................................
  199 CONTINUE
      IF(ITRACE.GE.1) WRITE(IODBUG,987)
  987 FORMAT(1H0,'** EXIT PRP19')
      RETURN
      END
