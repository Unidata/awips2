C     MODULE PUC31
C
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE PUNCHED CARD ROUTINE FOR THE 'SNOW-43 '
C        OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for the 'SNOW-17' operation.
c     Modified by...
C        Mark Woodbury - RTi, May 1995 for the 'SNOW-43' operation.
C.......................................
C    Argument       I/O         Description
C      PS            I          P, Parameter array
C      CS            I          Carry over array
C.......................................
      SUBROUTINE PUC31(PS,CS)
C
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real ps, cs
C     --- L O C A L ---
      real    ssckf, sckf
      integer idprint, ipprint
C     --- C O M M O N  B L O C K  V A R S ---
C           what's in the common:
C             COMMON/IONUM/IN,IPR,IPU
C             COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
C             COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
C             COMMON/FENGMT/METRIC
C             COMMON/PUDFLT/IPDFLT
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/pudflt'
C
      real    pprtopt, none, full, diag, mnth
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PS(*),CS(*)
      DIMENSION UPDATE(2),UPS(2),FID(6),UPPM(2),RSID(2),AE(2,14)
C
C----- T Y P E statements for formating -----------------
      EXTERNAL      FMTDIG
      CHARACTER*1   FMTDIG
      CHARACTER*30  FORMT1
      CHARACTER*26  FORMT2
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/puc31.f,v $
     . $',                                                             '
     .$Id: puc31.f,v 1.2 2000/12/19 15:02:02 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA ALL,FNONE,YES,SUMS,RDCO/4HALL ,4HNONE,3HYES,4HSUMS,4HRDCO/
      DATA BLANK,BLANK3,BLANK2/4H    ,3H   ,2H  /
      DATA UPDATE/4HUPDA,2HTE/
      DATA CMETR,CENGL/4HMETR,4HENGL/
      DATA AVSE/4HAVSE/
      data pcov,var/4hPROP,3hVAR/
      data ssckf/4hSCKF/
      data none/4hNONE/
      data full/4hFULL/
      data diag/4hDIAG/
      data mnth/4hMNTH/
C
      DATA   FORMT1 / '(F8.0,F8.0,1X,2A4,1X,A4,1X,A4)' /
      DATA   FORMT2 / '(F8.0,F8.0,F8.0,F8.0,F8.0)' /
C.......................................
C     TRACE LEVEL=1,NO DEBUG OUTPUT
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,16H** PUC31 ENTERED)
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
C     Set up IDPRINT and IPPRINT
C
      IPPRINT = (IPRINT/10) * 10
      IDPRINT = IPRINT - IPPRINT
C
C.......................................
C     PUNCH CARD NUMBER 1.
      PRC=BLANK
      IF(IPRINT.EQ.0) PRC=FNONE
      IF(IPRINT.EQ.1) PRC=ALL
      ODS=BLANK3
      IF((LOWE.GT.0).OR.(LOSC.GT.0)) ODS=YES
      OSS=BLANK3
      IF ((LSWE.GT.0).OR.(LCOVER.GT.0)) OSS=YES
      SS=BLANK
      IF(LSUMS.GT.0) SS=SUMS
      UPS(1)=BLANK
      UPS(2)=BLANK2
      IF(MAINUM.LT.3) GO TO 101
      IF(LUPPM.EQ.0) GO TO 101
      UPS(1)=UPDATE(1)
      UPS(2)=UPDATE(2)
  101 RDCOS=BLANK
      WE=CS(1)
      IF(WE.EQ.0.0) GO TO 107
      RDCOS=RDCO
  107 AVSES=BLANK
      IF (LAEC.EQ.0) GO TO 140
      AVSES=AVSE
 140  pcovs=blank
      if (lkfpm .eq. 0) go to 102
      pcovs = pcov
  102 WRITE(IPU,901) (PS(I),I=2,6),PS(LPM+1),PS(LPM+13),PRC,ODS,OSS,SS,
     1UPS,RDCOS,AVSES,pcovs
  901 FORMAT(5A4,F5.0,F5.1,1X,A4,2X,A3,2X,A3,1X,A4,4X,A4,A2,1X,A4,1X,A4,
     11x,a4)
C.......................................
C     PUNCH CARD NUMBER 2.
      DO 103 I=1,6
  103 FID(I)=BLANK
      IF (LRM.EQ.0) GO TO 104
      FID(1)=PS(LRM)
      FID(2)=PS(LRM+1)
      FID(3)=PS(LRM+2)
  104 IF(LPCTS.EQ.0) GO TO 105
      FID(4)=PS(LPCTS)
      FID(5)=PS(LPCTS+1)
      FID(6)=PS(LPCTS+2)
  105 WRITE(IPU,902) ITPX,(PS(I),I=7,9),PS(LPM),(FID(I),I=1,6)
  902 FORMAT(3X,I2,2X,2A4,1X,A4,F10.3,7X,2A4,1X,A4,7X,2A4,1X,A4)
C.......................................
C     PUNCH CARD NUMBER 3.
      IF(LTAPM.GT.0) GO TO 106
      WRITE(IPU,903)(PS(I),I=11,13),IDT
  903 FORMAT(2X,2A4,1X,A4,3X,I2,F10.0,2F5.2)
      GO TO 130
  106 WRITE(IPU,903) (PS(I),I=11,13),IDT,PS(LTAPM),PS(LTAPM+1),
     1PS(LTAPM+2)
  130 IF (LAEC.EQ.0) GO TO 110
C.......................................
C     PUNCH CARD NUMBER 3A
      IAEU=PS(LAEC+1)
      IF (METRIC.EQ.0) GO TO 131
      IF ((METRIC.EQ.-1).AND.(IAEU.EQ.0)) GO TO 131
      UNIT=CMETR
      CONV=1.0
      GO TO 132
  131 UNIT=CENGL
      CONV=1.0/0.3048
  132 NPTAE=PS(LAEC)
      DO 133 I=1,NPTAE
      L=LAEC+5+(I-1)*2
      AE(1,I)=PS(L)*CONV
      AE(2,I)=PS(L+1)
  133 CONTINUE
      EMIN=AE(1,1)
      EMAX=AE(1,NPTAE)
      NPTAE=NPTAE-2
      LRS=LAEC+2
      RSID(1)=PS(LRS)
      RSID(2)=PS(LRS+1)
      RSTYPE=PS(LRS+2)
      WRITE(IPU,911) NPTAE,EMIN,EMAX,UNIT,RSID,RSTYPE
  911 FORMAT(3X,I2,2F10.0,1X,A4,2X,2A4,1X,A4)
      IF (NPTAE.EQ.0) GO TO 110
C     PUNCH CARD NUMBER 3B
      WRITE(IPU,912) (AE(1,I+1),AE(2,I+1),I=1,NPTAE)
  912 FORMAT(4(F10.0,2X,F3.2))
  110 IF(ODS.EQ.BLANK3) GO TO 115
C.......................................
C     PUNCH CARD NUMBER 4.
      DO 111 I=1,8
  111 FID(I)=BLANK
      IT1=0
      IT2=0
      IF(LOWE.EQ.0) GO TO 112
      FID(1)=PS(LOWE)
      FID(2)=PS(LOWE+1)
      FID(3)=PS(LOWE+2)
      IT1=PS(LOWE+3)
      vars=blank3
      if (lowev .eq. 0) go to 112
      vars = var
  112 IF(LOSC.EQ.0) GO TO 113
      FID(4)=PS(LOSC)
      FID(5)=PS(LOSC+1)
      FID(6)=PS(LOSC+2)
      IT2=PS(LOSC+3)
  113 WRITE(IPU,904) (FID(I),I=1,3),IT1,vars,(FID(I),I=4,6),IT2
  904 FORMAT(2X,2A4,1X,A4,3X,I2,2x,a3,7x,2A4,1X,A4,3X,I2)
      if (lowev .eq. 0) goto 115
c.......................................
c     punch card number 4a
      fid(1) = ps(lowev)
      fid(2) = ps(lowev+1)
      fid(3) = ps(lowev+2)
      write (ipu,904) (fid(i), i = 1,3)
 915  format (2x,2a4,1x,a4)
 115  IF(OSS.EQ.BLANK3) GO TO 120
c.......................................
C     PUNCH CARD NUMBER 5.
      DO 116 I=1,6
  116 FID(I)=BLANK
      IT1=0
      IT2=0
      IF(LSWE.EQ.0) GO TO 117
      FID(1)=PS(LSWE)
      FID(2)=PS(LSWE+1)
      FID(3)=PS(LSWE+2)
      IT1=PS(LSWE+3)
      vars=blank3
      if (lswev .eq. 0) go to 117
      vars = var
  117 IF(LCOVER.EQ.0) GO TO 118
      FID(4)=PS(LCOVER)
      FID(5)=PS(LCOVER+1)
      FID(6)=PS(LCOVER+2)
      IT2=PS(LCOVER+3)
  118 WRITE(IPU,904) (FID(I),I=1,3),IT1,vars,(FID(I),I=4,6),IT2
      if (lswev .eq. 0) goto 120
c.......................................
c     punch card number 5a
      fid(1) = ps(lswev)
      fid(2) = ps(lswev+1)
      fid(3) = ps(lswev+2)
      write (ipu,904) (fid(i), i = 1,3)
C.......................................
C     PUNCH CARD NUMBER 6.
  120 MV=0
      IF (LMFV.GT.0) MV=1
      WRITE(IPU,905) (PS(LPM+I),I=2,6),MV
  905 FORMAT(3F5.2,F5.3,F5.0,I5)
C     PUNCH CARD 6A IF NEEDED
      IF (LMFV.EQ.0) GO TO 125
      WRITE(IPU,910) PS(LMFV),(PS(LMFV+I),I=1,11)
  910 FORMAT(12F5.2)
C.......................................
C     PUNCH CARD NUMBER 7.
  125 DO 121 I=1,2
  121 UPPM(I)=0.0
      IF(LUPPM.EQ.0) GO TO 122
      UPPM(1)=PS(LUPPM)
      UPPM(2)=PS(LUPPM+1)
  122 WRITE(IPU,906) (PS(LPM+I),I=7,12),UPPM
  906 FORMAT(2F5.2,2F5.1,4F5.2)
C.......................................
C     PUNCH CARD NUMBER 8.
      WRITE(IPU,907) PS(LADC),(PS(LADC+I),I=1,8)
  907 FORMAT(9F5.2)
      if (lkfpm .eq. 0) go to 150
c.......................................
c     punch card number 9
        FORMT1(5:5) = FMTDIG( ps(lkfpm), 8 )
        FORMT1(10:10) = FMTDIG( ps(lkfpm+3), 8 )
      pprtopt = none
          if( ipprint .eq. 10.) then
          pprtopt = diag
      else if( ipprint .eq. 20.) then
          pprtopt = full
      else if( ipprint .eq. 30.) then
          pprtopt = mnth
      endif 
      sckf = blank
      if( ps(lkfpm + 45) .eq. 1) sckf = ssckf
      write (ipu,FORMT1) ps(lkfpm),ps(lkfpm+3),ps(lkfpm+41),
     1                   ps(lkfpm+42),sckf, pprtopt
c.......................................
c     punch card number 10
      do 145 i = 1,5

         indx = 0
         do 144 j = 1,i
            indx = indx+5
            FORMT2(indx:indx) = FMTDIG( ps(lkfpm+3+i+(j-1)*5), 8 )
 144     continue

         write (ipu,FORMT2) (ps(lkfpm+3+i+(j-1)*5), j = 1,i)
 145  continue

c.......................................
c     punch card number 11
      write (ipu,920) (ps(lkfpm+28+i), i = 1,6)
      write (ipu,920) (ps(lkfpm+28+i), i = 7,12)
 920  format (8f8.0)
 150  IF(RDCOS.EQ.BLANK) RETURN
C.......................................
C     PUNCH CARD NUMBER 12.
      ICO=1
      IF(IPDFLT.EQ.1) ICO=0
      IWE=CS(1)+0.5
      IAX=CS(5)+0.5
      NEXLAG=5/ITPX+2
      L=10+NEXLAG
      WRITE(IPU,908) IWE,(CS(I),I=2,4),IAX,ICO,cs(l+1)
  908 FORMAT(I5,F5.1,F5.0,F5.1,I5,4X,I1,f5.2)
      IF (ICO.EQ.0) RETURN
C.......................................
C     PUNCH CARD NUMBER 13.
      ISB=CS(6)+0.5
      ISBWS=CS(8)+0.5
      IAEA=CS(10)+0.5
      WRITE(IPU,909) ISB,CS(7),ISBWS,CS(9),IAEA,(CS(I),I=11,L)
  909 FORMAT(I5,F5.2,I5,F5.1,I5,7F5.1)
c.......................................
c     punch card number 14
      if (lkfpm .eq. 0) go to 160
      do 155 i = 1,5

         indx = 0
         do 154 j = 1,i
            indx = indx+5
            FORMT2(indx:indx) = FMTDIG( cs(l+1+i+(j-1)*5), 8 )
 154     continue

         write (ipu,FORMT2) (cs(l+1+i+(j-1)*5), j = 1,i)
 155  continue

C.......................................
 160  continue
      IF(ITRACE.GE.1) WRITE(IODBUG,987)
  987 FORMAT(1H0,13H** EXIT PUC31)
      RETURN
      END
