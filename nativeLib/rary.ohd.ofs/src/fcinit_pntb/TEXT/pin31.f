C     MODULE PIN31
C
      SUBROUTINE PIN31(pall, mp, PS,LEFTP,IUSEP,CS,LEFTC,IUSEC)
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS IS THE INPUT ROUTINE FOR THE 'SNOW-43 ' OPERATION.
C     TEMPERATURE INDEX SNOW MODEL FIRST DESCRIBED IN NOAA TECH MEMO
C     NWS-HYDRO-17.  Kalman filtering updating technique described in
C     nws technical memo 43.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 for the SNOW-17 operation
C     Modified by...
C        Mark S. Woodbury - RTi, May 1995 for use in SNOW-43 
C        Nalneesh Gaur - RTi, Aug 1995 
C.......................................
C    Arguments:
C
C    Argument       I/O         Description
C      pall          I          Entire P array
C      mp            I          Size of P array
C      PS            O          P, Parameter array
C      LEFTP         I          Integer variable indicating 
C                               space remaining in P array
C      IUSEP         O          Integer variable indicating
C                               space used in P array
C      CS            O          Carry over array
C      LEFTC         I          Integer variable indicating
C                               space remaining in C array
C      IUSEC         O          Integer variable indicating
C                               space used in C array
C.......................................
C
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      real     pall, ps, cs
      integer  mp, leftp, iusep, leftc, isec
C
C     --- L O C A L ---
      integer rsnloc, irnsnop
      integer iscfltr
      real    rsopid, sckf, ssckf
      real    mfmax, mfmin, nmf, mbase
      real    rlapse
      real    pprtopt
      real    none, full, mnth, diag
C
C----- D I M E N S I O N --------------------------------
      DIMENSION PS(*),CS(*),pall(*)
      DIMENSION SNAME(2),ANAME(5),UPS(2),UPDATE(2),PXID(2),RMID(2)
      DIMENSION TAID(2),OWEID(2),OSCID(2),SWEID(2),SSCID(2),RSID(2)
      DIMENSION PSID(2)
      DIMENSION ADC(11),SMFV(12),AE(2,14)
      dimension owevid(2),swevid(2),rmonth(12)
      dimension rnsnwop(2),rsop(2)
C
C----- C O M M O N  B L O C K S -------------------------
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FPROG/MAINUM,VERS,VDATE(2),PNAME(5),NDD
      include 'snow43/snco31'
      include 'snow43/cupdt31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_pntb/RCS/pin31.f,v $
     . $',                                                             '
     .$Id: pin31.f,v 1.2 2006/03/23 13:38:17 xfan Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T -----------------------
      DATA SNAME/4HPIN3,4H1   /
      DATA ALL,FNONE,YES,SUMS,UPDATE,RDCO/4HALL ,4HNONE,3HYES,4HSUMS,
     14HUPDA,2HTE,4HRDCO/
      DATA BLANK/4H    /
      data ssckf/4hSCKF/
      data none/4hNONE/
      data full/4hFULL/
      data diag/4hDIAG/
      data mnth/4hMNTH/
C
      DATA DL2,DL,DLES,TEMP/4HL2  ,4HL   ,4HDLES,4HTEMP/
      DATA AVSE,CMETR/4HAVSE,4HMETR/
      data pcov,var/4hPROP,3hVAR/
      data rsop/4hRSNW,4hELEV/
C.......................................
C     TRACE LEVEL=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,31,IBUG)
      IF(ITRACE.GE.1) WRITE(IODBUG,899)
  899 FORMAT(1H0,16H** PIN31 ENTERED)
C
C.......................................
C     INITIALIZE CONTROL VARIABLES
      IUSEP=0
      IUSEC=0
      NOFILL=0
      IVER=1
      LRM=0
      LPCTS=0
      LOWE=0
      lowev=0
      lswev=0
      LSWE=0
      LOSC=0
      LCOVER=0
      LSUMS=0
      LPM=38
      LADC=LPM+14
      LTAPM=0
      LAEC=0
      LUPPM=0
      lkfpm=0
      LMFV=0
      NXPS=LADC+9
      rsnloc  = 0
      irnsnop = 2
      iscfltr = 0
      sckf       = blank
      rnsnwop(1) = blank
      rnsnwop(2) = blank
C.......................................
C     READ INPUT CARDS AND MAKE CHECKS.
C
C     INPUT CARD NUMBER 1
      READ(IN,900) ANAME,ELEV,ALAT,PRC,ODS,OSS,SS,UPS,RDCOS,AEC,pcovs
  900 FORMAT(5A4,2F5.0,1X,A4,2X,A3,2X,A3,1X,A4,4X,A4,A2,1X,A4,1X,A4,
     11x,a4)
      IPRINT=2
      IF(PRC.EQ.FNONE)IPRINT=0
      IF(PRC.EQ.ALL)IPRINT=1
      IOBS=0
      IF(ODS.EQ.YES)IOBS=1
      ISIM=0
      IF(OSS.EQ.YES)ISIM=1
      IF(SS.NE.SUMS) GO TO 101
      LSUMS=NXPS
      NXPS=NXPS+7
  101 IUP=0
      IF((UPS(1).EQ.UPDATE(1)).AND.(UPS(2).EQ.UPDATE(2))) IUP=1
      IRDCO=0
      IF(RDCOS.EQ.RDCO) IRDCO=1
      IAEC=0
      IF (AEC.EQ.AVSE) IAEC=1
      iprop=0
      if(pcovs.eq.PCOV) iprop=1
C.......................................
C     INPUT CARD NUMBER 2
      READ (IN,901) ITPX,PXID,PXTYPE,PXADJ,RMID,RMTYPE,PSID,PSTYPE
  901 FORMAT(3X,I2,2X,2A4,1X,A4,F10.0,7X,2A4,1X,A4,7X,2A4,1X,A4)
      IF(ITPX.LE.0) ITPX=6
      CALL CHEKTS(PXID,PXTYPE,ITPX,1,DL,0,1,IERR)
      IF((RMID(1).EQ.BLANK).AND.(RMID(2).EQ.BLANK)) GO TO 102
      LRM=NXPS
      NXPS=NXPS+3
      CALL CHEKTS(RMID,RMTYPE,ITPX,1,DL,0,1,IERR)
  102 IF((PSID(1).EQ.BLANK).AND.(PSID(2).EQ.BLANK)) GO TO 105
      LPCTS=NXPS
      NXPS=NXPS+3
      CALL CHEKTS(PSID,PSTYPE,ITPX,1,DLES,1,1,IERR)
C.......................................
C     INPUT CARD NUMBER 3
  105 READ(IN,902) TAID,TATYPE,IDT,TAELEV,TALMAX,TALMIN
  902 FORMAT(2X,2A4,1X,A4,3X,I2,F10.0,2F5.0)
      IF(IDT.LE.0) IDT=6
      CALL CHEKTS(TAID,TATYPE,IDT,1,TEMP,0,1,IERR)
      IF((IDT/ITPX)*ITPX.EQ.IDT) GO TO 106
      WRITE(IPR,903)IDT,ITPX
  903 FORMAT(1H0,10X,25H**ERROR** TIME INTERVAL (,I2,1X,
     148HHOURS) OF THE TEMPERATURE DATA IS NOT A MULTIPLE,/16X,
     222HOF THE TIME INTERVAL (,I2,1X,28HHOURS) OF THE PRECIPITATION.)
      CALL ERROR
  106 IF(TAELEV.EQ.0.0) GO TO 160
      IF(TAELEV.EQ.ELEV) GO TO 160
      LTAPM=NXPS
      NXPS=NXPS+3
      IF((TALMAX.GE.0.0).AND.(TALMIN.GE.0.0)) GO TO 160
      WRITE(IPR,904)
  904 FORMAT(1H0,10X,93H**WARNING** AT LEAST ONE OF THE LAPSE RATES ARE
     1NOT POSITIVE.  CHECK THAT VALUES ARE CORRECT.)
      CALL WARN
  160 IF (IAEC.EQ.0) GO TO 110
C.......................................
C     INPUT CARD NUMBER 3A
      READ(IN,913) NPTAE,EMIN,EMAX,AEUNT,RSID,RSTYPE
  913 FORMAT(3X,I2,2F10.0,1X,A4,2X,2A4,1X,A4)
      IF (NPTAE.LE.12) GO TO 161
      WRITE(IPR,914) NPTAE
  914 FORMAT(1H0,10X,'**ERROR** THE NUMBER OF AREA-ELEVATION CURVE POINT
     1S INPUT(',I2,') EXCEEDS 12.')
      CALL ERROR
      NPTAE=12
  161 IAEU=0
      IF (AEUNT.EQ.CMETR) IAEU=1
      LAEC=NXPS
      NXPS=NXPS+5+2*(NPTAE+2)
      CALL CHEKTS(RSID,RSTYPE,IDT,1,DL,0,1,IERR)
      IF (NPTAE.EQ.0) GO TO 165
C     INPUT CARD NUMBER 3B
      READ(IN,915) (AE(1,I+1),AE(2,I+1),I=1,NPTAE)
  915 FORMAT(4(F10.0,2X,F3.2))
  165 AE(1,1)=EMIN
      AE(2,1)=0.0
      NPTAE=NPTAE+2
      AE(1,NPTAE)=EMAX
      AE(2,NPTAE)=1.0
      IER=0
      DO 167 I=2,NPTAE
      IF (AE(1,I).GE.AE(1,I-1)) GO TO 166
      IER=1
  166 IF (AE(2,I).GE.AE(2,I-1)) GO TO 167
      IER=1
  167 CONTINUE
      IF (IER.EQ.0) GO TO 110
      WRITE(IPR,916)
  916 FORMAT(1HO,10X,'**ERROR** ONE OR MORE VALUES IN THE AREA-ELEVATION
     1 CURVE ARE LESS THAN THE PRECEEDING VALUE.')
      CALL ERROR
  110 IF(IOBS.EQ.0) GO TO 120
C.......................................
C     INPUT CARD NUMBER 4
      READ(IN,905) OWEID,OWETYP,ITOWE,owevars,OSCID,OSCTYP,ITOSC
  905 FORMAT(2X,2A4,1X,A4,3X,I2,2x,a3,7x,2A4,1X,A4,3X,I2)
      iowevar=0
      IF((OWEID(1).EQ.BLANK).AND.(OWEID(2).EQ.BLANK)) GO TO 111
      if(owevars.eq.var) iowevar=1
C
      LOWE=NXPS
      NXPS=NXPS+4
      CALL CHEKTS(OWEID,OWETYP,ITOWE,1,DL,1,1,IERR)
      IF((ITOWE/IDT)*IDT.EQ.ITOWE) GO TO 111
      WRITE(IPR,906)OWEID,OWETYP,ITOWE,IDT
  906 FORMAT(1H0,10X,49H**ERROR** THE TIME INTERVAL OF TIME SERIES (I.D.
     1=,2A4,2X,5HTYPE=,A4,2X,I2,1X,6HHOURS),/16X,40HIS NOT A MULTIPLE OF
     2 THE TIME INTERVAL (,I2,1X,31HHOURS) OF THE TEMPERATURE DATA.)
      CALL ERROR
  111 IF((OSCID(1).EQ.BLANK).AND.(OSCID(2).EQ.BLANK)) GO TO 115
      LOSC=NXPS
      NXPS=NXPS+4
      CALL CHEKTS(OSCID,OSCTYP,ITOSC,1,DLES,1,1,IERR)
      IF((ITOSC/IDT)*IDT.EQ.ITOSC) GO TO 115
      WRITE(IPR,906)OSCID,OSCTYP,ITOSC,IDT
      CALL ERROR
  115 if(iowevar.eq.0) go to 120
C.......................................
C     input card number 4a
      read(in,917) owevid,owevtyp
  917 format(2x,2a4,1x,a4)
      if((owevid(1).eq.blank).and.(owevid(2).eq.blank)) go to 120
      lowev=nxps
      nxps=nxps+4
C
      if( (iowevar.eq.1) .AND. (iprop.eq.0)) then
           write(IPR, 986) owevid, owevtyp
  986      FORMAT(1H0,"**WARNING** VAR flag is set in CARD 4 but the PRO 
     &P flag is not set in CARD 1.", / , "No values will be read from",
     &" the time series",1x, 2a4, 1x, a4, 1x, "." )
           CALL WARN
      endif
C
      call chekts(owevid,owevtyp,itowe,1,dl2,1,1,ierr)
  120 IF(ISIM.EQ.0) GO TO 125
C.......................................
C     INPUT CARD NUMBER 5
      READ(IN,905)SWEID,SWETYP,ITSWE,swevars,SSCID,SSCTYP,ITSSC
      IF((SWEID(1).EQ.BLANK).AND.(SWEID(2).EQ.BLANK)) GO TO 121
      iswevar=0
      if( swevars.eq.var ) iswevar=1
C
      LSWE=NXPS
      NXPS=NXPS+4
      CALL CHEKTS(SWEID,SWETYP,ITSWE,1,DL,0,1,IERR)
      IF((ITSWE/IDT)*IDT.EQ.ITSWE) GO TO 121
      WRITE(IPR,906) SWEID,SWETYP,ITSWE,IDT
      CALL ERROR
  121 IF((SSCID(1).EQ.BLANK).AND.(SSCID(2).EQ.BLANK)) GO TO 123
      LCOVER=NXPS
      NXPS=NXPS+4
      CALL CHEKTS(SSCID,SSCTYP,ITSSC,1,DLES,0,1,IERR)
      IF((ITSSC/IDT)*IDT.EQ.ITSSC) GO TO 123
      WRITE(IPR,906) SSCID,SSCTYP,ITSSC,IDT 
      CALL ERROR
  123 if(iswevar.eq.0) go to 125
C.......................................
C     input card number 5a
      read(in,917) swevid,swevtyp
      if((swevid(1).eq.blank).and.(swevid(2).eq.blank)) go to 125
      lswev=nxps
      nxps=nxps+4
C
      if( (iswevar.eq.1) .AND. (iprop.eq.0)) then
           write(IPR, 985) swevid,swevtyp
  985      FORMAT(1H0,"**WARNING** VAR flag is set in CARD 5 but the PRO
     &P flag is not set in CARD 1.", / ,"Missing values will be written"
     &,"to the time series",1x,2a4,1x,a4,1x, ".")
           CALL WARN
      endif
C
      call chekts(swevid,swevtyp,itswe,1,dl2,1,1,ierr)
C.......................................
C
C     SET-UP SPACE FOR UPDATING PARAMETERS.
  125 IF(MAINUM.LT.3) GO TO 126
      IF(IUP.EQ.0) GO TO 129
      IF((LOWE.EQ.0).AND.(LOSC.EQ.0)) GO TO 129
      NUP=2
      GO TO 127
  126 NUP=7
  127 LUPPM=NXPS
      NXPS=NXPS+NUP
c.......................................
c     set-up space for Kalman filter parameters
C     12 monthly variance + 
C      4 spaces for rnsnow operation +
C      1 space for iscfltr = 17
c
  129 if(iprop .eq. 0) go to 130
      lkfpm=nxps
      nxps=nxps + llfltr*llfltr + nnfltr*nnfltr + 17
C.......................................
C     INPUT CARD NUMBER 6.
  130 READ(IN,907) SCF,MFMAX,MFMIN,UADJ,SI,MV
  907 FORMAT(5F5.0,4X,I1,f5.0)
C     INPUT CARD NUMBER 6A IF NEEDED
      IF(MV.EQ.0) GOTO 133
      READ(IN,908) SMFV
      LMFV=NXPS
      NXPS=NXPS+12
      DO 134 I=1,12
CFAN
CFAN  HSD bug r26-25
CFAN
CFAN      IF (SMFV(I).LT.0.0) SMFV(I)=0.0
CFAN      IF (SMFV(I).GT.1.0) SMFV(I)=1.0
      IF (SMFV(I).LT.0.0) THEN
        SMFV(I)=0.0
        WRITE (IPR, 888) I
 888    FORMAT(1H0,"**WARNING** SET SMFV(",I2,")=0.0 SINCE IT < 0.0 .")
      ENDIF
      IF (SMFV(I).GT.1.0) THEN
        SMFV(I)=1.0
        WRITE (IPR, 889) I
 889    FORMAT(1H0,"**WARNING** SET SMFV(",I2,")=1.0 SINCE IT > 1.0 .")
      ENDIF
CFAN
  134 CONTINUE
C.......................................
C     INPUT CARD NUMBER 7.
  133 READ(IN,908) NMF,TIPM,MBASE,PXTEMP,PLWHC,DAYGM,WETOL,SCTOL
  908 FORMAT(12F5.0)
CFAN
CFAN  HSD bug r26-25
CFAN
CFAN      IF(TIPM.LT.0.1) TIPM=0.1
CFAN      IF(TIPM.GT.1.0) TIPM=1.0
      IF(TIPM.LT.0.01) THEN
        TIPM=0.01
        WRITE (IPR, 890) TIPM
 890    format(1H0,"**WARNING** SET TIMP=", F5.2, " SINCE TIMP<0.01 .")
      ENDIF  
      IF(TIPM.GT.1.0) THEN
        TIPM=1.0
        WRITE (IPR, 891) TIPM
 891    format(1H0,"**WARNING** SET TIMP=", F5.2, " SINCE TIMP>1.0 .")
      ENDIF  
CFAN
      IF(PLWHC.GT.1.0) PLWHC=PLWHC*0.01
C.......................................
C     INPUT CARD NUMBER 8.
      READ(IN,908) (ADC(I),I=2,10)
      ADC(1)=0.05
      ADC(11)=1.0
      IER=0
      DO 131 I=2,10
      IF (ADC(I).GE.ADC(I-1)) GO TO 131
      IER=1
  131 CONTINUE
      IF (IER.EQ.0) GO TO 136
      WRITE (IPR,912)
  912 FORMAT (1H0,10X,93H**ERROR** ONE OR MORE VALUES OF THE AREAL DEPLE
     1TION CURVE ARE LESS THAN THE PRECEEDING VALUE.)
      CALL ERROR
c.......................................
c     input card number 9 (MAP coefficient of variation, MAT variance,
c     rain-snow op name, snow cover filter flag, P matrix print opt)
  136 if(iprop .eq. 0) go to 132
      read(in,918) cvu(1,1), cvu(2,2), rnsnwop, sckf, pprtopt
  918 format (2f8.0,1x,2a4,1x,a4,1x,a4)
  919 format (8f8.0)
      cvu(1,2)=0.
      cvu(2,1)=0.
c---------------------------------------
c     find the location of rain snow operation
c     irnsnop=0  The operation ID is not found
c     irnsnop=1  The operation ID is found
c     irnsnop=2  The operation ID not provided
c---------------------------------------
C
      rlapse = 0.
      if ( laec .gt. 0. ) then
        rlapse = 0.0055
        if( (rnsnwop(1) .le. blank) .and. (rnsnwop(2) .le. blank) ) then
            irnsnop=2
        else
            call fopcde(rsop, rsopid)
            call fserch(rsopid, rnsnwop, rsnloc, pall, mp)
            if(rsnloc .eq. 0) then
                write(ipr, 767) rnsnwop(1), rnsnwop(2)
  767           format(1H0,10x,'**WARNING** UNABLE TO LOCATE OPERATION IDE
     1NTIFIER -', 2a4,'. USING DEFAULT LAPSE RATE OF 0.0055 DEGC/M')
                call warn
                irnsnop=0
            else
C    7th location is the lapse rate location
                irnsnop=1
                rlapse = pall(rsnloc + 6)/100.0
            endif
        endif 
      endif
C
      if( sckf .ne. ssckf .or. sckf .le. blank ) then 
         iscfltr = 0
      else
         iscfltr = 1
      endif
C
C     pprtopt - P array print options
C
      if(pprtopt .eq. none) then
          iprint = 0
      else if(pprtopt .eq. diag) then
          iprint = iprint + 10
      else if(pprtopt .eq. full) then
          iprint = iprint + 20
      else if(pprtopt .eq. mnth) then
          iprint = iprint + 30
      endif
c.......................................
c     input card number 10 (system error covariance matrix)
      do 137 i=1, nnfltr
  137    read(in,919)(q(i,j),j=1,i)
      call fmxsfil(q,nnfltr)
c.......................................
c     input card number 11 (Monthly default variance for observed we)
      read(in,919)(rmonth(i),i=1,6)
      read(in,919)(rmonth(i),i=7,12)
      do 180 i = 1, 12
 180     if (rmonth(i) .le. 0.) rmonth(i) = -99.
c
c iprop = 1   PROP is set.
c IOBS = 1    Observed snow water equiv time series is provided
c iowevar = 0 There is no variance time series.
c
      if((mainum .ge. 3) .and. (iup .eq. 0)) go to 132
      if((IOBS .eq. 1).AND.(iowevar .eq. 0)) then
      do 897 i = 1, 12
         if( rmonth(i) .le. 0.) then
             write (ipr, 898) i
 898         format(1H0,"**WARNING** UPDATING CANNOT BE PERFORMED FOR
     1MONTH - ", I2, "USING OBSERVED TIME SERIES.")
         endif
 897  continue
      endif
c
C.......................................
C     CARRYOVER SECTION
  132 NEXLAG=5/ITPX+2
      IF(IRDCO.EQ.0) GO TO 135
C.......................................
C     INPUT CARD NUMBER 12.
      READ(IN,907) WE,NEGHS,LIQW,TINDEX,ACCMAX,ICO,aesc
      IF(ICO.EQ.0) GO TO 135
C.......................................
C     INPUT CARD NUMBER 13.
      READ(IN,908)SB,SBAESC,SBWS,STORGE,AEADJ,(EXLAG(I),I=1,NEXLAG)
      IRDCO=2
      if(iprop .eq. 0) go to 135
c.......................................
c     input card number 14. (carryover for Kalman filter)
      do 185 i=1,nnfltr
  185    read(in,919)(p(i,j),j=1,i)
      call fmxsfil(p,nnfltr)
C
C     CHECK CARRYOVER VALUES.
  135 CALL CKCO31(IRDCO,PLWHC,SI,ADC,ITPX, iprop)
C.......................................
C     STORAGE REQUIREMENTS KNOWN CHECK SPACE IN P AND C ARRAYS
      NPS=NXPS-1
      NCS=10+NEXLAG+1+25*iprop
      CALL CHECKP(NPS,LEFTP,IERR)
      IF(IERR.EQ.1) NOFILL=1
      CALL CHECKC(NCS,LEFTC,IERR)
      IF(IERR.EQ.1) NOFILL=1
      IF(NOFILL.EQ.1) RETURN
C.......................................
C     SPACE AVAILABLE - STORE ALL VALUES.
C
C     STORE PARAMETER VALUES IN PS().
      PS(1)=IVER+0.01
      DO 140 I=1,5
  140 PS(I+1)=ANAME(I)
      PS(7)=PXID(1)
      PS(8)=PXID(2)
      PS(9)=PXTYPE
      PS(10)=ITPX+0.01
      PS(11)=TAID(1)
      PS(12)=TAID(2)
      PS(13)=TATYPE
      PS(14)=IDT+0.01
      PS(15)=IRDCO+0.01
      PS(16)=NPS+0.01
      PS(17)=LRM+0.01
      PS(18)=LPCTS+0.01
      PS(19)=LOWE+0.01
      PS(20)=LSWE+0.01
      PS(21)=LOSC+0.01
      PS(22)=LCOVER+0.01
      PS(23)=LSUMS+0.01
      PS(24)=IPRINT+0.01
      PS(25)=LPM+0.01
      PS(26)=LADC+0.01
      PS(27)=LTAPM+0.01
      PS(28)=LUPPM+0.01
      PS(29)=LMFV+0.01
      PS(30)=LAEC+0.01
      ps(31)=lowev+0.01
      ps(32)=lswev+0.01
      ps(33)=lkfpm+0.01
      PS(34)=0.01
      PS(35)=0.01
      PS(36)=0.01
      PS(37)=0.01
      PS(LPM)=PXADJ
      PS(LPM+1)=ELEV
      PS(LPM+2)=SCF
      PS(LPM+3)=MFMAX
      PS(LPM+4)=MFMIN
      PS(LPM+5)=UADJ
      PS(LPM+6)=SI
      PS(LPM+7)=NMF
      PS(LPM+8)=TIPM
      PS(LPM+9)=MBASE
      PS(LPM+10)=PXTEMP
      PS(LPM+11)=PLWHC
      PS(LPM+12)=DAYGM
      PS(LPM+13)=ALAT
      DO 141 I=2,10
      J=LADC+I-2
  141 PS(J)=ADC(I)
      IF(LMFV.EQ.0) GOTO 139
      DO 138 I=1,12
       J=LMFV+I-1
  138 PS(J)=SMFV(I)
  139 IF (LRM.EQ.0) GO TO 151
      PS(LRM)=RMID(1)
      PS(LRM+1)=RMID(2)
      PS(LRM+2)=RMTYPE
  151 IF(LPCTS.EQ.0) GO TO 142
      PS(LPCTS)=PSID(1)
      PS(LPCTS+1)=PSID(2)
      PS(LPCTS+2)=PSTYPE
  142 IF(LOWE.EQ.0) GO TO 170
      PS(LOWE)=OWEID(1)
      PS(LOWE+1)=OWEID(2)
      PS(LOWE+2)=OWETYP
      PS(LOWE+3)=ITOWE+0.01
 170  if(lowev .eq. 0) go to 143
      ps(lowev)=owevid(1)
      ps(lowev+1)=owevid(2)
      ps(lowev+2)=owevtyp
      ps(lowe+3)=itowe+0.01
  143 IF(LSWE.EQ.0) GO TO 175
      PS(LSWE)=SWEID(1)
      PS(LSWE+1)=SWEID(2)
      PS(LSWE+2)=SWETYP
      PS(LSWE+3)=ITSWE+0.01
 175  if(lswev .eq. 0) go to 144
      ps(lswev)=swevid(1)
      ps(lswev+1)=swevid(2)
      ps(lswev+2)=swevtyp
      ps(lswe+3)=itswe+0.01
  144 IF(LOSC.EQ.0) GO TO 145
      PS(LOSC)=OSCID(1)
      PS(LOSC+1)=OSCID(2)
      PS(LOSC+2)=OSCTYP
      PS(LOSC+3)=ITOSC+0.01
  145 IF(LCOVER.EQ.0) GO TO 146
      PS(LCOVER)=SSCID(1)
      PS(LCOVER+1)=SSCID(2)
      PS(LCOVER+2)=SSCTYP
      PS(LCOVER+3)=ITSSC+0.01
  146 IF(LSUMS.EQ.0) GO TO 148
      DO 147 I=1,7
  147 PS(LSUMS+I-1)=0.0
  148 IF(LTAPM.EQ.0) GO TO 150
      PS(LTAPM)=TAELEV
      PS(LTAPM+1)=TALMAX
      PS(LTAPM+2)=TALMIN
  150 IF (LAEC.EQ.0) GO TO 155
      PS(LAEC)=NPTAE+0.01
      PS(LAEC+1)=IAEU+0.01
      PS(LAEC+2)=RSID(1)
      PS(LAEC+3)=RSID(2)
      PS(LAEC+4)=RSTYPE
      CONV=1.0
      IF (IAEU.EQ.0) CONV=0.3048
      DO 152 I=1,NPTAE
      J=LAEC+5+(I-1)*2
      PS(J)=AE(1,I)*CONV
      PS(J+1)=AE(2,I)
  152 CONTINUE
  155 IF(LUPPM.EQ.0) GO TO 162
      PS(LUPPM)=WETOL
      PS(LUPPM+1)=SCTOL
      IF(NUP.EQ.2) GO TO 162
C     THE NEXT 2 POSITIONS MAY NOT BE ZERO IN ALL EXISTING FILES.
C         PREVIOUSLY HELD THE CARROLL PROCEDURE PARAMETERS.
      PS(LUPPM+2)=0.01
      PS(LUPPM+3)=0.01
      PS(LUPPM+4)=1.0
      PS(LUPPM+5)=1.0
      PS(LUPPM+6)=1.0
  162 if(lkfpm .eq. 0) go to 159
      loc=lkfpm
      do 191 i=1,llfltr
         do 190 j=1,llfltr
            ps(loc)=cvu(j,i)
            loc=loc+1
  190    continue
  191 continue
      do 164 i=1,nnfltr
         do 163 j=1,nnfltr
            ps(loc)=q(j,i)
            loc=loc+1
  163    continue
  164 continue
      do 168 i=1,12
         ps(loc)=rmonth(i)
         loc=loc+1
  168 continue
c--------------------------------------
c     store RAIN SNOW ELEVATION OPERATION PARAMETER
c     
      ps(loc) = rnsnwop(1)
      loc = loc + 1
      ps(loc) = rnsnwop(2)
      loc = loc + 1
      ps(loc) = rlapse
      loc = loc + 1
      ps(loc) = irnsnop + 0.01
c---------------------------------------
C     store flag for updating with snow cover computed
C     by the Kalman Filter when water equiv is updated.
      loc = loc + 1
      ps(loc) = iscfltr + 0.01
c
C     ALL PARAMETER VALUES STORED
  159 IUSEP=NPS
C.......................................
C     STORE CARRYOVER VALUES IN CS()
      CALL CSAV31(CS, iprop)
      IUSEC=NCS
C.......................................
C     CHECK FOR DEBUG OUTPUT
      IF(IBUG.EQ.0) GO TO 199
C     DEBUG OUTPUT.
      WRITE(IODBUG,909)
  909 FORMAT(1H0,43HSNOW-43 DEBUG--CONTENTS OF PS AND CS ARRAYS)
      WRITE(IODBUG,396)
  396 FORMAT(1H0,"PS Array Real")
      WRITE(IODBUG,910) (PS(I),I=1,IUSEP)
  910 FORMAT(1H0,15F8.3)
      WRITE(IODBUG,397)
  397 FORMAT(1H0,"PS Array Words")
      WRITE(IODBUG,911) (PS(I),I=1,IUSEP)
  911 FORMAT(1H0,15(4X,A4))
      WRITE(IODBUG,398)
  398 FORMAT(1H0,"CS Array Real")
      WRITE(IODBUG,910) (CS(I),I=1,IUSEC)
C.......................................
  199 CONTINUE
      IF(ITRACE.GE.1) WRITE(IODBUG,987)
  987 FORMAT(1H0,13H** EXIT PIN31)
      RETURN
      END
