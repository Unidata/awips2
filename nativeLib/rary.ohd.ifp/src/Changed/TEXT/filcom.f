C$PRAGMA C (gethomedirectory)
C$PRAGMA C (getsegmentnamefromproperty)
C$PRAGMA C (pad_blanks)
C$PRAGMA C (getuniversaltechs)
C$PRAGMA C (getrundates)
C$PRAGMA C (set_end_date_atom)
c  module: filcom.f

C     Given two holleriths compares them to see if they are equal
      logical function lholeq(hol1,hol2)
         integer hol1,hol2
         lholeq = (hol1.eq.hol2)
      end

c  subroutine fills common blocks needed by the Interactive Forecast
c   Program to make hydrologic computations using NWSRFS OFS routines
c
c  routine originally written by George Smith, HRL, 1991
c
c
      SUBROUTINE FILCOM
C
      Equivalence (CURSEG(1), ICURSG(1))
c
      COMMON/PUNITS/KMAPTS,KGENTS,KFMPTS,KFUTTS,KFCTTS,KINDEX,KRFCPR
C
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/flarys'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/fmodft'
      INCLUDE 'common/fpwarn'
      INCLUDE 'common/fcfutp'
      INCLUDE 'common/ionum'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/where'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'ucmdbx'
      INCLUDE 'common/ffgctl'
      INCLUDE 'common/fcassm'
      INCLUDE 'ufreex'
c  Don't INCLUDE hdflts because we need to change a few var. names
C                                           *--> FROM HCLCOMMON.HDFLTS
      COMMON/HDFLTS/ TIME(3),HNAMRF(2),METRhd,CCODE,DDATES(7),TDATES(7),
     *                  LOCALh,NLSTZh,NHOPhd,NHOChd
      INTEGER TIME,HNAMRF,CCODE,TDATES,DDATES
c
      COMMON/FCKTMP/NEEDTX
c
      Character*150 SEGCHR
c
      DIMENSION utility_type(2)
      DIMENSION SEGNAM(2), operation_type(2)
      DIMENSION CURSEG(2), ICURSG(2)
      DIMENSION SBNAME(2),OLDOPN(2)
c

      logical lholeq

      DATA SBNAME/4HFILC,4HOM  /
      DATA BLANK4/4h    /,IBLANK4/4h    /
      Character*100 homdir
      Integer      length
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Changed/RCS/filcom.f,v $
     . $',                                                             '
     .$Id: filcom.f,v 1.5 2003/08/12 18:17:26 aivo Exp $
     . $' /
C  =====================================================================
C
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
c  read file '/u/nwsrfs/Prod.082291/working/system_debug_codes_on'
c    to fill sysbug common
      IREAD = 17

      CALL gethomedirectory(homdir, length)
      WRITE(SEGCHR(:), 200)homdir
 200  FORMAT(a)
      WRITE(SEGCHR(length+1:),
     1      '(''/.ifp_files/local/system_debug_codes_on'')')

      OPEN(UNIT=IREAD,
     1     FILE=SEGCHR,
     1     FORM='FORMATTED',STATUS='OLD',ERR=992)
C
      go to 100
 992  WRITE(IPR,'(A)') 'Problem opening file for system debug codes'
      IALL = 0
      NDEBGS = 0
      go to 118
C
 100  IALL = 0
      Do 110 i = 1, 20
      Read(IREAD, 630, END=111)IDEBGS(I)
 630  Format(2a4)
 110  Continue
c
 111  NDEBGS = i - 1
      CLOSE(IREAD)
c
c  see if debug code for filcom subroutine is on
c
 118  ibug = IFBUG(4hFILC)
c
      if(ibug.eq.1) then
        Write(ipr,637)NDEBGS
 637    Format(1x,i3,' system debug codes set.')
        if(NDEBGS .gt. 0) then
           Do 119 i = 1, NDEBGS
           Write(ipr, 631)i, IDEBGS(I)
 631       Format('System debug code ',i2,'. ',a4)
 119       Continue
        end if
      end if
c
c  read file '/u/nwsrfs/Prod.082291/working/operation_debug_codes_on'
c    to fill fdbug common

      WRITE(SEGCHR(:), 200)homdir
      WRITE(SEGCHR(length+1:),
     1      '(''/.ifp_files/local/operation_debug_codes_on'')')

      OPEN(UNIT=IREAD,
     1 FILE=SEGCHR,
     1     FORM='FORMATTED',STATUS='OLD',ERR=997)
C
      go to 115
 997  WRITE(IPR,'(A)') 'Problem opening file for operation debug codes'
      IDBALL = 0
      NDEBUG = 0
      go to 130
C
 115  Do 120 i = 1, 20
      Read(IREAD, 630, END=121)operation_type
c
      Call FOPCDE(operation_type, IDEBUG(i))
c
      if(ibug.eq.1)Write(ipr, 632)i, operation_type, IDEBUG(I)
 632  Format('Operation debug code ',i2,'. ',2a4,' (',i2,')')
 120  Continue
c
 121  NDEBUG = i - 1
      if(ibug.eq.1)Write(ipr,638)NDEBUG
 638  Format(1x,i3,' operation debug codes set.')
      CLOSE(IREAD)
c
c  read file '$HOME/.ifp_files/local/utility_debug_codes_on'
c    to fill udebug common

 130  WRITE(SEGCHR(:), 200)homdir
      WRITE(SEGCHR(length+1:),
     1      '(''/.ifp_files/local/utility_debug_codes_on'')')
c
      IHCLTR = 0
      IHCLDB = 0
      IPRTR = 0
      IPRDB = 0
      IPDTR = 0
      IPDDB = 0
      IPPTR = 0
      IPPDB = 0
      IUTLTR = 0
      IUTLDB = 0
      IDETR = 0
      IDEDB = 0
      ICMTRC = 0
      ICMDBG = 0
c
      OPEN(UNIT=IREAD,
     1 FILE=SEGCHR,
     1     FORM='FORMATTED',STATUS='OLD',ERR=983)
C
      go to 125
 983  WRITE(IPR,'(A)') 'Problem opening file for utility debug codes'
      go to 140
C
 125  Do 128 i = 1, 20
      Read(IREAD, 630, END=131)utility_type
c
      if(ibug.eq.1)Write(ipr, 627)utility_type
 627  Format('Utility debug code = ',2a4)
c
      if(lholeq(utility_type(1) , 4hIHCL) .and.
     1   lholeq(utility_type(2) , 4hTR  )) then
        IHCLTR = 1
      else if (lholeq(utility_type(1) , 4hIHCL) .and.
     1         lholeq(utility_type(2) , 4hDB  )) then
        IHCLDB = 1
      else if (lholeq(utility_type(1) , 4hIPRT) .and.
     1         lholeq(utility_type(2) , 4hR   )) then
        IPRTR = 1
      else if (lholeq(utility_type(1) , 4hIPRD) .and.
     1         lholeq(utility_type(2) , 4hB   )) then
        IPRDB = 1
      else if (lholeq(utility_type(1) , 4hIPDT) .and.
     1         lholeq(utility_type(2) , 4hR   )) then
        IPDTR = 1
      else if (lholeq(utility_type(1) , 4hIPDD) .and.
     1         lholeq(utility_type(2) , 4hB   )) then
        IPDDB = 1
      else if (lholeq(utility_type(1) , 4hIPPT) .and.
     1         lholeq(utility_type(2) , 4hR   )) then
        IPPTR = 1
      else if (lholeq(utility_type(1) , 4hIPPD) .and.
     1         lholeq(utility_type(2) , 4hB   )) then
        IPPDB = 1
      else if (lholeq(utility_type(1) , 4hIUTL) .and.
     1         lholeq(utility_type(2) , 4hTR  )) then
        IUTLTR = 1
      else if (lholeq(utility_type(1) , 4hIUTL) .and.
     1         lholeq(utility_type(2) , 4hDB  )) then
        IUTLDB = 1
      else if (lholeq(utility_type(1) , 4hIDET) .and.
     1         lholeq(utility_type(2) , 4hR   )) then
        IDETR = 1
      else if (lholeq(utility_type(1) , 4hIDED) .and.
     1         lholeq(utility_type(2) , 4hB   )) then
        IDEDB = 1
      else if (lholeq(utility_type(1) , 4hICMT) .and.
     1         lholeq(utility_type(2) , 4hRC   )) then
        ICMTRC = 1
      else if (lholeq(utility_type(1) , 4hICMD) .and.
     1         lholeq(utility_type(2) , 4hBG   )) then
        ICMDBG = 1
      end if
c
 128  Continue
c
 131  nutility = i - 1
      if(ibug.eq.1)Write(ipr,625)nutility
 625  Format(1x,i3,' utility debug codes set.')
      CLOSE(IREAD)
c
 140  Call GetSegmentNameFromProperty(CURSEG)
c
      Call pad_blanks(CURSEG, 8)
c
c  open a file for error messages - unit IOERR in /ERRDAT/
c
      WRITE(SEGCHR(:), 200)homdir
      WRITE(SEGCHR(length+1:),
     1      '(''/.ifp_files/local/error_messages'')')

      OPEN(UNIT=IOERR,
     1 FILE=SEGCHR,
     1     FORM='FORMATTED',STATUS='UNKNOWN',ERR=981)
C
      go to 135
 981  WRITE(IPR,'(A)') 'Problem opening file for error_messages'
C
C  fcrunc
 135  ITYPRN = 3
      RUNID(1) = CURSEG(1)
      RUNID(2) = CURSEG(2)
C      ISEGEX
      NSEGEX = 1
C      MSEGEX
C      IRSGEX(1000)
c
c  fcsegn
c
      idsegn(1) = ICURSG(1)
      idsegn(2) = ICURSG(2)
c
      Call GetUniversalTechs(IUMGEN, IUMSAC, IUMAPI, METRIC,
     1                       MODWRN, IFPR,
     2                       INPTZC, NOUTZ, NOUTDS, MODTZC)
c
c  fctime, fctim2, and fmodft
      if(ibug.eq.1)Write(ipr,703)INPTZC, NOUTZ, NOUTDS, MODTZC
 703  Format("INPTZC, NOUTZ, NOUTDS, MODTZC =",1x, a4, 2i10, 1x, a4)
c
c  fpwarn and fcfutp
      if(ibug.eq.1)Write(ipr,704)MODWRN, IFPR
 704  Format("MODWRN, IFPR = ",2i10)
c
c  fengmt and fmodft
      if(ibug.eq.1)Write(ipr,700)IUMGEN,IUMAPI,IUMSAC,METRIC
 700  Format("IUMGEN, IUMAPI, IUMSAC, METRIC = ",4i10)
C
C  fprog
      MAINUM = 1
      VERS = BLANK4
      VDATE(1) = BLANK4
      VDATE(2) = BLANK4
      DO 34 i=1, 5
        PNAME(i) = BLANK4
  34  CONTINUE
      NDD = 31
c
c  fctime
      DO 38 i=1,5
         NOW(i) = 0
 38   CONTINUE
      LOCAL = 5
      NLSTZ = -7
      icount=1
c
      Call GetRunDates(ISTRHR, ILCDHR, IENDHR, ISDAHR, icount)
C
      if(IENDHR - ISTRHR .gt. NDD*24) then
         IENDHR = ISTRHR + NDD*24
         IDARUN = ISTRHR/24 +1
         IHRRUN = ISTRHR - (IDARUN-1)*24
         LDARUN = IENDHR/24 +1
         LHRRUN = IENDHR - (LDARUN-1)*24
         if(LHRRUN .eq. 0) then
            LDARUN = LDARUN - 1
            LHRRUN = 24
         end if
C
      CALL MDYH2(IDARUN,IHRRUN,NBEGM,NBEGD,NBEGY,NBEGH,NZXX,NDXX,INPTZC)
      CALL MDYH2(LDARUN,LHRRUN,NENDM,NENDD,NENDY,NENDH,NZXX,NDXX,INPTZC)
c
c  post Xwindow atom for new end date/time
c
      Call set_end_date_atom(NENDM,NENDD,NENDY,NENDH,INPTZC)
c
      WRITE(IPR,626)NENDM,NENDD,NENDY,NENDH,INPTZC,NDD
  626 FORMAT(1H0,10X,'**WARNING** THE ENDING DATE AND TIME HAVE BEEN ',
     1  'CHANGED TO',1X,I2,1H/,I2,1H/,I4,1H-,I2,1X,A4/
     2  23X,'BECAUSE THE RUN PERIOD WAS LONGER THAN', I3, ' DAYS.')
c
      CALL WARN
c
      write(IPR,634)
      write(IPR,635)
      write(IPR,636)
      write(IPR,618)NBEGM,NBEGD,NBEGY,NBEGH,INPTZC,
     1              NENDM,NENDD,NENDY,NENDH,INPTZC
      WRITE(IPR,636)
      WRITE(IPR,635)
      WRITE(IPR,634)
c
 634  FORMAT(1H )
 635  FORMAT(11X,100(1H*)/11X,100(1H*))
 636  FORMAT(11X,5(1H*),90X,5(1H*)/11X,5(1H*),90X,5(1H*))
 618  FORMAT(11X,5(1H*),6X,'**NOTE** THE RUN PERIOD IS NOW FROM ',
     1 I2,1H/,I2,1H/,I4,1H-,I2,1X,A4,'. TO ',
     2 I2,1H/,I2,1H/,I4,1H-,I2,1X,A4,1H.,6X,5(1H*))
c
      end if
c
      IDARUN =  ISTRHR/24 +1
      IHRRUN =  ISTRHR - (IDARUN-1)*24
      LDARUN =  IENDHR/24 +1
      LHRRUN =  IENDHR - (LDARUN-1)*24
      if(LHRRUN .eq. 0) then
         LDARUN = LDARUN - 1
         LHRRUN = 24
      end if
      LDACPD =  ILCDHR/24 +1
      LHRCPD =  ILCDHR - (LDACPD-1)*24
      if(LHRCPD .eq. 0) then
         LDACPD = LDACPD - 1
         LHRCPD = 24
      end if
      IDA = IDARUN
      IHR = IHRRUN
      LDA = LDARUN
      LHR = LHRRUN
      IDADAT = ISDAHR/24 + 1
      if(ibug.eq.1)Write(ipr,701)ISTRHR, ILCDHR, IENDHR, ISDAHR,
     1                           IDA, IHR, LDA, LHR, IDADAT
 701  Format("ISTRHR, ILCDHR, IENDHR, ISDAHR =",4i10/
     1       "IDA, IHR, LDA, LHR, IDADAT =",5i10)
C
C  fctim2
      NHOPDB = 12
      NHOCAL = 0
      DO 36 i=1, 8
         NHROFF(i) = 0
  36  CONTINUE
C
C  hdflts
      LOCALh = LOCAL
      NLSTZh = NLSTZ
      NHOPhd = NHOPDB
      NHOChd = NHOCAL
C
C  flarys
      LTS = NTS
      LP = NP
      LC = NC
      LT = NT
      LD = ND
c
c  pdftbl  - set number of processed data base time series to be
c             kept in-core to 0 for all data types
c
      Do 40 i = 1, 50
         DATFIL(3, i) = 0
  40  Continue
c
c  ffgctl - set flag for ffg run to off (not a ffg run)
      IFFG = 0
c
c  fcassm - set flag for assimilator run to off
      ASSMRUN = 0
c
c
c  set variables for common ufreex - now done in
c       fcstm2.f which we don't link with - dp 25 Jan. 1996
      IOPREP=1
      ICDSPC=0
      ICDQTE=0
c
C     RESET WHERE COMMON BLOCK
C
      IOPNUM=0
      ISEG(1)=IDSEGN(1)
      ISEG(2)=IDSEGN(2)
      OPNAME(1)=SBNAME(1)
      OPNAME(2)=SBNAME(2)
C
      RETURN
      END
