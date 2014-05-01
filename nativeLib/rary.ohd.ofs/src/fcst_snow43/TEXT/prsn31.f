C     Module PRSN31
C.......................................
C     FOR OPERATION 31 (SNOW-43)
C     THIS ROUTINE PRINTS OUTPUT FOR THE 'SNOW-43 ' OPERATION.
C.......................................
C     ROUTINE INITIALLY WRITTEN BY...
C        ERIC ANDERSON - HRL   MAY 1980 FOR THE SNOW-17 OPERATION
C     Modified by...
C         Nalneesh Gaur for SNOW-43 Aug 1995
C.......................................
      SUBROUTINE PRSN31(KDA, KHR, IPRINT, ITITLE, ANAME,
     1                  TWE, twev, owev, CWE, PTWE,
     2                  POWE, PCOVER, CAESC, POSC, LAEC,
     3                  MOPR, IDPR, IYPR, IHPR)
C
CC    implicit none
C----- D E C L A R A T I O N S --------------------------
C     --- F U N C T I O N  A R G U M E N T S ---
      integer kda, khr, iprint, ititle
      real    aname, twe, twev, owev, cwe, ptwe
      real    powe, pcover, caesc, posc
      integer laec, mopr, idpr, iypr, ihpr
C     --- L O C A L ---
      integer       i, j, ih
      integer       idprint, ipprint
      real          prtval(28)
      real          conv, convm, sqconv
      real          ecwe, rsl, pliqw, tzcode, etwe
      character*256 fmt
      integer       nxtfil, nxtpos

      integer iprup, iprday
      real    opname

      CHARACTER*4   UNIT,UTMM,UTIN
C----- D I M E N S I O N --------------------------------
      DIMENSION ANAME(5)

C----- T Y P E statements for formating -----------------
      EXTERNAL      FMTDIG
      CHARACTER*1   FMTDIG
      CHARACTER*39  FORMT1
      CHARACTER*35  FORMT2
      CHARACTER*45  STMT1
      CHARACTER*32  STMT2

C----- C O M M O N  B L O C K S -------------------------
C           what's in the common:
C             COMMON/IONUM/IN,IPR,IPU
C             COMMON/FENGMT/METRIC
C             COMMON/WHERE/ISEG(2),IOPNUM,OPNAME(2)
C             COMMON/FCTIME/IDARUN,IHRRUN,LDARUN,LHRRUN,LDACPD,LHRCPD,
C            $              NOW(5),LOCAL,NOUTZ,NOUTDS,NLSTZ,IDA,IHR,
C            $              LDA,LHR,IDADAT

      INCLUDE 'common/ionum'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fengmt'
      INCLUDE 'common/where'
      include 'snow43/snco31'
      include 'snow43/sums31'
      include 'snow43/cntl31'
      include 'snow43/cupdt31'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_snow43/RCS/prsn31.f,v $
     . $',                                                             '
     .$Id: prsn31.f,v 1.2 2000/12/19 15:02:27 dws Exp $
     . $' /
C    ===================================================================
C
C
C----- D A T A  S T A T E M E N T S ---------------------
      DATA  fmt/'(1H ,I3,1x,f8.1,1x,f8.1,1x,f10.1,F10.2,2F12.1,F14.0'/
      DATA  UTMM,UTIN / '''MM''','''IN''' /

      DATA  STMT1  / ' STATE ERROR COVARIANCE MATRIX (P) DIAGONALS ' /
      DATA  STMT2  / '(WE, NEGHS, LIQW, TINDEX, AESC):'              /
      DATA  FORMT1 / '(''0'',A,A,F10.0,F10.0,F10.0,F10.0,F10.0)'     /
      DATA  FORMT2 / '(36X,F10.0,F10.0,F10.0,F10.0,F10.0)'           /
C
C     Setup IPPRINT and IDPRINT
C
      IPPRINT = (IPRINT/10) * 10
      IDPRINT = IPRINT - IPPRINT
C
C.......................................
C     PRINTING ONLY OCCURS IF HOUR=24 UNLESS UPDATE OCCURS.
      IPRUP=0
      IF( (CWE.NE.TWE) .or. (caesc .ne. pcover) ) IPRUP=1
      IF((KHR.NE.24).AND.(IPRUP.EQ.0)) RETURN
C.......................................
C     CHECK IF PRINT NEEDED FOR THIS DAY
      IPRDAY=0
      IF (KHR.NE.24) GO TO 175
      IF(IDPRINT.GT.1) GO TO 171
C
C     PRINT ON ALL DAYS WHEN TWE OR PTWE ARE GREATER THAN ZERO.
      IF(TWE.GT.0.0) GO TO 173
      IF (PTWE.GT.0.0) GO TO 173
      GO TO 174
C
C     PRINT ONLY SIGNIFICANT DAYS.
  171 IF(TWE.GT.0.0) GO TO 172
      IF(PTWE.GT.0.0) GO TO 173
      GO TO 174
  172 IF(PTWE.EQ.0.0) GO TO 173
      IF(KDA.EQ.LDA) GO TO 173
      IF(KDA.EQ.LDACPD) GO TO 173
      IF(DRAIN.GT.5.0) GO TO 173
      IF(DSFALL.GT.5.0) GO TO 173
      IF(DQNET.GT.5.0) GO TO 173
      IF((POWE.GE.0.0).OR.(POSC.GE.0.0)) GO TO 173
      GO TO 174
  173 IPRDAY = 1
  174 PTWE=TWE
  175 IF((IPRDAY.EQ.0).AND.(IPRUP.EQ.0)) RETURN
C.......................................
C     UNITS
C     ENGLISH UNITS
      if ( metric .eq. 0.)then
          conv   = 1.0/25.4
C         1 feet = 0.3408M
          convm  = 2.9342723004
          sqconv = conv * conv
C     METRIC UNITS
      else
          conv   = 1.0
          convm  = 1.0
          sqconv = 1.0
      endif
      ETWE=TWE * CONV
  180 IF(ITITLE.EQ.1) GO TO 177
C.......................................
C     PRINT TITLE.
      DO 176 I=1,2
  176 WRITE(IPR,630)
  630 FORMAT(1H0)
      CALL MDYH1(KDA,KHR,MOPR,IDPR,IYPR,IHPR,NOUTZ,NOUTDS,TZCODE)
      IH=IHPR+24-KHR
      IF (IH.GT.24) IH=IH-24
      UNIT=UTMM
      IF (METRIC.EQ.0) UNIT=UTIN
      WRITE(IPR,631) OPNAME,(ANAME(I),I=1,5),MOPR,IYPR,UNIT,IH,TZCODE
      if(iprop .eq. 0) then
          WRITE(IPR,632)
      else
          WRITE(IPR,633)
      endif
C
      ITITLE=1
  177 IF(IPRUP.EQ.0) GO TO 178
C.......................................
C     PRINT UPDATE.
      IF (CWE .EQ. TWE) GO TO 179
      IF (METRIC.EQ.0) GO TO 181
      WRITE(IPR,634) IDPR,IHPR,CWE,TWE
      GO TO 179
  181 ECWE=CWE*CONV
      WRITE(IPR,635) IDPR,IHPR,ECWE,ETWE
  179 IF(CAESC .NE. PCOVER) then
          WRITE(IPR,635) IDPR,IHPR,CAESC,PCOVER
      endif
  178 IF(IPRDAY.EQ.0) go to 999
C.......................................
C     PRINT DAILY OUTPUT
C
C
      PLIQW=0.0
      IF(WE.GT.0.0) PLIQW=(LIQW/WE)*100.0
      prtval(1) = DSFALL*conv
      prtval(2) = DRAIN*conv
      prtval(3) = dqnet*conv
      prtval(4) = pcover
      prtval(5) = pliqw
C
      prtval(6) = neghs*conv
      prtval(7) = twe*conv
      nxtfil = 8
      nxtpos = 52
C
C     Simulated Water Equivalent variance
C
      if(iprop .eq. 1) then
          fmt(nxtpos: ) = ' ,f14.0'
          prtval(nxtfil) = twev * sqconv
          nxtfil = nxtfil + 1
          nxtpos = nxtpos + 7
      endif
C
C     Observed Water Equivalent
C
      if(powe .ge. 0.0) then
          prtval(nxtfil) = powe * conv
          fmt(nxtpos: ) = ' ,f12.0'
          nxtpos = nxtpos + 7
          nxtfil = nxtfil + 1
      else
          fmt(nxtpos: ) = ' ,12h            '
          nxtpos = nxtpos + 17
      endif
C
C     Observed Water Equivalent variance
C
      if( iprop .eq. 1 ) then
         if(owev .ge. 0.0) then
             prtval(nxtfil) = owev * sqconv
             fmt(nxtpos: ) = ' ,f10.0'
             nxtpos = nxtpos + 7
             nxtfil = nxtfil + 1
         else
             fmt(nxtpos: ) = ' ,10h          '
             nxtpos = nxtpos + 15
         endif
      endif
C
C     Observed Snow Cover
C
      IF(POSC.GE.0.0) then
          prtval(nxtfil) = posc
          fmt(nxtpos: ) = ' ,f12.2'
          nxtpos = nxtpos + 7
          nxtfil = nxtfil + 1
      else
          fmt(nxtpos: ) = ' ,12h            '
          nxtpos = nxtpos + 17
      endif
C
C     Rain-Snow Elevation
C
      if(laec .ne. 0. .and. ndrsp .ne. 0.) then
          rsl = drsl/ndrsp
          prtval(nxtfil) = rsl * convm
          fmt(nxtpos: ) = ' ,f14.0'
          nxtpos = nxtpos + 7
          nxtfil = nxtfil + 1
      endif
C
C     Close parentheses
      fmt(nxtpos: ) = ' )'
      nxtfil = nxtfil - 1
C
C     Now print all the stuff
      write(IPR, fmt) IDPR, (prtval(I),I=1,nxtfil)
C
C     Print P array if requested
C
      if ( ipprint .ge. 10. ) then
C
        if ( ipprint .eq. 10. ) then
C
C     DIAGONAL
C
          indx = 8
          do 210 i=1,nnfltr
             indx = indx+6
             FORMT1(indx:indx) = FMTDIG( p(i, i), 8 )
 210      continue
          write(IPR, FORMT1) STMT1,STMT2,(p(I,I),I=1,nnfltr)
        endif
C
        if ( (ipprint .eq. 20.) .or.
     $       ( ipprint .eq. 30. .and. idpr .eq. 1 ) ) then
C     FULL
C
            write(IPR, 625)
            do 215 i=1,nnfltr
                indx = 3
                do 216 j=1,nnfltr
                    indx = indx+6
                    FORMT2(indx:indx) = FMTDIG( p(i, j), 8 )
  216           continue
                write(ipr, FORMT2) (p(i,j), j=1,nnfltr)
  215       continue
        endif
      endif
C
C     Format Statements
C
  625 format(1H0, ' STATE ERROR COVARIANCE MATRIX (P):      WE       NEG
     1HS     LIQW     TINDEX     AESC')
C 629 format(1H0, ' STATE ERROR COVARIANCE MATRIX (P) ON DAY 1')
  631 FORMAT(1H0,1H',2A4,22H'   SNOW-43 OUTPUT FOR,1X,5A4,5X,I2,1H/,I4,
     110X,10H(UNITS ARE,1X,A4,1X,22HEXCEPT FOR AREAL COVER,/11X,
     224HDAILY OUTPUT IS FOR HOUR,1X,I2,5X,10HTIME ZONE=,A4,28X,
     320HAND PCT. LIQ. WATER))
C
  632 FORMAT(1H0,26X,6HENERGY,3X,10HSIM. AREAL,3X,9HPCT. LIQ.,5X,
     14HHEAT,41X,9HRAIN-SNOW,
     2/1X,3HDAY,3X,8HSNOWFALL,3X,4HRAIN,4X,8HEXCHANGE,5X,
     35HCOVER,7X,5HWATER,5X,7HDEFICIT,6X,7HSIM. WE,5X,7HOBS. WE,
     42X,10HOBS. COVER,3X,9HELEVATION)
C
  633 FORMAT(1H0,26X,6HENERGY,3X,10HSIM. AREAL,3X,9HPCT. LIQ.,5X,
     14HHEAT,21x,8HSIM. WE ,13x,8HOBS. WE ,17X,9HRAIN-SNOW,
     2/1X,3HDAY,3X,8HSNOWFALL,3X,4HRAIN,4X,8HEXCHANGE,5X,
     35HCOVER,7X,5HWATER,5X,7HDEFICIT,7X,7HSIM. WE,6x,8HVARIANCE,
     44X,7HOBS. WE,2X,8hVARIANCE,4x,10HOBS. COVER,3X,9HELEVATION)
C
  634 FORMAT(1H ,8x,3HDAY,1X,I2,2X,4HHOUR,1X,I2,4X,2HWE,2X,
     112HCHANGED FROM,F9.2,1X,2HTO,F9.2)
C
  635 FORMAT(1H ,8x,3HDAY,1X,I2,2X,4HHOUR,1X,I2,2X,5HCOVER,1X,
     112HCHANGED FROM,F9.3,1X,2HTO,F9.3)
C.......................................
  999 RETURN
      END
