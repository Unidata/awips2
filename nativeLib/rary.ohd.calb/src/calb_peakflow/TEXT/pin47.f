C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE PIN47
C
      SUBROUTINE PIN47(PO,LEFTP,IUSEP,CO,LEFTC,IUSEC,WORK,
     &SWORK,LEFTW)
C....................................................................
C
C     THIS IS THE INPUT ROUTINE FOR THE 'PEAKFLOW' OERATION.  THIS
C     ROUTINE READS IN ALL INPUT CARDS AND PERFORMS THE NECESSARY
C     CHECKS.  THE TIME SERIES ARE CHECKED, AND THE PO AND CO ARRAYS
C     ARE FILLED.  PIN47 CALLS ROUTINE URPEAK WHICH RETURNS OBSERVED
C     INSTANTANEOUSE PEAKFLOW DATA FROM THE USGS DATA BASE AND STORES
C     THE DATA IN THE SCRATCH FILE RECORDS.
C
C     THIS ROUTINE WAS INITIALLY WRITTEN BY:
C       BRYCE D. FINNERTY    HYDROLOGIC RESEARCH LAB  DECEMBER, 1994.
c
cbf   This routine was redesign to work from new peakflow data that
cbf   is available over the internet or FTP from the USGS.
cbf
cbf   Bryce D. Finnerty,  Hydrologic Research Lab, October, 1997.
cbf
C....................................................................
C
C     CONTENTS OF THE PO ARRAY ARE AS FOLLOWS:
C
C     POSITION                        CONTENTS
C
C        1            VERSION NUMBER OF THE OPERATION.
C
C       2-6           20 CHARACTER USER SUPPLIED BASIN HEADING
C                     DESCRIPTION.
C
C       7-8           SIMULATED INSTANTANEOUS DISCHARGE TIME SERIES
C                     INTERNAL IDENTIFIER.
C
C        9            SIMULATED INSTANTANEOUS DISCHARGE TIME SERIES
C                     DATA TYPE.
C
C        10           SIMULATED INSTANTANEOUS DISCHARGE TIME SERIES
C                     TIME INTERVAL (HOURS).
C
C      11-12          OBSERVED INSTANTANEOUS PEAKFLOW USGS STATION
C                     IDENTIFICATION NUMBER.
C
C        13           WINDOW SIZE (DAYS) ON EACH SIDE OF OBSERVED PEAK
C                     FOR DETERMINING CORRESPONDING SIMULATED PEAK.
C
C
C        14           TOTAL NUMBER OF OBSERVED PEAKS TO BE PROCESSED.
C
C        15           DISPLAY OPTION SWITCH FOR SIMULATED VS. OBSERVED
C                     PEAKS, 0=TABLE ONLY, 1=GRAPH AND TABLE.
C
C       16            OPTION FLAG: 0 = DISPLAY PEAKS CHRONOLOGICALLY.
C                                  1 = DISPLAY PEAKS BY MAGNITUDE.
C
CBF    17-24          FILE NAME OF INPUT DATA, 32 CHARACTERS.
C
C
C....................................................................
      DIMENSION  PO(*),CO(*),WORK(*),LSTDA(12),SWORK(*)
      DIMENSION  BASNAM(5),PEAKID(2),SIMTS(2),STYPE(1)
      DIMENSION  DATAFN(8),temp(20),temp1(20)
c
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fwyds'
      INCLUDE 'common/fwydat'
      INCLUDE 'common/fctime'
C
      CHARACTER*32 DATAFNC
      CHARACTER*8  CHPEAK,SNAME
      CHARACTER*4  UNITX,UNITD
      character*1  filenm,fmt
      character    dirpeak*128, tokstr*64, filename*128
      integer toklen, dirlen, filelen, datalen
C
      EQUIVALENCE (CHPEAK,PEAKID(1))
      EQUIVALENCE (DATAFN(1),DATAFNC)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_peakflow/RCS/pin47.f,v $
     . $',                                                             '
     .$Id: pin47.f,v 1.6 2002/02/11 13:29:21 michaelo Exp $
     . $' /
C    ===================================================================
C
      data filenm/1h /, fmt/1hU/
      DATA  DATAFNC / 1h  /
C
      DATA  SNAME / 8hPIN47    /
      DATA  LSTDA /31,28,31,30,31,30,31,31,30,31,30,31/
      DATA  DIM / 4hL3/T /
      DATA  UNITX / 4hCMS  /
C
C   TRACE LEVEL FOR ROUTINE=1, DEBUG SWITCH=IBUG
      CALL FPRBUG(SNAME,1,47,IBUG)
c
c      itrace=1
C
C   INITIALIZE VALIABLES, WORK, AND SWORK ARRAYS TO ZERO.  WORK AND
C   SWORK ARE OF EQUAL LENGTH=LEFTW.
      DO 600 I=1,LEFTW
        WORK(I)=0
        SWORK(I)=0
  600 CONTINUE
      IERROR=0
      IERR=0
C
C   READ INPUT CARDS, PERFORM INPUT ERROR CHECKS
C
      READ(IN,860) BASNAM,CHPEAK,SIMTS,STYPE
      READ(IN,862) IDT,IWINDOW,IDISP,MAGFLG,DATAFN
c     IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,861)
        WRITE(IPR,860)BASNAM,CHPEAK,SIMTS,STYPE
        WRITE(IPR,862)IDT,IWINDOW,IDISP,MAGFLG,DATAFN
        WRITE(IPR,855)LEFTW,LEFTP,LEFTC
  861   FORMAT('0**ECHO OUTPUT OF INPUT CARD DECK**')
  860   FORMAT(5A4,2X,A8,2X,2A4,1X,A4)
  862   FORMAT(3X,I2,4X,I1,4X,I1,4X,I1,4X,8A4)
  855   FORMAT(/,10X,'LEFTW,LEFTP,LEFTC = ',3I10)
      END IF
C
C   CHECK THE TIME SERIES AND USER SPECIFIED OPTIONS.
C
       IF ((MAGFLG.LT.0).OR.(MAGFLG.GT.1)) THEN
         CALL WARN
         MAGFLG=1
         WRITE(IPR,662)
       END IF
  662  FORMAT('0**WARNING** USER OPTION FLAG TO DISPLAY',
     &' PEAK FLOW DATA CHRONOLOGICALLY',/,10X,'OR BY MAGNITUDE OF ',
     &'FLOW IS NOT SET TO ZERO OR ONE RESPECTIVELY.',/,10X,'DEFAULT ',
     & 'VALUE IS SET TO ONE, SORTING BY MAGNITUDE OF FLOW.')
C
       IF ((IWINDOW.GT.3).OR.(IWINDOW.LT.1)) THEN
         WRITE(IPR,666)
         CALL WARN
         IWINDOW=3
       END IF
  666 FORMAT('0**WARNING** WINDOW SIZE FOR PEAK SEARCH IS ',
     &'OUT OF RANGE.  USER SPECIFIED VALUE',/,10X,'MUST BE BETWEEN ',
     &'1 AND 3 DAYS.  A DEFAULT VALUE OF 3 WILL BE ASSUMED.')
C
      IF (IDISP.ne.0) THEN
        IDISP=0
        CALL WARN
        WRITE(IPR,667)
      END IF
  667 FORMAT('0**WARNING** OUTPUT DISPLAY OPTION SWITCH IS',
     &' NOT EQUAL TO ZERO.',/,10X,'A DEFAULT VALUE OF 0 IS ASSUMED.',
     &'  ONLY THE TABLE OPTION IS CURRENTLY AVAILABLE.')
C
      ICKDIM=1
      MISSNG=0
      NVAL=1
      CALL CHEKTS(SIMTS,STYPE,IDT,ICKDIM,DIM,MISSNG,NVAL,IERR)
      IF(IERR.GT.0) THEN
        IERROR=1
        WRITE(IPR,300)
  300   FORMAT('0**WARNING** ROUTINE CHEKTS RETURNED AN ',
     &  'ERROR FLAG.  TIME SERIES IS NOT VALID.')
        CALL WARN
      END IF
C
      IERR=0
      CALL FDCODE(STYPE,UNITD,DIMD,MSGD,NPDT,TSCALE,NADD,IERR)
C
      IF (IERR.GT.0) THEN
        IERROR=1
        CALL WARN
        WRITE(IPR,305)
 305    FORMAT('0**WARNING** NOT A VALID DATA TYPE FOR THE',
     &  ' FORECAST COMPONENT.  CHECK DATA TYPE OF TIME SERIES.')
      END IF
C
      IF (UNITD.NE.UNITX) THEN
        CALL WARN
        WRITE(IPR,669)
      END IF
  669 FORMAT('0**WARNING** THE UNITS FOR THE SIMULATED',
     &' TIME SERIES ARE NOT CONSISTENT WITH NWSRFS STANDARD UNITS')
C
C   CHECK FOR SPACE IN THE P ARRAY
C
      NEEDP=24
      CALL CHECKP(NEEDP,LEFTP,IERR)
      IF(IERR.EQ.1) THEN
        IERROR=1
        IUSEP=0
        WRITE(IPR,671)
        CALL WARN
      END IF
  671 FORMAT('0**ERROR** NOT ENOUGH SPACE IN THE P ARRAY FOR',
     &' OPERATION 47 - PEAKFLOW')
C
C   CHECK FOR SPACE AVAILABLE IN THE C ARRAY.  REQUIRED SIZE OF C
C   ARRAY IS A FUNCTION OF THE SIZE OF THE USER DEFINED SEARCH WINDOW.
C
      NEEDC=4*2*IWINDOW+5
      CALL CHECKC(NEEDC,LEFTC,IERR)
      IF(IERR.EQ.1) THEN
        IERROR=1
        IUSEC=0
        CALL WARN
        WRITE(IPR,672)
      END IF
  672 FORMAT('0**ERROR** NOT ENOUGH SPACE IN THE C ARRAY FOR',
     &' OPERATION 47 - PEAKFLOW')
C
C   GET DATES FOR FIRST AND LAST DAY OF THE RUN
      KHR=12
      CALL MDYH1(IDARUN,KHR,IMO,IDA,IYR,IHOUR,NOUTZ,NOUTDS,ITZ)
      CALL MDYH1(LDARUN,KHR,LMO,LDA,LYR,LHOUR,NOUTZ,NOUTDS,LTZ)
C
C  *****************************************************************
c  *****************************************************************
c
c  get the path of the directory in which the file will be written
c
      toklen=17
      tokstr='peakflow_data_dir'
      tokstr(18:18)=char(0)
      call get_apps_defaults(tokstr,toklen,dirpeak,dirlen)
      dirpeak(dirlen+1:dirlen+1) = '/'
      dirlen = dirlen+1
c
c      IBUG=1
      IF (IBUG .GT. 0)  THEN
        WRITE(IPR,330)tokstr,dirpeak
        WRITE(IPR,335)toklen,dirlen
  330   FORMAT('TOKEN=',A64,/,'dirpeak=',A128)
  335   FORMAT('toklen=',I5,5x,'dirlen=',I5)
      END IF
c
      call ulenth(datafn,32,datalen)
      filename=dirpeak(1:dirlen)//datafnc(1:datalen)
      filelen=dirlen+datalen
      filename(filelen+1:filelen+1)=char(0)
c
c   open peakflow data file.
      call opfile( filename,'PEAKDATA ','SEQUENTIAL','OLD',
     & 'FORMATED',0,iounit,IERR)
      if (ierr .ne. 0 ) then
        write(ipr,340)filename
  340 format(10x,'**WARNING** Error opening Peakflow data file =',a128)
        call warn
        istat=1
        goto 999
      end if
  999 continue
cbf  ***************************************************************
cbf  read the peakflow input data file with fixed format.  if any data
cbf  is in the wrong format the thing will exit the routine with istat=2
cbf  it will skip over comment cards and header cards because of the
cbf  iflag1 is read as i2 when it is really i1. this will cause an error
cbf  on the comments and header lines.
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      numpks=0
  10   read(iounit,500,err=1000,end=900)iflag1,ista,iiyr,imn,iday,peakq,
     & flagq,peakh,flagh
      istat=0
  500  format(i2,6x,a8,8x,i4,1x,i2,1x,i2,6x,f7.0,1x,a1,7x,f5.2,3x,a1)
c
c     first year is iyr:  last year is lyr; iiyr is read in above
       if (iiyr .lt. iyr) goto 10
       if (iiyr .gt. lyr) goto 10
       if ((iiyr .eq. iyr) .and. (imn .lt. imo)) goto 10
       if ((iiyr .eq. lyr) .and. (imn .gt. lmo)) goto 10
       numpks=numpks+1
 1000  continue
       goto 10
  900  continue
c     close peakflow input data file
      call clfile('PEAKDATA ',iounit,ierr)
c
      if (ierr .ne. 0 ) then
         write(ipr,350)filename
  350 format(10x,'**WARNING** Error closing Peakflow data file =',a128)
         call warn
         istat=3
         goto 998
      end if
  998 continue
C  ********************************************************************
C   CALCULATE THE WORK SPACE REQUIRED FOR PEAK DATA
C   ARRAYS TO BE RETRIEVED FROM ROUTINE URPEAK.
C
      LSPACE = 0
C
      LSPACE = numpks*10
      IF (IBUG.EQ.1) WRITE(IODBUG,95) NWY,LSPACE
  95  FORMAT(/,10X,'NWY,LSPACE =',2(I12,2X))
C
C   CHECK FOR AVAILABLE WORK SPACE TO STORE PEAK DATA
      IF ( LSPACE .GE. LEFTW ) THEN
        CALL WARN
        IERROR=1
        WRITE(IPR,710)
 710  FORMAT('0**WARNING** INSUFFICIENT WORK SPACE TO STORE',
     &' REQUESTED OBSERVED INSTANTANEOUS PEAK FLOW DATA.')
      END IF
C
C
C  DONT RUN REMAINDER OF PIN47 ROUTINE IF ERROR HAS OCCURED ABOVE.
C
      IF (IERROR.GE.1) THEN
        IUSEP=0
        IUSEC=0
        WRITE(IPR,720)
 720  FORMAT('0**NOTE** DUE TO PRECEEDING WARNINGS OPERATION',
     &' PEAKFLOW IS IGNORED')
        GO TO 405
      END IF
C  *******************************************************************
C  CALL THE URPEAK ROUTINE TO TRANSFER OBSERVED INSTANTANEOUS PEAKFLOW
C  DATA INTO TEMPORARY WORK SPACE.
C*********************************************************************
C
c      IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,320)
        WRITE(IODBUG,310)CHPEAK,IMO,IYR,LMO,LYR
        WRITE(IODBUG,311)MAXPKS,DATAFN,LSPACE
 310    FORMAT(10X,'PEAKID=',A8,'  IMO=',I2,2X,'IYR=',I4,2X,'LMO=',
     &  I2,2X,'LYR=',I4)
 311    FORMAT(10X,'MAXPKS=',I6,2X,'DATAFN=',8A4,2X,'LSPACE=',I10)
 320    FORMAT(/,10X,'INPUT PARAMETERS PASSED TO ROUTINE URPEAK')
      END IF
C
      MET=1
      ID1=1
cXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
      CALL URPEAK(CHPEAK,IMO,IYR,LMO,LYR,MAXPKS,DATAFN,
     &MET,IBUG,NUMPKS,WORK,ISTAT)
cxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
Call
      LD1=ID1+10*numpks
c
c     IBUG=1
      IF (istat.GE.1) THEN
        IUSEP=0
        IUSEC=0
        WRITE(IPR,721)
  721 FORMAT('0**NOTE** DUE TO WARNINGS ASSOCIATED WITH READING',
     &' THE PEAKFLOW DATA, OPERATION PEAKFLOW IS IGNORED')
        GO TO 405
      END IF
C
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,800)
  800   FORMAT('0** ECHO OUTPUT FROM ROUTINE URPEAK, WHICH ',
     &  'RETURNS THE OBSERVED PEAK FLOW DATA **')
        WRITE(IODBUG,801)NUMPKS,ISTAT,MAXPKS
        WRITE(IODBUG,802)PEAKID,CHPEAK
  802   FORMAT(10X,'PEAKID(2) = ',2A4,10X,'CHPEAK = ',A8)
  801   FORMAT(10X,'NUMPKS,ISTAT,MAXPKS=',2(3X,I4),2X,I10)
      END IF
c
      IF (IBUG.GT.0) THEN
       WRITE(IODBUG,323)
       WRITE(IODBUG,325) ID1,LD1
       WRITE(IODBUG,327) (WORK(I),I=ID1,LD1)
  323  FORMAT('0**WORK() DATA RETURNED FROM ROUTINE URPEAK**')
  325  FORMAT(/,'PEAK DATA, WORK() ARRAY POSITIONS ',I10,
     & ' THRU',I10)
  327  FORMAT(5F10.2)
      END IF
C
C  COPY WORK ARRAY TO SWORK ARRAY TEMPORARILY FOR CHRONOLOGICAL SORTING
C  OF THE ENTIRE OBSERVED PEAK DATA SET STORED IN THE WORK ARRAY.
      DO 620 I=ID1,LD1
        SWORK(I)=WORK(I)
c       WRITE(IODBUG,*) SWORK(I)
  620 CONTINUE
C******************************************************************
C  CALL ROUTINE TO SORT THE WORK ARRAY INTO CHRONOLOGICAL ORDER.
C
      IDIMS=numpks
      IDIM1=10
      IWORD1=1
      IWORD2=3
      ISPTR=0
C
      CALL USORT2(IDIM1,IDIMS,IWORD1,IWORD2,SWORK,WORK,ISPTR,ISTAT)
c
c     IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,819)
        WRITE(IODBUG,820) (WORK(I),I=id1,ld1)
      END IF
  819 FORMAT('0** SORTED OBSERVED PEAK DATA WORK ARRAY **')
  820 FORMAT(10X,5F10.1)
C
C   FILL SCRATCH FILES WITH OBSERVED PEAK DISCHARGE AND STAGE DATA.
cbf each record of scratch file store positions 1-10 the observed
cbf peak data from the input data file.  positions 11-20 store
cbf the corresponding simulated peak event data.
****************************************************************
c open and write scratch file for processing of data
c during the ex47.f routine
c***************************************************************
c  unit 19 used for scratch file
      itunit=19
      if (ibug .gt. 0) then
        write(iodbug,346) filenm,itunit,ierr
  346   format('about to open scratch filenm=',a,' on itunit=',i3,
     &  ' ierr=',i3)
      end if
      call upinio()
      CALL UPOPEN(itunit,filenm,20,fmt,ierr)
      if (ibug .gt. 0) then
        write(iodbug,345) filenm,itunit,ierr
  345   format('just opened scratch filenm=',a,' on itunit=',i3,
     &  ' ierr=',i3)
      end if
      if (ierr .ne. 0) then
        write(ipr,344)ierr, filenm, itunit
  344   format('ierr open scratch file=',i5,' filenm=',a,' itunit=',i3)
      end if
C
      do 501 i=1,numpks
         temp(1)=work((i-1)*10 + 1)
         temp(2)=work((i-1)*10 + 2)
         temp(3)=work((i-1)*10 + 3)
         temp(4)=work((i-1)*10 + 4)
         temp(5)=work((i-1)*10 + 5)
         temp(6)=work((i-1)*10 + 6)
         temp(7)=work((i-1)*10 + 7)
         temp(8)=work((i-1)*10 + 8)
         temp(9)=0.01
         temp(10)=0.01
         temp(11)=0.01
         temp(12)=0.01
         temp(13)=0.01
         temp(14)=0.01
         temp(15)=0.01
         temp(16)=0.01
         temp(17)=0.01
         temp(18)=0.01
         temp(19)=0.01
         temp(20)=0.01
         WRITE (UNIT=ITUNIT,REC=i) temp
c
c       IBUG=1
        IF (IBUG.GT.0) THEN
         read(unit=itunit,rec=i) temp1
         WRITE(IODBUG,685)i
         WRITE(IODBUG,680) (temp1(K),K=1,10)
        END IF
  501 CONTINUE
c
  685 FORMAT('0** CONTENTS OF SCRATCH FILE RECORD number =',i3)
  680 FORMAT(10F10.1)
C
C  FILL PO ARRAY WITH PARAMETERS OF THE PEAKFLOW OPERATION.
      IUSEP=24
      PO(1)=47.01
      DO 690 I=2,6
        K=I-1
        PO(I)=BASNAM(K)
  690 CONTINUE
        do 691 i=1,8
        po(i+16)=DATAFN(i)
  691 continue
      PO(7)=SIMTS(1)
      PO(8)=SIMTS(2)
      PO(9)=STYPE(1)
      PO(10)=IDT+0.01
      PO(11)=PEAKID(1)
      PO(12)=PEAKID(2)
      PO(13)=IWINDOW+0.01
      PO(14)=numpks+0.01
      PO(15)=IDISP+0.01
      PO(16)=MAGFLG+0.01
C
c     IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,402)
        WRITE(IODBUG,403) (PO(I),I=1,12)
        WRITE(IODBUG,404) (PO(I),I=13,IUSEP)
      END IF
 402  FORMAT('0** CONTENTS OF PIN47 PO ARRAY **')
 403  FORMAT(10X,F5.0,2X,5A4,2X,2A4,1X,A4,2X,F5.0,2X,2A4)
 404  FORMAT(10X,4(F7.2,2X),8a4)
C
C   FILL CO ARRAY WITH DEFAULT CARRYOVER, WHERE THE NUMBER OF
C   CARRYOVER PEAKS IS A FUNCTION OF THE SEARCH WINDOW SIZE.
      IUSEC=NEEDC
      NPK=0
      COPKS=2*IWINDOW
      CO(1)=NPK
      CO(2)=0.01
      CO(3)=0.01
      CO(4)=0.01
      CO(5)=0.01
      DO 99 I=1,COPKS
        CO(6+2*(I-1))=-999.
        CO(7+2*(I-1))=-999
 99   CONTINUE
C
c     IBUG=1
      IF (IBUG.GT.0) THEN
        WRITE(IODBUG,401)
        WRITE(IODBUG,400) (CO(I),I=1,20)
      END IF
 401  FORMAT('0** CONTENTS OF PIN47 CO ARRAY **')
 400  FORMAT(10X,5F10.2)
C
 405     IF (ITRACE.EQ.1) WRITE(IPR,410)
 410  FORMAT('0** EXIT PIN47 ROUTINE **')
C
C
      RETURN
      END
