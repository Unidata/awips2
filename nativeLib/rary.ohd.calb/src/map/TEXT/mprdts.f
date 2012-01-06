C$PRAGMA C (GET_APPS_DEFAULTS)
CC - AV this is not a c routine C$PRAGMA C (DIRNAME)
C MODULE MPRDTS
C-----------------------------------------------------------------------
C
      SUBROUTINE MPRDTS (RUNITS,BMO,BYR,EMO,EYR,NUM,RSTA,IMOW,IMOS,
     *  TIMEOB,PCORR,IMCOR,IOBCGE,SCORF,MC,NDUMST,ITUNIT)
C
C  THIS ROUTINE READS TIME SERIES DATA FROM THE DATA FILE, APPLIES ANY
C  CORRECTION FACTORS AND WRITE IT TO A SCRATCH FILE.
C
c   bmiss = array of missing data
c
      INTEGER      BMO,BYR,EMO,EYR,RSTA,HR,ROW
      INTEGER      DEBUG,DEBUGA
C
      CHARACTER*4  RUNITS,UNITS
      CHARACTER*4  DTYPE,PTPX
      CHARACTER*12 STAID
      character    FILEN*112,FILEN1*40,OFORMAT*12,dirname*72
      DIMENSION    DATA(744),bmiss(745),NDAYS(12)
      DIMENSION    DESCRP(5)
      DIMENSION    TIMEOB(M1,MC),PCORR(M1,MC),IMCOR(M1,MC)
      DIMENSION    IOBCGE(M1,MC),SCORF(M1,MC)
      DIMENSION    NDUMST(1)
      CHARACTER*8  DUMY
C
      INCLUDE 'common/ionum'
      INCLUDE 'clbcommon/bhtime'
      INCLUDE 'clbcommon/crwctl'
      COMMON /MAP/ DEBUG,DEBUGA
      COMMON /MAP3/ B(745)
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mprdts.f,v $
     . $',                                                             '
     .$Id: mprdts.f,v 1.8 2002/02/11 16:52:41 dws Exp $
     . $' /
C    ===================================================================
C
      DATA PTPX / 'PTPX' /, DTYPE / 'PTPX' /
      DATA WSYM / 'W   ' /, SSYM / 'S   ' /
      DATA NDAYS/31,28,31,30,31,30,31,31,30,31,30,31/
      DATA DUMY / 'DUMMY   ' /, FILEN / ' ' /, FILEN1 / ' ' /
      data bmiss/ 744*999.99, 0.0 /
C
      IF (DEBUG.GT.0.OR.DEBUGA.GT.0) WRITE (IPR,*) 'ENTER MPRDTS'
      IF (DEBUG.GT.0) WRITE (IPR,520) BMO,BYR,EMO,EYR,NUM,RSTA
C
      WRITE (IPR,450)
      ISTOP=0
      ISTOP2=0
      IUSTOP=0
      ISTA=0
      LMOW=IMOS-1
      bmiss(745)=0.0
c
c  read directory name of input data from .Apps_defaults
c
      call get_apps_defaults('calb_sta_ts_dir',15,dirname,lendr)
C
C  READ file name
C
   10 continue
      ISTA=ISTA+1
      IERROR=0
      IF (ISTA.GT.NUM) GO TO 400
      IF (NDUMST(ISTA).EQ.1) GO TO 20
C
      READ(IN,430,END=410) FILEN1
      FILEN=dirname(1:lendr)//'/pcpn/'//FILEN1
      LFILEN=LENSTR(FILEN)+1
      FILEN(LFILEN:LFILEN)=CHAR(0)
      IF (DEBUG.GT.0) WRITE (IPR,540) ISTA
C
20    ROW=1
      N=ISTA
      ICMO=0
C
C  CHECK FOR DUMMY STATION
      IF (NDUMST(ISTA).EQ.1) GO TO 30
      GO TO 120
C
C  WRITE DATA ONTO SCRATCH FILE FOR DUMMY STATION
30    WRITE (IPR,500)  DUMY
C
      NMO=(EMO+EYR*12)-(BMO+BYR*12)+1
      JMO=BMO
      JYR=BYR
      IF (DEBUG.GT.0) WRITE (IPR,530) ISTA,NMO,N,JMO,JYR
C
      DO 110 II=1,NMO
         DO 40 I=1,745
         B(I)=0.0
40       CONTINUE
C     DETERMINE OBSERVATION TIME
         IF (ISTA.GT.RSTA) GO TO 50
            HR=0.0
            GO TO 60
50       HR=TIMEOB((ISTA-RSTA),ROW)
         ICMO=(JMO+JYR*12)-(BMO+BYR*12)+1
         IF (ICMO.LT.IOBCGE((ISTA-RSTA),ROW)) GO TO 60
            ROW=ROW+1
            HR=TIMEOB((ISTA-RSTA),ROW)
c
   60    continue
         B(745)=HR
         IF (ISTA.LE.RSTA) GO TO 80
c
C        DUMMY NONRECORDING STATION
c
            DO 70 I=1,31
               IHR=HR+24*(I-1)
               B(IHR)=999.99
70            CONTINUE
            GO TO 100
c
C     DUMMY RECORDING STATION
c
80       DO 90 I=1,744
            B(I)=999.99
90          CONTINUE
c
100      continue
         CALL UWRITT (ITUNIT,N,B,IERR)
         if(ierr.ne.0) then
            write(ipr,2) ierr
            stop
         end if
c
         N=N+NUM
         JMO=JMO+1
         IF (JMO.LE.12) GO TO 110
            JMO=1
            JYR=JYR+1
110      CONTINUE
      GO TO 10
C
C  CHECK FOR VALID DATA TYPE AND TIME INTERVAL
120   IF (DEBUG.GT.0) WRITE (IPR,550) ISTA,N
C
      IMO=0
      IYR=0
      LMO=0
      LYR=0
C
C  CALL CARDLO TO LOCATE THE TIME SERIES AND SEE IF ANY UNITS CONVERSION
C  IS NEEDED. CARDLO WILL ALSO RETURN THE BEGINNING AND ENDING DATES AND
C  THE DATA FORMAT OF THE INPUT DATA FILE.
      CALL CARDLO(IMO,IYR,LMO,LYR, IM1, IY1, IM2, IY2,
     +            RUNITS, UNITS, FILEN, DTYPE, IT, STAID,
     +            DESCRP, IUNIT, OFORMAT, XMFAC, AFAC, IAM1, IAM2,
     +            IERROR)
      IF (IERROR.EQ.1) GO TO 390
C

      IF (DTYPE.EQ.PTPX) GO TO 145
      WRITE (IPR,490) DTYPE
      WRITE (IPR,460)  FILEN1,DTYPE,IT,staid,DESCRP
      CALL USTOP (IPR,IUSTOP)
C
145   IF (ISTA.LE.RSTA) JT=1
      IF (ISTA.GT.RSTA) JT=24
      IF (IT.EQ.JT) GO TO 147
      WRITE (IPR,470) IT,JT
      WRITE (IPR,460)  FILEN1,DTYPE,IT,staid,DESCRP
      CALL USTOP (IPR,IUSTOP)
c
  147 continue
      WRITE (IPR,480) FILEN1,DTYPE,IT,staid,DESCRP,IMO,IYR,LMO,LYR
      JMO=BMO
      JYR=BYR+1900
C
C  CHECK IF BEGINNING OF TIME SERIES PERIOD OF RECORD IS THE SAME AS
C  DESIRED MAP PERIOD OF RECORD
c  if POR of run begins after beginning of TS data, cardrd will 
c   automatically set file pointer to read the requested first month
c
      NMOI=IMO+IYR*12
      NMOB=BMO+(BYR+1900)*12
      IF (DEBUG.GT.0) WRITE (IPR,560) ISTA,N,NMOI,NMOB,JMO,JYR
C
C  FILL IN MISSING DATA FOR BEGINNING OF PERIOD
c
      IF (NMOI.gt.NMOB) then
         NMOE=EMO+(EYR+1900)*12
         IF (NMOI.GT.NMOE) NMOI=NMOE
         NMO=NMOI-NMOB
         IF (DEBUG.GT.0) WRITE (IPR,570) ISTA,N,NMO,JMO,JYR
c
         DO 160 I=1,NMO
         CALL UWRITT (ITUNIT,N,bmiss,IERR)
         if(ierr.ne.0) then
            write(ipr,3) ierr
            stop
         end if
c
         N=N+NUM
         JMO=JMO+1
         IF (JMO.gt.12) then
            JMO=1
            JYR=JYR+1
         end if
160      CONTINUE
      end if
C
C  READ PRECIPITATION DATA
C  CHECK ENDING OF TIME SERIES PERIOD OF RECORD
c
      NMOL=LMO+LYR*12
      NMOE=EMO+(EYR+1900)*12
      IF (NMOL.GT.NMOE) GO TO 200
         NMO=(LMO+LYR*12)-(JMO+JYR*12)+1
         GO TO 210
C
200   NMO=(EMO+(EYR+1900)*12)-(JMO+JYR*12)+1
C
210   IF (DEBUG.GT.0) WRITE (IPR,590) ISTA,N,NMOL,NMOE,NMO,JMO,JYR
      IF (NMO.EQ.0) GO TO 390
C
      CFS=1.0
      CFW=1.0
C
      DO 360 II=1,NMO
         DO 220 I=1,745
            B(I)=0.0
220         CONTINUE
         IF (ISTA.GT.RSTA) GO TO 230
            HR=0.0
            GO TO 240
230      HR=TIMEOB((ISTA-RSTA),ROW)
         ICMO=(JMO+JYR*12)-(BMO+(BYR+1900)*12)+1
         IF (ICMO.LT.IOBCGE((ISTA-RSTA),ROW)) GO TO 240
            ROW=ROW+1
            HR=TIMEOB((ISTA-RSTA),ROW)
240      B(745)=HR
         IF (DEBUG.GT.1) WRITE (IPR,600) JMO,JYR,ROW,ICMO,HR
         IF (JMO.NE.2) GO TO 250
            KYR=JYR/4*4
            IF (JYR.EQ.KYR) NDAYS(2)=29
250      NDAY=NDAYS(JMO)
         NDAYS(2)=28
         IF (IT.EQ.1) GO TO 270
c
C     READ DAILY DATA
c
         CALL CARDRD (1,IUNIT,OFORMAT,IAM1,IAM2,XMFAC,AFAC,IT,
     +               JMO,JYR,DATA,1,IERROR)
         if(ierror.ne.0) then
            write(ipr,1) FILEN,jmo,jyr
            stop
         end if
c
         DO 260 I=1,NDAY
         MOHR=HR+24*(I-1)
         B(MOHR)=DATA(I)
260      CONTINUE
         GO TO 290
c
C     READ HOURLY DATA
c
  270    continue
         CALL CARDRD (1,IUNIT,OFORMAT,IAM1,IAM2,XMFAC,AFAC,IT,
     +               JMO,JYR,DATA,1,IERROR)
         if(ierror.ne.0) then
            write(ipr,1) jmo,jyr
            stop
         end if
c
         NHRS=NDAY*24
         DO 280 I=1,NHRS
            B(I)=DATA(I)
280         CONTINUE
C     CHECK FOR MISSING OR ACCUMULATED VALUES
290      DO 300 I=1,744
            IF (B(I).GE.-999.5.AND.B(I).LT.-998.5) B(I)=999.99
            IF (B(I).GE.-998.5.AND.B(I).LT.-997.5) B(I)=999.98
            IF (B(I).LT.0.0) WRITE (IPR,420) DTYPE,STAID,IT,JMO,JYR,I,
     *         B(I)
300         CONTINUE
c
c   BMO,BYR = beginning month,year (2 digits) of POR of run as read from card A
c   JMO,JYR = last month,year (4 digits) of TS data read
c   IMON = difference in months bet beginning of POR of run and last
c          month,year of TS data read
c   IMCOR = difference in months bet beginning of POR of run and beginning
c           of precip correction factor
c           defined in mapmn2
c         = (YR*12+MO) - (BYR*12+BMO) + 1
c           where MO,YR read from card O fields 3,4
c   PRCF = precip correction factor
c   CFW = precip correction factor for winter
c   CFS = precip correction factor for summer
c   IMOS = month number of first summer month
c   IMOW = month number of first winter month
c
         IMON=(JYR*12+JMO)-((BYR+1900)*12+BMO)+1
         DO 330 I=1,10
            IF (IMON.ge.IMCOR(ISTA,I)) then
               IF (SCORF(ISTA,I).NE.WSYM) GO TO 310
                  CFW=PCORR(ISTA,I)
                  GO TO 330
310            IF (SCORF(ISTA,I).NE.SSYM) GO TO 320
                  CFS=PCORR(ISTA,I)
                  GO TO 330
320            CFW=PCORR(ISTA,I)
               CFS=PCORR(ISTA,I)
            end if
330      CONTINUE
c
c   determine which precip correction factor to use
c   (winter value or summer value)
c
         PRCF=CFW
         IF (JMO.GT.LMOW.AND.JMO.LT.IMOW) PRCF=CFS
c
c   apply precip correction factor
c
         IF (PRCF.ne.1.0) then
         DO 340 I=1,744
            IF (B(I).GT.999.75) GO TO 340
            B(I)=B(I)*PRCF
340         CONTINUE
         end if 
c
c   write corrected B array to scratch file
c
         CALL UWRITT (ITUNIT,N,B,IERR)
         if(ierr.ne.0) then
            write(ipr,4) ierr
            stop
         end if
c
         N=N+NUM
         JMO=JMO+1
         IF (JMO.LE.12) GO TO 360
            JMO=1
            JYR=JYR+1
360      CONTINUE
C
      IF (NMOL.GE.NMOE) GO TO 390
C
C  TIME SERIES PERIOD OF RECORD ENDS BEFORE MAP PERIOD REQUESTED - FILL
C  IN MISSING DATA
      NMOJ=JMO+JYR*12
      NMO=NMOE-NMOJ+1
      IF (DEBUG.GT.0) WRITE (IPR,610) ISTA,N,NMOJ,NMO,JMO,JYR
      DO 370 I=1,744
370   B(I)=999.99
      B(745)=0.0
c
      DO 380 I=1,NMO
         CALL UWRITT (ITUNIT,N,B,IERR)
         if(ierr.ne.0) then
            write(ipr,5) ierr
            stop
         end if
c
         N=N+NUM
380      CONTINUE
C
390   IF (IERROR.EQ.1) ISTOP2=1
      CALL CCLOSL
      GO TO 10
C
400   IF (ISTOP2.EQ.1) CALL USTOP (IPR,IUSTOP)
      RETURN
C
410   WRITE (IPR,510) ISTA,NUM
      CALL USTOP (IPR,IUSTOP)
C
      RETURN
C
    1 format(/,2x,'*** error reading input time series for',1x,
     *  'file=',a,/,3x,
     *  'month=',i2,2x,'year=',i2,2x,'-- program stopping ***')
    2 format(/,2x,'*** error number = ',i1,' writing to ',
     *  1x,'scratch file near statement 100 in MPRDTS')
    3 format(/,2x,'*** error number = ',i1,' writing to ',
     *  1x,'scratch file near statement 160 in MPRDTS')
    4 format(/,2x,'*** error number = ',i1,' writing to ',
     *  1x,'scratch file near statement 360 in MPRDTS')
    5 format(/,2x,'*** error number = ',i1,' writing to ',
     *  1x,'scratch file near statement 380 in MPRDTS')
420   FORMAT (' *** WARNING - ',A4,' VALUE IS LESS THAN 0 ',
     *   'FOR STATION ',A12,'. TIME INT=',I2,1X,'MONTH=',I2,1X,
     *   'YEAR=',I4,1X,'HOUR=',I3,1X,'VALUE=',G12.3)
430   FORMAT (A40)
450   FORMAT (// 28X,'*** TIME SERIES INPUT DATA ***' //
     *  2x,'FILENAME',34x,'TYPE',2x,'DUR',2x,'IDENTIFIER',3x,
     *  'DESCRIPTION',18x,'PERIOD OF RECORD'  /)
460   FORMAT (2X,A,1x,A4,1x,I2,1X,A,1x,5A4)
470   FORMAT (  63H0*** ERROR - DATA TIME INTERVAL OF THE FOLLOWING TIME
     * SERIES  (,I2,10H)  IS NOT ,I2,10H  HOUR(S).    )
480   FORMAT (2X,A,2x,A4,2x,I2,2X,A,2x,5A4,9X,
     *   I2.2,'/',I4,' THRU ',I2.2,'/',I4)
490   FORMAT  (  24H0*** ERROR - DATA TYPE  ,A4,33H  IS NOT ALLOWABLE IN
     *PUT TO MAP.   )
500   FORMAT (1H0,T17,A)
510   FORMAT (    58H0*** ERROR - THE NUMBER OF TIME SERIES HEADER CARDS
     * READ (  ,I3,1H)  /  T14,61HDOES NOT EQUAL THE TOTAL NUMBER OF STA
     *TIONS INPUT ON CARD D (  ,I3,2H).   )
520   FORMAT ( 8H0TRACE 1 ,9I6  )
530   FORMAT ( 8H0TRACE 3 ,9I6  )
540   FORMAT ( 8H0TRACE 2 ,9I6  )
550   FORMAT ( 8H0TRACE 4 ,9I6  )
560   FORMAT ( 8H0TRACE 5 ,9I6  )
570   FORMAT ( 8H0TRACE 6 ,9I6  )
590   FORMAT ( 8H0TRACE 8 ,9I6  )
600   FORMAT ( 9H0TRACE 9 ,9I6  )
610   FORMAT ( 9H0TRACE 10 ,9I6  )
C
      END
