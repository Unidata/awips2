C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CHECK_EXIST)
c - fan C$PRAGMA C (DIRNAME)
C MODULE SXHRTM
C-----------------------------------------------------------------------
C
      SUBROUTINE SXHRTM (FILEN,BN,FN,DTYPE,DUNITO,DDIMN,IMO,IYR,
     *   LMO,LYR,ANAME,NSTA,IL,NAREAS,MONTH,IYEAR,TMM,W,AREAID,
     *   NOOUT,IPUNCH,UNITI,UNITO,BATAMY,IUNIT,ICNT,
     *   MXSTA,MXAREA,MXYEAR,TSTA,DUNITI)
C
C  ROUTINE TO COMPUTE 6-HOUR STATION TEMPERATURE FROM MAX-MIN VALUES AND
C  COMPUTE AREAL TEMPERATURE.
C
      INCLUDE 'uiox'
      INCLUDE 'clbcommon/crwctl'
C
      CHARACTER*4 DTYPE,UNITI,UNITO,DUNITI,DUNITO
      CHARACTER*12 FILEN,AREAID(MXAREA)
      CHARACTER*20 ANAME(MXAREA)
      CHARACTER*150 PATHNAME,UNIXCMD
      CHARACTER*12 BN(MXAREA)
      CHARACTER*12 FN(MXAREA)
      CHARACTER DIRNAME*80,FCSTGNM*12,FILE1*100
      CHARACTER OFORMAT*12
      CHARACTER*112 FULLN/' '/
C
      DIMENSION BATAMY(MXAREA,12,MXYEAR)
      DIMENSION IUNIT(MXAREA)
      DIMENSION W(MXAREA,MXSTA)
      DIMENSION TMM(MXSTA,66),TSTA(MXSTA,124)
      DIMENSION TAREA(124)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/sxhrtm.f,v $
     . $',                                                             '
     .$Id: sxhrtm.f,v 1.8 2002/02/11 18:57:46 dws Exp $
     . $' /
C    ===================================================================
C
C
      LDEBUG=0
C
      IUSTOP=0
C
      DTYPE='MAT'
      OFORMAT='(6F9.3)'
C
C  INCREMENT THE NUMBER OF TIMES THIS ROUTINE HAS BEEN CALLED
      ICNT=ICNT+1
C
C  SET STOP INDICATOR FOR CALIBRATION READ/WRITE ROUTINES
      ISTOP=0
C
      DO 70 IRG=1,NSTA
         ITMM=2
         DO 60 IP=1,IL
            ID=(IP-1)/4
            IP6=IP-(ID*4)
            GO TO (10,20,30,40),IP6
C        MIDNITE TO 6 AM
10          ITMM=ITMM+1
C        ITMM IS MINIMUM
            TSTA(IRG,IP)=0.95*TMM(IRG,ITMM)+0.05*TMM(IRG,(ITMM-1))
            IF (TMM(IRG,(ITMM-1)).GT.900.0) TSTA(IRG,IP)=999.0
            GO TO 50
C        6 AM TO NOON
20          TSTA(IRG,IP)=0.40*TMM(IRG,ITMM)+0.60*TMM(IRG,(ITMM+1))
            GO TO 50
C        NOON TO 6 PM
30          ITMM=ITMM+1
C        ITMM IS MAXIMUM
            TSTA(IRG,IP)=0.025*TMM(IRG,(ITMM-1))+
     *                   0.925*TMM(IRG,ITMM)+
     *                   0.050*TMM(IRG,(ITMM+1))
            IF ((TMM(IRG,(ITMM-1)).GT.900.0).OR.
     *          (TMM(IRG,(ITMM+1)).GT.900.0)) TSTA(IRG,IP)=999.0
            GO TO 50
C        6 PM TO MIDNITE
40          TSTA(IRG,IP)=0.33*TMM(IRG,ITMM)+0.67*TMM(IRG,(ITMM+1))
50          IF (TSTA(IRG,IP).GT.200.0) TSTA(IRG,IP)=999.0
60          CONTINUE
70       CONTINUE
C
C  COMPUTE 6 HOUR MAT
      DO 200 N=1,NAREAS
         IF (ICNT.EQ.1) THEN
C        GET DIRECTORY NAME FOR OUTPUT DATA FROM .APPS_DEFAULTS
            CALL GET_APPS_DEFAULTS ('calb_area_ts_dir',16,DIRNAME,LENDR)
            FCSTGNM=FILEN
C        CONSTRUCT FULL PATHNAME FOR OUTPUT FILE
            IF (FCSTGNM(1:1).EQ.' '.AND.FCSTGNM(2:2).EQ.' ') THEN
               FILE1=DIRNAME(1:LENDR)//'/'
               ELSE
                  LEN=INDEX(FCSTGNM,' ')-1
                  FILE1=DIRNAME(1:LENDR)//'/'//FCSTGNM(1:LEN)//'/'
               ENDIF
            L1=INDEX(FILE1,' ')-1
            L2=INDEX(BN(N),' ')-1
            L3=INDEX(FN(N),' ')-1
            IF (BN(N).EQ.' ') THEN
               L2=0
               ELSE
                  IF (L2.EQ.-1) L2=12
               ENDIF
            IF (FN(N).EQ.' ') THEN
               L3=0
               ELSE
                  IF (L3.EQ.-1) L3=12
               ENDIF
            IF (L1.GT.0.AND.L2.GT.0.AND.L3.GT.0) THEN
               PATHNAME=FILE1(1:L1)//BN(N)(1:L2)
               UNIXCMD='mkdir -p '//PATHNAME
               CALL SYSTEM (UNIXCMD)
               FULLN=PATHNAME(1:LENSTR(PATHNAME))//
     *               '/'//
     *               FN(N)(1:L3)//
     *               '.MAT'
               ENDIF
            IF (L1.GT.0.AND.L2.GT.0.AND.L3.EQ.0) THEN
               PATHNAME=FILE1(1:L1)
               UNIXCMD='mkdir -p '//PATHNAME
               CALL SYSTEM (UNIXCMD)
               FULLN=PATHNAME(1:LENSTR(PATHNAME))//
     *               BN(N)(1:L2)//
     *               '.MAT'
               ENDIF
            IF (L1.GT.0.AND.L2.EQ.0.AND.L3.EQ.0) THEN
               PATHNAME=FILE1(1:L1)
               UNIXCMD='mkdir -p '//PATHNAME
               CALL SYSTEM (UNIXCMD)
               L4=LENSTR(AREAID(N))
               FULLN=PATHNAME(1:LENSTR(PATHNAME))//
     *               AREAID(N)(1:L4)//
     *               '.MAT'
               ENDIF
C        CHECK IF DIRECTORY EXISTS
            IPRINT=1
            PATHNAME(LENSTR(PATHNAME)+1:LENSTR(PATHNAME)+1)=CHAR(0)
            CALL CHECK_EXIST (PATHNAME,'directory',IEXIST,IPRINT)
            IF (IEXIST.NE.1) THEN
               CALL UEROR (LP,1,-1)
               WRITE (LP,255) PATHNAME(1:LENSTR(PATHNAME))
               CALL USTOP (LP,IUSTOP)
               ENDIF
            ITIME=6
C        WRITE DATACARD FILE HEADER RECORDS
            CALL CARDHD (FULLN,DTYPE,DUNITO,DDIMN,ITIME,
     *         IMO,IYR,LMO,LYR,
     *         AREAID(N),ANAME(N),IUNIT(N),OFORMAT,
     *         IAM1,IAM2,IERR)
            IF (IERR.NE.0) THEN
               CALL UEROR (LP,1,-1)
               WRITE (LP,260) 'CARDHD',IUNIT(N)
               CALL USTOP (LP,IUSTOP)
               ENDIF
            ENDIF
C    INSERT MISSING VALUES FOR EACH PERIOD PAST 28 DAYS
        DO 80 IP=113,124
           TAREA(IP)=999.0
80         CONTINUE
C    INITIALIZE COUNT OF PERIODS IN MONTH AND ACCUMULATION OF AREA
        IPGOOD=0
        ACCUM=0.0
        DO 100 IP=1,IL
          TAREA(IP)=0.0
          DO 90 IRG=1,NSTA
C         CHECK IF STATION HAS WEIGHT
            IF (W(N,IRG).GE.0.001) THEN
C           CHECK IF TEMP MISSING
              IF (TSTA(IRG,IP).GT.998.0) THEN
                ID=(IP-1)/4+1
                IP6=IP-(ID-1)*4
                WRITE(LP,210) ID,IP6
                TAREA(IP)=999.0
                GO TO 100
              ELSE
C             ADD WEIGHTED STATION VALUE TO MEAN FOR AREA
                TAREA(IP)=TAREA(IP)+TSTA(IRG,IP)*W(N,IRG)
                IF (LDEBUG.GT.0) THEN
                   WRITE (LP,220) IRG,IP,TSTA(IRG,IP),W(N,IRG),
     *                TAREA(IP)
                   ENDIF
              ENDIF
            ENDIF
90        CONTINUE
C       KEEP COUNT OF NON-MISSING PERIODS (MAX 124); ACCUMULATE TAREA'S
        IPGOOD=IPGOOD+1
        ACCUM=ACCUM+TAREA(IP)
100     CONTINUE
C
C FIND RELATIVE VALUE OF CURRENT YEAR (TO BE YEAR DIMENSION OF BATAMY)
C   I.E., IF YEAR IS 1987 IN PERIOD 1980 TO 1990:
C   RELATIVE YEAR(NYEAR) = CURRENT YEAR(IYEAR) - INITIAL YEAR(IYR) + 1
C   8 = 1987 - 1980 + 1
      NYEAR=IYEAR-IYR+1
C VALUE OF BATAMY FOR THIS MONTH = ACCUMULATION / NUMBER OF GOOD PERIODS
      IF (IPGOOD.GT.0) BATAMY(N,MONTH,NYEAR)=ACCUM/IPGOOD
C
C IF OUTPUT IS ONLY A FILE OR IS NOTHING (NOOUT=2,3) PRINT NO MESSAGES
      IF (NOOUT.GT.1) GO TO 150
      WRITE (LP,230) AREAID(N),MONTH,IYEAR,DUNITI
      WRITE (LP,240)
      IDA=IL/4
      IDL=IDA/5
      IF (IDA.LT.30)IDL=5
      DO 110 J=1,IDL
        I4=(J-1)*20+1
        I5=I4+19
        IDAY=(J-1)*5+1
        WRITE(LP,250) IDAY,(TAREA(I),I=I4,I5)
110   CONTINUE
      IF (IDA-30)120,150,130
120   I4=101
      I5=112
      IDAY=26
      IF(IDA.EQ.29) I5=116
      GO TO 140
130   I4=121
      I5=124
      IDAY=31
140   WRITE (LP,250) IDAY,(TAREA(I),I=I4,I5)
C
150   IF (NOOUT.EQ.1.OR.NOOUT.EQ.3) GO TO 200
C
C  CHECK IF NEED TO CONVERT DATA VALUES
      IF (UNITI.NE.UNITO) THEN
         ICONV=1
         NVAL=1
         IF (UNITI.EQ.'METR'.AND.UNITO.EQ.'ENGL') GO TO 170
C        INPUT UNITS ENGLISH AND OUTPUT UNITS METRIC
         DO 160 I=1,124
            IF (TAREA(I).EQ.999.) GO TO 160
            CALL UDUCNV ('DEGF','DEGC',ICONV,NVAL,TAREA(I),TAREA(I),
     *         IERR)
160         CONTINUE
         GO TO 190
C      INPUT UNITS METRIC AND OUTPUT UNITS ENGLISH
170      DO 180 I=1,124
            IF (TAREA(I).EQ.999.) GO TO 180
            CALL UDUCNV ('DEGC','DEGF',ICONV,NVAL,TAREA(I),TAREA(I),
     *         IERR)
180         CONTINUE
         ENDIF
C     WRITE DATA TO OUTPUT FILE
CCC         CALL WTFILE (DTYPE,6,MONTH,IYEAR,1,1,124,TAREA,IPUNCH)
190      CALL CARDWT (DTYPE,6,MONTH,IYEAR,1,TAREA,IUNIT(N),OFORMAT,
     *      IERR)
         IF (IERR.NE.0) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,260) 'CARDWT',IUNIT(N)
            CALL USTOP (LP,IUSTOP)
            ENDIF
200      CONTINUE
C
      RETURN
C
C-----------------------------------------------------------------------
C
210   FORMAT (' DATA MISSING--DAY=',I2,'  PERIOD=',I1)
220   FORMAT (' IRG=',I3,' IP=',I3,
     *   ' TSTA(IRG,IP)=',F6.2,
     *   ' W(N,IRG)=',F6.3,
     *   ' TAREA(IP)=',F6.2)
230   FORMAT ('0',10X,'MAT FOR:',2X,
     *   'AREA=',A,2X,
     *   'DATE=',I2.2,'/',I4,2X,
     *   'UNITS=',A)
240   FORMAT ('  DAY')
250   FORMAT (I5,1X,5(4F5.0,' / '))
255   FORMAT ('0*** ERROR - IN SXHRTM - DIRECTORY ',A,' NOT FOUND.')
260   FORMAT ('0*** ERROR - IN SXHRTM - CALLING ROUTINE ',A,
     *   ' FOR UNIT ',I2,'.')
C
      END
