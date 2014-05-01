C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CHECK_EXIST)
C MODULE MEBASN
C-----------------------------------------------------------------------
C      
      SUBROUTINE MEBASN (FCSTGNM,BASNAM,BASZONE,DTYPE,DUNIT,DDIMN,
     *      IMO,IYR,LMO,LYR,ARNAME,NSTA,IL,NAREA,MONTH,IYEAR,W,AREAID,
     *      NOOUT,MONUM,IUNIT,ICNT)
C
C  ROUTINE TO COMPUTE AREA AND WRITE TO CALIBRATION DATA FILES
C
      DIMENSION IUNIT(*)
      DIMENSION W(10,25),RMBPE(31)
C
      CHARACTER*12 AREAID(*)
      CHARACTER*20 ARNAME(*)
      CHARACTER*4  DTYPE, DUNIT, DDIMN  
      CHARACTER  FCSTGNM*12, DIRNAM*80, FILE1*100
      CHARACTER  FULLN(10)*112, BASNAM(10)*12, BASZONE(10)*12
      CHARACTER  OFORMAT*12
      CHARACTER*150 PATHNAME,UNIXCMD
C
      INCLUDE 'uiox'
      COMMON /MEDATX/ X(25),Y(25),PTPE(25,31),NWSSTA(25),PXNAME(25),
     *   FE(25),PENORM(25,12),ELEV(25)
      CHARACTER*20 PXNAME
      COMMON /MEAREX/ PEAREA(10,600),MFLAG(600)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/mebasn.f,v $
     . $',                                                             '
     .$Id: mebasn.f,v 1.6 2002/02/11 18:40:40 dws Exp $
     . $' /
C    ===================================================================
C
C
      IUSTOP=0
C
      FLG_M_999=-999.0
      FLG_P_999=+999.0
      FLG_P_998=+998.0
      OFORMAT='(16F6.2)'
C
      ICNT=ICNT+1
C
      DO 90 N=1,NAREA
C     SET FULL PATHNAME FOR OUTPUT FILE.
         IF (ICNT.EQ.1) THEN
            CALL GET_APPS_DEFAULTS ('calb_area_ts_dir',16,DIRNAM,LENGTH)
C        APPEND FORECAST GROUP NAME IF SPECIFIED
            IF (FCSTGNM.EQ.' ') THEN
               FILE1=DIRNAM(1:LENGTH)//
     *               '/'
               ELSE
                  CALL ULENTH (FCSTGNM,12,LEN1)
                  FILE1=DIRNAM(1:LENGTH)//
     *                  '/'//
     *                  FCSTGNM(1:LEN1)//
     *                  '/'
               ENDIF
C        IF A BASIN NAME NOT SPECIFIED TERMINATE PATHNAME AT
C        FORECAST GROUP NAME AND USE AREA ID AS OUTPUT FILE NAME
            CALL ULENTH (AREAID(N),12,LEN5)
            IF (LEN5.EQ.-1) LEN5=12
            IF (BASNAM(N).EQ.' ') THEN
               PATHNAME=FILE1(1:LENSTR(FILE1))
               UNIXCMD='mkdir -p '//PATHNAME
               CALL SYSTEM (UNIXCMD)
               FULLN(N)=PATHNAME(1:LENSTR(PATHNAME))//
     *                  AREAID(N)(1:LEN5)//
     *                  '.'//
     *                  DTYPE
               ELSE
C           IF A BASIN ZONE NOT SPECIFIED APPEND BASIN NAME TO 
C           PATHNAME AND USE BASIN NAME AS OUTPUT FILE NAME
                  CALL ULENTH (BASNAM(N),12,LEN3)
                  IF (LEN3.EQ.-1) LEN3=12
                  IF (BASZONE(N).EQ.' ') THEN
                     PATHNAME=FILE1(1:LENSTR(FILE1))//
     *                        BASNAM(N)(1:LEN3)
                     UNIXCMD='mkdir -p '//PATHNAME
                     CALL SYSTEM (UNIXCMD)
                     FULLN(N)=PATHNAME(1:LENSTR(PATHNAME))//
     *                        '/'//
     *                        BASNAM(N)(1:LEN3)//
     *                        '.'//
     *                        DTYPE
                  ELSE
C              IF A BASIN ZONE IS SPECIFIED APPEND BASIN NAME TO 
C              PATHNAME BUT USE BASIN ZONE AS OUTPUT FILE NAME
                   CALL ULENTH (BASZONE(N),12,LEN4)
                   IF (LEN4.EQ.-1) LEN4=12
                   PATHNAME=FILE1(1:LENSTR(FILE1))//
     *                      BASNAM(N)(1:LEN3)
                   UNIXCMD='mkdir -p '//PATHNAME
                   CALL SYSTEM (UNIXCMD)
                   FULLN(N)=PATHNAME(1:LENSTR(PATHNAME))//
     *                      '/'//
     *                      BASZONE(N)(1:LEN4)//
     *                      '.'//
     *                      DTYPE
                 ENDIF
              ENDIF
C        CHECK IF DIRECTORY EXISTS
            IPRINT=1
            PATHNAME(LENSTR(PATHNAME)+1:LENSTR(PATHNAME)+1)=CHAR(0)
            CALL CHECK_EXIST (PATHNAME,'directory',IEXIST,IPRINT)
            IF (IEXIST.NE.1) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,185) PATHNAME(1:LENSTR(PATHNAME))
185   FORMAT ('0*** ERROR - IN MEBASN - DIRECTORY ',A,' NOT FOUND.')
               CALL USTOP (LP,IUSTOP)
               ENDIF
C       OPEN OUTPUT TIME SERIES FILE
           ITIME=24
           CALL CARDHD (FULLN(N),DTYPE,DUNIT,DDIMN,ITIME,
     *       IMO,IYR,LMO,LYR,
     *       AREAID(N),ARNAME(N),IUNIT(N),OFORMAT,IAM1,IAM2,IERR)
           IF (IERR.GT.0) THEN
             CALL UEROR (LP,0,-1)
             WRITE (LP,190) 'CARDHD',IUNIT(N)
190   FORMAT ('0*** ERROR - IN MEBASN - CALLING ROUTINE ',A,
     *   ' FOR UNIT ',I2,'.')
             CALL USTOP (LP,IUSTOP)
             ENDIF
         ENDIF
         PEAREA(N,MONUM)=0.0
         DO 10 IP=29,31
            RMBPE(IP)=FLG_P_999
10          CONTINUE
C     COMPUTE DAILY AREA PE
         DO 30 IP=1,IL
            RMBPE(IP)=0.0
            DO 20 IRG=1,NSTA
            IF (W(N,IRG).LT.0.001) GO TO 20
C        CALCULATE THE SUM OF WEIGHTED PTPE FOR EACH STATION
            RMBPE(IP)=RMBPE(IP)+PTPE(IRG,IP)*W(N,IRG)
20          CONTINUE
C        COMPUTE MONTHLY TOTALS
            PEAREA(N,MONUM)=PEAREA(N,MONUM)+RMBPE(IP)
30          CONTINUE
         IF (NOOUT.GT.1) GO TO 70
         WRITE (LP,40) AREAID(N),MONTH,IYEAR
40       FORMAT ('0DAY',20X,'MAPE FOR AREA=',A,5X,I2,'/',I4)
         K=1
         L=16
         DO 60 I=1,2
            IF (L.GT.IL) L=IL
            WRITE (LP,50) K,(RMBPE(J),J=K,L)
50          FORMAT (I5,1X,16F6.2)
            K=K+16
            L=L+16
60          CONTINUE
70       IF (NOOUT.LT.1) GO TO 90
            DO 80 I=29,31
               IF (RMBPE(I).GT.FLG_P_998) RMBPE(I)=FLG_M_999
80             CONTINUE
CCC            CALL WTFILE (DTYPE,24,MONTH,IYEAR,1,1,31,RMBPE,IPUNCH)
            CALL CARDWT (DTYPE,24,MONTH,IYEAR,1,RMBPE,IUNIT(N),OFORMAT,
     *         IERR)
            IF (IERR.GT.0) THEN
               CALL UEROR (LP,0,-1)
               WRITE (LP,190) 'CARDWT',IUNIT(N)
               CALL USTOP (LP,IUSTOP)
               ENDIF
90       CONTINUE
C
      RETURN
C
      END
