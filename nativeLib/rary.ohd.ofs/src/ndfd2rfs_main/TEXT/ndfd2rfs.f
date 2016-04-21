C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (SETUPAPI)
C$PRAGMA C (genericprobe)
C23456---1---------2---------3---------4---------5---------6---------712
C-----------------------------------------------------------------------
C
      SUBROUTINE NDFD2RFS_MAIN
C
C-----------------------------------------------------------------------
C
C     FIRST PART get all grids lat,lon for every basin from nwsrfs DB
C     SECOND PART read data for the given lat,lon from NDFD data files
C     THIRD PART calculate the basin average based on the NDFD data
C     FORTH PART write the forcast data into nwsrfs DB for every basin
C
C    Three input data files are:
C      ds.temp.bin: temperature output from NDFD
C                   (every  3 hours, upto  72 hours;
C                    every  6 hours, upto 168 hours)
C      ds.maxt.bin: max temperature output from NDFD
C                   (every 24 hours, upto 168 hours)
C      ds.mint.bin: min temperature output from NDFD
C                   (every 24 hours, upto 168 hours)
C    ===================================================================
C
      PARAMETER (LARRAY=60000)
      PARAMETER (MARRAY=600)
      PARAMETER (NARRAY=60)
      REAL*8    ALAT(LARRAY),ALON(LARRAY)
      REAL*8    FLAT(LARRAY),FLON(LARRAY),DATA(LARRAY,NARRAY)
      REAL      TS0(NARRAY,LARRAY),TS3(NARRAY,LARRAY),TS6(NARRAY,LARRAY)
      CHARACTER*8 BASNID(LARRAY)
      CHARACTER*8 TMPMATID(LARRAY)                        !cfan 2007/09
      CHARACTER*4 TSTYPE/'MAT '/,TSUNIT/'DEGF'/
      INTEGER  NGRID(LARRAY)
      REAL*8    TSTEMP(NARRAY,LARRAY)
      REAL*8    TSMAXT(NARRAY,LARRAY),TSMINT(NARRAY,LARRAY)
      REAL      TS(LARRAY,MARRAY),TSDATA(MARRAY)
      CHARACTER*23 LOGF
      CHARACTER*100 LOGFILE
      CHARACTER*2  IHR1,IMIN1,ISEC1,IHR2,IMIN2,ISEC2
      INTEGER IV1(8),IV2(8), LENTIME1,  LENTIME2, LENTIME3, F_INTERP/0/
      CHARACTER*12  VALTIME1(NARRAY),VALTIME2(NARRAY),VALTIME3(NARRAY)
      CHARACTER*100  LINE,inputfile1,inputfile2,inputfile3
      CHARACTER*100 VAR_NAME,VAR_VALUE
      CHARACTER*12  NDFDBGN,NDFDEND
      COMMON/TIMESTAMP/NDFDBGN,NDFDEND
      COMMON/TMPMATID/TMPMATID                           !cfan 2007/09

      common /CMNDFD2RFS/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_main/RCS/ndfd2rfs.f,v $
     . $',                                                             '
     .$Id: ndfd2rfs.f,v 1.4 2004/11/02 20:16:14 xfan Exp $
     . $' /
C    ===================================================================
C
C

C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      ISTAT=0
      CALL UPINIO()

c      CALL WRITEMSGF(40960+128+0,
c     &         ' NDFD2RFS PREPROCESSOR    VERSION  '//PGMVRN)

      VAR_NAME='ndfd2rfs_output'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)

      CALL DATE_AND_TIME(VALUES=IV1)
      WRITE(IHR1, '(I2.2)')IV1(5)
      WRITE(IMIN1,'(I2.2)')IV1(6)
      WRITE(ISEC1,'(I2.2)')IV1(7)

c      WRITE(LOGF,'(13Hndfd2rfs_log_,3A2)')IHR1,IMIN1,ISEC1
c      WRITE(LOGFILE,'(A,1h/,A)')VAR_VALUE(1:LENGTH),LOGF

C************************************ API message start here

      ILEN=LENGTH+1+13+6
      CALL SETUPAPI(LOGFILE,ILEN,'ndfd2rfs_log_level',18)

      CALL WRITEMSGF(40960+128+0,
     &         ' PGMNAM                   = '//PGMNAM)
      CALL WRITEMSGF(40960+128+0,
     &         ' PGMVRN                   = '//PGMVRN)
      CALL WRITEMSGF(40960+128+0,
     &         ' PGMVRD                   = '//PGMVRD)
      CALL WRITEMSGF(40960+128+0,
     &         ' PGMSYS                   = '//PGMSYS)
      CALL WRITEMSGF(40960+128+0,
     &         ' PGMCMP                   = '//PGMCMP)

C
C  SETUP TOKEN

      VAR_NAME='ofs_level'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)
      CALL WRITEMSGF(40960+128+0,
     &         ' TOKEN ofs_level          = '//VAR_VALUE(1:LENGTH))

      VAR_NAME='ndfd2rfs_log_level'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)
      CALL WRITEMSGF(40960+128+0,
     &         ' TOKEN ndfd2rfs_log_level = '//VAR_VALUE(1:LENGTH))

      VAR_NAME='ndfd2rfs_input'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)
      CALL WRITEMSGF(40960+128+0,
     &         ' TOKEN ndfd2rfs_input     = '//VAR_VALUE(1:LENGTH))

      inputfile1=VAR_VALUE(1:LENGTH)//'/ds.temp.bin'
      inputfile2=VAR_VALUE(1:LENGTH)//'/ds.maxt.bin'
      inputfile3=VAR_VALUE(1:LENGTH)//'/ds.mint.bin'

      VAR_NAME='ndfd2rfs_output'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)
      IF (LENGTH .EQ.0) THEN
      VAR_NAME='ofs_output'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *                        VAR_VALUE,LENGTH)
      ENDIF
      CALL WRITEMSGF(40960+128+0,
     &         ' TOKEN ndfd2rfs_output    = '//VAR_VALUE(1:LENGTH))

c      CALL WRITEMSGF(49152+128+0,
c     &         ' '//LOGFILE(1:ILEN)//'_api opened successfully!')

C
C-----1----- get all grids lat,lon for every basin from nwsrfs DB --------

      CALL WRITEMSGF(16384+128+80,' GETINFO')

      CALL GETINFO(BASNID,NGRID,ALAT,ALON,NALLBASN,NALLGRID)

      CALL WRITEMSGF(24576+128+80,' GETINFO')
C
C-----2----- read data for the given lat,lon from NDFD data files --------

      CALL WRITEMSGF(16384+128+80,' GENERICPROBE')

      IF (F_INTERP .EQ. 0) THEN
c      CALL WRITEMSGF(40960+128+50,
c     &               ' F_INTERP=0, find the nearest neighbor grid!')
      ELSEIF (F_INTERP .EQ. 1) THEN
c      CALL WRITEMSGF(40960+128+50,
c     &               ' F_INTERP=1, perform bi-linear interpolation!')
      ELSE
      CALL WRITEMSGF(40960+384+50,' F_INTERP must equal to 0 or 1')
      STOP 'F_INTERP is wrong!'
      ENDIF

      CALL WRITEMSGF(32768+128+80,' GENERICPROBE 1 for '//inputfile1)

      CALL GENERICPROBE (inputfile1,LENSTR(inputfile1),
     &   NALLGRID, ALAT, ALON, F_INTERP, LENTIME1, VALTIME1, DATA)

      NDFDBGN=VALTIME1(1)
      NDFDEND=VALTIME1(LENTIME1)

        DO I=1,LENTIME1

c        WRITE(LINE,'(4x,A12)') VALTIME1(I)
c        CALL WRITEMSGF(57344+128+80,LINE(1:16))

         DO J=1,NALLGRID
         TSTEMP(I,J)=DATA(J,I)
         ENDDO

c        IF (TSUNIT.EQ.'DEGC') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              (TSTEMP(I,L),L=1,10)
c        ELSEIF(TSUNIT.EQ.'DEGF') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              ((TSTEMP(I,L)-32.)*5./9.,L=1,10)
c        ENDIF
c        CALL WRITEMSGF(57344+128+80,LINE(1:90))

        ENDDO

C      CALL WRITEMSGF(57344+128+0,
C     &               ' NDFD TEMP DATA begins at '//VALTIME1(1))
C      CALL WRITEMSGF(57344+128+0,
C     &               ' NDFD TEMP DATA ends   at '//VALTIME1(LENTIME1))

      CALL WRITEMSGF(32768+128+80,' GENERICPROBE 2 for '//inputfile2)

      CALL GENERICPROBE (inputfile2,LENSTR(inputfile2),
     &   NALLGRID, ALAT, ALON, F_INTERP, LENTIME2, VALTIME2, DATA)

        DO I=1,LENTIME2

c        WRITE(LINE,'(4x,A12)') VALTIME2(I)
c        CALL WRITEMSGF(57344+128+80,LINE(1:16))

         DO J=1,NALLGRID
         TSMAXT(I,J)=DATA(J,I)
         ENDDO

c        IF (TSUNIT.EQ.'DEGC') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              (TSMAXT(I,L),L=1,10)
c        ELSEIF(TSUNIT.EQ.'DEGF') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              ((TSMAXT(I,L)-32.)*5./9.,L=1,10)
c        ENDIF
c        CALL WRITEMSGF(57344+128+80,LINE(1:90))

        ENDDO

C      CALL WRITEMSGF(57344+128+0,
C     &               ' NDFD TMAX DATA begins at '//VALTIME2(1))
C      CALL WRITEMSGF(57344+128+0,
C     &               ' NDFD TMAX DATA ends   at '//VALTIME2(LENTIME2))

      CALL WRITEMSGF(32768+128+80,' GENERICPROBE 3 for '//inputfile3)

      CALL GENERICPROBE (inputfile3,LENSTR(inputfile3),
     &   NALLGRID, ALAT, ALON, F_INTERP, LENTIME3, VALTIME3, DATA)

        DO I=1,LENTIME3

c        WRITE(LINE,'(4x,A12)') VALTIME3(I)
c        CALL WRITEMSGF(57344+128+80,LINE(1:16))

         DO J=1,NALLGRID
         TSMINT(I,J)=DATA(J,I)
         ENDDO

c        IF (TSUNIT.EQ.'DEGC') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              (TSMINT(I,L),L=1,10)
c        ELSEIF(TSUNIT.EQ.'DEGF') THEN
c        WRITE(LINE,'(4X,10F8.2,6H......)')
c     &              ((TSMINT(I,L)-32.)*5./9.,L=1,10)
c        ENDIF
c        CALL WRITEMSGF(57344+128+80,LINE(1:90))

        ENDDO

      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TEMP DATA begins at '//VALTIME1(1)(1:4)
     & //'/'//VALTIME1(1)(5:6)//'/'//VALTIME1(1)(7:8)
     & //'-'//VALTIME1(1)(9:10))
      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TEMP DATA ends   at '//VALTIME1(LENTIME1)(1:4)
     & //'/'//VALTIME1(LENTIME1)(5:6)//'/'//VALTIME1(LENTIME1)(7:8)
     & //'-'//VALTIME1(LENTIME1)(9:10))
      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TMAX DATA begins at '//VALTIME2(1)(1:4)
     & //'/'//VALTIME2(1)(5:6)//'/'//VALTIME2(1)(7:8)
     & //'-'//VALTIME2(1)(9:10))
      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TMAX DATA ends   at '//VALTIME2(LENTIME2)(1:4)
     & //'/'//VALTIME2(LENTIME2)(5:6)//'/'//VALTIME2(LENTIME2)(7:8)
     & //'-'//VALTIME2(LENTIME2)(9:10))
      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TMIN DATA begins at '//VALTIME3(1)(1:4)
     & //'/'//VALTIME3(1)(5:6)//'/'//VALTIME3(1)(7:8)
     & //'-'//VALTIME3(1)(9:10))
      CALL WRITEMSGF(57344+128+0,
     &            ' NDFD TMIN DATA ends   at '//VALTIME3(LENTIME3)(1:4)
     & //'/'//VALTIME3(LENTIME3)(5:6)//'/'//VALTIME3(LENTIME3)(7:8)
     & //'-'//VALTIME3(LENTIME3)(9:10))

      CALL WRITEMSGF(24576+128+80,' GENERICPROBE')
C
C-----3----- calculate the basin average based on the NDFD data --------

      CALL WRITEMSGF(16384+128+80,' CALDATA')

      CALL CALDATA(BASNID,NGRID,
     &     VALTIME1,LENTIME1,TSTEMP,
     &     VALTIME2,LENTIME2,TSMAXT,
     &     VALTIME3,LENTIME3,TSMINT,
     &     NALLBASN,NALLGRID,
     &     KK,IYEAR,JULDAY,IHOUR,TS)

C     WRITE(IUNIT9,'(4H   I,1x,6HBASNID,3x,6HIYEAR=,I4,2x,
C    &     7HJULDAY=,I3,2x,6HIHOUR=,I2)')IYEAR,JULDAY,IHOUR

      CALL WRITEMSGF(24576+128+80,' CALDATA')
C
C-----4----- write the forcast data into nwsrfs DB for every basin --------

      CALL WRITEMSGF(16384+128+80,' RDWTPDB2')

      DO I=1,NALLBASN        !-------------i=1,nallbasn
        DO K=1,KK            !-------------k=1,kk
          TSDATA(K)=TS(I,K)
        ENDDO                !-------------k===========

C       WRITE(IUNIT9,'(I4,1x,A8,30(1x,F8.2))')
C    *               I, BASNID(I),(TSDATA(J),J=1,KK)

CFAN    CALL RDWTPDB2(I,BASNID(I),TSTYPE,TSDATA,KK,
C
C       There is no difference when BASNID is the same as MATID.
C       If the basin is divided into LWR and UPR, it should use
C       MATID with LWR here, not the BASNID.
C
        CALL RDWTPDB2(I,TMPMATID(I),TSTYPE,TSDATA,KK,               !cfan 2007/09
     *               IYEAR,JULDAY,IHOUR,ISTAT)

        IF (ISTAT.EQ.0.OR.ISTAT.EQ.2) THEN
C          PRINT*, 'There was no Error ',ISTAT, 'for BASIN ',BASNID(I),I
          CALL WRITEMSGF(32768+128+80,
     &                   ' There is no Error for BASIN '//BASNID(I))
        ELSE
C          PRINT*, 'There was an Error ',ISTAT, 'for BASIN ',BASNID(I),I
          CALL WRITEMSGF(32768+384+80,
     &                   ' There are Errors for BASIN '//BASNID(I))
        ENDIF
      ENDDO                  !-------------i===========

      CALL WRITEMSGF(24576+128+80,' RDWTPDB2')

1000  CONTINUE
      CALL DATE_AND_TIME(VALUES=IV2)
      WRITE(IHR2, '(I2.2)')IV2(5)
      WRITE(IMIN2,'(I2.2)')IV2(6)
      WRITE(ISEC2,'(I2.2)')IV2(7)

      CALL WRITEMSGF(8192 + 128 + 0,
     &         ' Beginning Time is '//IHR1//':'//IMIN1//':'//ISEC1)
      CALL WRITEMSGF(8192 + 128 + 0,
     &         ' Ending    Time is '//IHR2//':'//IMIN2//':'//ISEC2)

      STOP
      END
