C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE USRNEW
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHANGE THE NAME OF THE FILE SET TO BE PROCESSED.
C
      SUBROUTINE USRNEW (USERN,PATHN,ISTAT)
C
      CHARACTER*(*) USERN,PATHN
      CHARACTER*4 KEY
      CHARACTER*32 INNAME
      CHARACTER*100 VAR_NAME,VAR_VALUE,DIRNAME,PUTSTR
      CHARACTER*128 FILNAM/' '/
      CHARACTER*160 OTNAME
C
      INCLUDE 'uio'
      INCLUDE 'uunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/usrnew.f,v $
     . $',                                                             '
     .$Id: usrnew.f,v 1.3 2002/02/11 20:27:47 dws Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      LDEBUG=0
C
C  GET CURRENT PATHNAME FOR OFS FILES
      VAR_NAME='ofs_fs5files'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *   VAR_VALUE,LENGTH)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' VAR_NAME=',VAR_NAME(1:LENSTR(VAR_NAME)),
     *      ' VAR_VALUE=',VAR_VALUE(1:LENGTH)
         ENDIF
C
C  SET NEW PATHNAME FOR OFS FILES
      VAR_NAME='ofs_files'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *   VAR_VALUE,LENGTH)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' VAR_NAME=',VAR_NAME(1:LENSTR(VAR_NAME)),
     *      ' VAR_VALUE=',VAR_VALUE(1:LENGTH)
         ENDIF
      DIRNAME=VAR_VALUE
      VAR_NAME='ofs_level'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *   VAR_VALUE,LENGTH)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' VAR_NAME=',VAR_NAME(1:LENSTR(VAR_NAME)),
     *      ' VAR_VALUE=',VAR_VALUE(1:LENGTH)
         ENDIF
      VAR_NAME='ofs_fs5files'
      PUTSTR=VAR_NAME(1:LENSTR(VAR_NAME))
     *   //'='
     *   //DIRNAME(1:LENSTR(DIRNAME)-1)
     *   //'/'
     *   //USERN(1:LENSTR(USERN))
     *   //'/'
     *   //'fs5files'
     *   //CHAR(0)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' PUTSTR=',PUTSTR(1:LENSTR(PUTSTR))
         ENDIF
C
C  SET ENVIRONMENT VARIABLE - NEW VALUE ONLY SET FOR ROUTINES CALLED
C  AFTER CALLING PUTENV AND NOT TO ROUTINES CALLED BEFORE
      CALL PUTENV (PUTSTR)
C
      IF (LDEBUG.GT.0) THEN
         VAR_NAME='ofs_fs5files'
         VAR_VALUE=' '
         CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *      VAR_VALUE,LENGTH)
         WRITE (LPD,*) ' VAR_NAME=',VAR_NAME(1:LENSTR(VAR_NAME)),
     *      ' VAR_VALUE=',VAR_VALUE(1:LENGTH)
         ENDIF
C
C  SET NEW USERNAME
      VAR_NAME='ofs_level'
      PUTSTR=VAR_NAME(1:LENSTR(VAR_NAME))
     *   //'='
     *   //USERN(1:LENSTR(USERN))
     *   //CHAR(0)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' PUTSTR=',PUTSTR(1:LENSTR(PUTSTR))
         ENDIF
C
C  SET ENVIRONMENT VARIABLE
      CALL PUTENV (PUTSTR)
C
      VAR_NAME='ofs_fs5files'
      VAR_VALUE=' '
      CALL GET_APPS_DEFAULTS (VAR_NAME,LENSTR(VAR_NAME),
     *   VAR_VALUE,LENGTH)
      IF (LDEBUG.GT.0) THEN
         WRITE (LPD,*) ' VAR_NAME=',VAR_NAME(1:LENSTR(VAR_NAME)),
     *      ' VAR_VALUE=',VAR_VALUE(1:LENGTH)
         ENDIF
      PATHN=VAR_VALUE(1:LENGTH)
C
C  SET NEW OFS DIRECTORY NAME
      KEY='OPNW'
      INNAME='ofs_fs5files'//CHAR(0)
      CALL UPPFIX (KEY,INNAME,OTNAME,LOTNAME)
C
C  GET USERPARM FILNAME
      IUNIT=KUPARM
      CALL UPFNCU ('NORM',' ',IUNIT,FILNAM,IUNITO,LRECL,LBLOCK,IERR)
      IF (IERR.NE.0) THEN
         WRITE (LP,10) 'USERPARM'
10    FORMAT ('0**ERROR** CANNOT OBTAIN FILE NAME FOR ','FILE ',A,'.')
         ISTAT=1
         GO TO 30
         ENDIF
C
C  CHECK IF FILE EXISTS
      IUNIT=0
      CALL UPEXIS (IUNIT,FILNAM,IERR)
      IF (IERR.EQ.-1) THEN
         WRITE (LP,20) 'USERPARM',USERN(1:LENSTR(USERN))
20    FORMAT ('0**ERROR** ',A,' FILE DOES NOT EXIST FOR USER ',A,'.')
         ISTAT=1
         GO TO 30
         ENDIF
C
30    RETURN
C
      END
