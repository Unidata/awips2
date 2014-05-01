C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CHECK_EXIST)
CC AV-- 6/29/01 this is not c routine C$PRAGMA C (DIRNAME)
C MODULE OPNFIL55
C-----------------------------------------------------------------------
C
      SUBROUTINE OPNCRS55(IOPNERR)

C  THIS ROUTINE OPENS THE FILES NEEDED FOR PROGRAM FLDAT.
C
      CHARACTER*20 FILNAM
      CHARACTER*20 FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,FILEFORM_U
      CHARACTER*100 ENVVAR1,ENVVAR2
      CHARACTER*150 FILNM
      CHARACTER*150 DIRNAME,PATHNAME,UNIXCMD
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fprog'
      INCLUDE 'common/opfil55'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/opncrs55.f,v $
     . $',                                                             '
     .$Id: opncrs55.f,v 1.1 2004/02/02 21:27:42 jgofus Exp $
     . $' /
C    ===================================================================
C
C
C  GET DIRECTORY NAME
      DIRNAME=' '
      ENVVAR1='fldat_dir'
      LENVVAR1=LENSTR(ENVVAR1)
      CALL GET_APPS_DEFAULTS (ENVVAR1,LENVVAR1,DIRNAME,LDIRNAME)
      IF(LDIRNAME.EQ.0) THEN
         DIRNAME=' '
         ENVVAR2='HOME'
         LENVVAR2=LENSTR(ENVVAR2)
         CALL GET_APPS_DEFAULTS (ENVVAR2,LENVVAR2,DIRNAME,LDIRNAME)
         DIRNAME(LDIRNAME+1:LDIRNAME+1)=' '
         CALL UCNCAT (DIRNAME,'/fldat',IERR)
         CALL ULENTH (DIRNAME,LEN(DIRNAME),LDIRNAME)
         WRITE (IPR,10) ENVVAR1(1:LENVVAR1),DIRNAME(1:LDIRNAME)
10    FORMAT ('0**WARNING** FLDAT ENVIRONMENT VARIABLE (',A,') NOT ',
     *    'SPECIFIED. FLDAT FILES WILL BE WRITTEN TO ',A,'.')
         CALL WARN
      ENDIF
C
C  CREATE DIRECTORY
      UNIXCMD='mkdir -p '//DIRNAME
      CALL SYSTEM (UNIXCMD)
C
C  CHECK IF DIRECTORY EXISTS
      IPRINT=1
      DIRNAME(LDIRNAME+1:LDIRNAME+1)=CHAR(0)
      CALL CHECK_EXIST (DIRNAME,'directory',IEXIST,IPRINT)
      IF (IEXIST.NE.1) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,20) DIRNAME(1:LDIRNAME)
20    FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
         CALL ERROR
         IOPNERR=1
         ENDIF
C
      FILNAM=' '
      CALL UMEMOV (IDSEGN,FILNAM,2)
      IF (FILNAM.EQ.' ') FILNAM='dummy'
      FILNM=' '
      FILNM=DIRNAME(1:LDIRNAME)//'/'//FILNAM(1:LENSTR(FILNAM))
      LFILNM=LENSTR(FILNM)
C
C  OPEN FILES
C
      FILETYPE='FLDWAV-FLDAT  '
      FILEACCS='SEQUENTIAL'
      FILESTAT='UNKNOWN'
      FILEFORM_F='FORMATTED'
      FILEFORM_U='UNFORMATTED'
      LRECL=0
C
c ----------------
c open fldat files
c ----------------
      PATHNAME=FILNM(1:LFILNM)//'.crs'
      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFCRS,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)
C
40    RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C      SUBROUTINE OPNERR55 (PATHNAME,IOPNERR)
C
C      CHARACTER*(*) PATHNAME
C      INCLUDE 'common/ionum'
C
C      WRITE (IPR,10) PATHNAME(1:LENSTR(PATHNAME))
C10    FORMAT ('0**ERROR** IN OPNCRS55 - CANNOT OPEN FILE ',A,'.')
C      CALL ERROR
C      IOPNERR=1
C
C      RETURN
C
C      END





