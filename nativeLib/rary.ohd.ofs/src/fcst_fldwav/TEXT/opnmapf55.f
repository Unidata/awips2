C$PRAGMA C (GET_APPS_DEFAULTS)
C$PRAGMA C (CHECK_EXIST)
      SUBROUTINE OPNMAPF55(IOPNMAP,SYSPTH,TWNPTH,K30)
C
C THIS SUBROUTINE OPENS THE FILES NEEDED FOR THE FLDVIEW UTILITY
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C    major changes were made to this routine to 
C    create the appropriate directories for the system names and
C    scenario names, if they don't exist
C

cc      CHARACTER*20 FILNMP
      CHARACTER*20 FILNAM
      CHARACTER*20 FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,FILEFORM_U
      CHARACTER*100 ENVVAR1,ENVVAR2
      CHARACTER*150 FILNMP,FILNMP2,FILNMP3,FILNMP4,FILANIM,PATHMP
      CHARACTER*150 DIRNAME,PATHNAME,UNIXCMD,DIRNAME2,DIRNAME3,DIRNAMEO
      CHARACTER*13 FCST
      CHARACTER*24 SYSPTH(K30),TWNPTH(K30),SYSPATH,TWNPATH

cc      COMMON/FLDMAP55/NMAP,FILANIM,FILNAM,MPTIM
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/opfil55'
      INCLUDE 'common/fldmap55'
      INCLUDE 'common/fprog'

cc      DIMENSION SYSPTH(K30),TWNPTH(K30)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/opnmapf55.f,v $
     . $',                                                             '
     .$Id: opnmapf55.f,v 1.3 2004/10/18 19:06:00 jgofus Exp $
     . $' /
C    ===================================================================
C
C  GET DIRECTORY NAME
      DIRNAME=' '
      LDIRNAME=0
cfan  ENVVAR1='fldview_dir2'
      ENVVAR1='fldview_dir'    !cfan
      LENVVAR1=LENSTR(ENVVAR1)
      CALL GET_APPS_DEFAULTS (ENVVAR1,LENVVAR1,DIRNAME,LDIRNAME)
      IF (LDIRNAME.EQ.0) THEN
         DIRNAME=' '
         ENVVAR2='HOME'
         LENVVAR2=LENSTR(ENVVAR2)
         CALL GET_APPS_DEFAULTS (ENVVAR2,LENVVAR2,DIRNAME,LDIRNAME)
         DIRNAME(LDIRNAME+1:LDIRNAME+1)=' '
         CALL UCNCAT (DIRNAME,'/fldview',IERR)
         CALL ULENTH (DIRNAME,LEN(DIRNAME),LDIRNAME)
         WRITE (IPR,1000) ENVVAR1(1:LENVVAR1),DIRNAME(1:LDIRNAME)
1000  FORMAT ('0**WARNING** FLDVIEW ENVIRONMENT VARIABLE (',A,') NOT ',
     *    'SPECIFIED. FLDVIEW FILES WILL BE WRITTEN TO ',A,'.')
         CALL WARN
      ENDIF
C
C  CREATE DIRECTORY
      UNIXCMD='mkdir -p '//DIRNAME
cc      print '(a150)', unixcmd
      CALL SYSTEM (UNIXCMD)
C
C  CHECK IF DIRECTORY EXISTS
      IPRINT=1
      DIRNAME(LDIRNAME+1:LDIRNAME+1)=CHAR(0)
      CALL CHECK_EXIST (DIRNAME,'directory',IEXIST,IPRINT)
      IF (IEXIST.NE.1) THEN
         CALL UEROR (LPP,1,-1)
         WRITE (LPP,2000) DIRNAME(1:LDIRNAME)
2000  FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
         CALL ERROR
         IOPNMAP=1
      ENDIF
C
      DIRNAMEO=DIRNAME
      LDIRNAMEO=LENSTR(DIRNAMEO)

      IF(IOPNMAP.EQ.1) GO TO 400
C
      IF(MAINUM.NE.2) THEN
        FCST='deterministic'
        LFCST=13
      else
        FCST='probabilistic'
        LFCST=13
      endif

c  add river system name to directory tree

cc      DO 300 LP=1,NMAP
      LP=1
      syspath=' '
      lsyspth=lenstr(syspath)
      PATHMP=' '
cc      dirname=' '
cc      print*, "lp, dirname, syspath, lsyspth, syspth =", lp
cc      print '(a150)', dirname
cc      print '(a24)', syspath
cc      print '(a24)', syspth(lp)
cc      DIRNAME=DIRNAMEO
cc      LDIRNAME=LDIRNAMEO
      SYSPATH=SYSPTH(LP)
      LSYSPTH=LENSTR(SYSPATH)
      DIRNAME2=DIRNAME(1:LDIRNAME)//'/'//SYSPATH(1:LSYSPTH)
      LDIRNAME2=LENSTR(DIRNAME2)
      PATHMP=DIRNAME(1:LDIRNAME)
cc      print*, "dirname, syspath, lsyspth, dirname2="
cc      print '(a150)', dirname
cc      print '(a24)', syspath
cc      print*, lsyspth
cc      print '(a150)', dirname2
cc      print*, "pathmp in opnmapf55"
cc      print '(a150)', pathmp

C  CREATE DIRECTORY
      UNIXCMD='mkdir -p '//DIRNAME2
cc       print '(a150)', unixcmd

C
C  CHECK IF DIRECTORY EXISTS
      IPRINT=1
      DIRNAME2(LDIRNAME2+1:LDIRNAME2+1)=CHAR(0)
      CALL CHECK_EXIST (DIRNAME2,'directory',IEXIST,IPRINT)
cc      print*, "iexist=", iexist
      IF (IEXIST.NE.1) THEN
       CALL SYSTEM (UNIXCMD)
cc         CALL UEROR (LPP,1,-1)
cc         WRITE (LPP,2000) DIRNAME2(1:LDIRNAME2)
cc2000  FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
cc         CALL ERROR
cc         IOPNMAP=1
      ENDIF

      TWNPATH=TWNPTH(LP)
      LTWNPTH=LENSTR(TWNPATH)
      DIRNAME3=DIRNAME2(1:LDIRNAME2)//'/'//TWNPATH(1:LTWNPTH)
      LDIRNAME3=LENSTR(DIRNAME3)
cc      print '(a150)', dirname3

      DIRNAME=DIRNAME3
      LDIRNAME=LDIRNAME3
      dirname3=dirname(1:ldirname)//'/forecastdata/'//fcst(1:lfcst)

C
C  CREATE DIRECTORY
      UNIXCMD='mkdir -p '//DIRNAME3
      CALL SYSTEM (UNIXCMD)
C
C  CHECK IF DIRECTORY EXISTS
      IPRINT=1
      DIRNAME3(LDIRNAME3+1:LDIRNAME3+1)=CHAR(0)
      CALL CHECK_EXIST (DIRNAME3,'directory',IEXIST,IPRINT)
      IF (IEXIST.NE.1) THEN
       CALL SYSTEM (UNIXCMD)
cc         CALL UEROR (LPP,1,-1)
cc         WRITE (LPP,2000) DIRNAME3(1:LDIRNAME)
cc2000  FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
cc         CALL ERROR
cc         IOPNMAP=1
      ENDIF

      FILNAM=' '
      CALL UMEMOV (IDSEGN,FILNAM,2)
      IF (FILNAM.EQ.' ') FILNAM='dummy'
      FILNMP=' '
      FILNMP=DIRNAME(1:LDIRNAME)//'/forecastdata/'//
     .    FILNAM(1:LENSTR(FILNAM))
      LFILNMP=LENSTR(FILNMP)
      FILNMP2=DIRNAME(1:LDIRNAME)//'/forecastdata/'//FCST(1:LFCST)//
     .    '/'//FILNAM(1:LENSTR(FILNAM))
      LFILNMP2=LENSTR(FILNMP2)
cc      FILNMP3=DIRNAME(1:LDIRNAME)//'/forecastdata/segname'
cc      LFILNMP3=LENSTR(FILNMP3)
      FILNMP3=DIRNAMEO(1:LDIRNAMEO)//'/'//FILNAM(1:LENSTR(FILNAM))//
     .         '.segname'
      LFILNMP3=LENSTR(FILNMP3)
      FILNMP4=DIRNAME(1:LDIRNAME)//'/forecastdata'
      LFILNMP4=LENSTR(FILNMP4)
cc      print*, "filnmp, filnmp2, filnmp3, filnmp4, pathname="
cc      print '(a150)', filnmp
cc      print '(a24)', filnmp2

      FILANIM=FILNMP(1:LFILNMP4)//'/animation/'
     . //FILNAM(1:LENSTR(FILNAM))
      LFILANIM=LENSTR(FILANIM)
C
C  CREATE DIRECTORY
      UNIXCMD='mkdir -p '//FILANIM
      CALL SYSTEM (UNIXCMD)
C
C  CHECK IF DIRECTORY EXISTS
      IPRINT=1
      FILANIM(LFILANIM+1:LFILANIM+1)=CHAR(0)
      CALL CHECK_EXIST (FILANIM,'directory',IEXIST,IPRINT)
      IF (IEXIST.NE.1) THEN
       CALL SYSTEM (UNIXCMD)
cc         CALL UEROR (LPP,1,-1)
cc         WRITE (LPP,2000) FILANIM(1:LFILANIM)
cc2000  FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
cc         CALL ERROR
cc         IOPNMAP=1
      ENDIF

C
C  OPEN FILES
C
      FILETYPE='FLDWAV-FLDVIEW '
      FILEACCS='SEQUENTIAL'
      FILESTAT='UNKNOWN'
      FILEFORM_F='FORMATTED'
      FILEFORM_U='UNFORMATTED'
      LRECL=0
C
      PATHNAME=FILNMP(1:LFILNMP)//'.xy'
cc      print*, "lp,pathname=", lp
cc      print '(a150)', pathname

      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFXY,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

      PATHNAME=FILNMP(1:LFILNMP)//'.scn'
cc      print*, "lp,pathname=", lp
cc      print '(a150)', pathname

      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFSCEN,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

      PATHNAME=FILNMP2(1:LFILNMP2)//'.fcs'
      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFPRF,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

      PATHNAME=FILNMP(1:LFILNMP)//'.xsec'
      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFXSEC,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

      PATHNAME=FILNMP3(1:LFILNMP3)
      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFSEG,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)
      write(jfseg,100) filnam
  100 format(a20)
      do 110 ll=1,nmap
        write(jfseg,105) ll,syspth(ll),twnpth(ll)
  105   format(i5,2(1x,a24))
  110 continue

      PATHNAME=FILNMP2(1:LFILNMP2)//'.date'
      CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *   LRECL,JFDAT,IERR)
      IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

      IF(MPTIM.GT.0) THEN
      PATHNAME=FILANIM(1:LFILANIM)//'.date'
        CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *     LRECL,JFDAT2,IERR)
        IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)

        PATHNAME=FILANIM(1:LFILANIM)//'AN000000.fcs'
        CALL OPFILE (PATHNAME,FILETYPE,FILEACCS,FILESTAT,FILEFORM_F,
     *     LRECL,JFANIM,IERR)
        IF (IERR.NE.0) CALL OPNERR55 (PATHNAME,IOPNERR)
      ENDIF
cc  300 CONTINUE
  400 RETURN
      END








