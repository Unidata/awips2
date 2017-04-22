C$PRAGMA C (CHECK_EXIST)

        SUBROUTINE CPYMAP55(IOPNMAP,SYSPTH,TWNPTH,K30) 
C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C     copies the fldview file to locations for all scenarios
C

      CHARACTER*150 PATHMP,DIRO,DIRNAME,UNIXCMD,FILANIM
      CHARACTER*24 SYSPTH(K30),TWNPTH(K30),SYSPATH,TWNPATH
      CHARACTER*20 FILNAM
      CHARACTER*8 SNAME

      INCLUDE 'common/ionum'
      INCLUDE 'common/fldmap55'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/cpymap55.f,v $
     . $',                                                             '
     .$Id: cpymap55.f,v 1.1 2004/09/24 20:52:57 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/ 'CPYMAP55'    /

cc      DIMENSION SYSPTH(K30),TWNPTH(K30)
C
      CALL FPRBUG(SNAME,1,55,IBUG)
 
C
cc      print*, "=== starting cpymp55 ... pathmp  ==="
cc       print '(a150)', pathmp(1:lpth)

cc      print*, "twnpath, diro,pathmp,twnpath,syspath,dirname"
 
      TWNPATH=' '
      TWNPATH=TWNPTH(1)
      LTWNPTH=LENSTR(TWNPATH)
      DIRO=' '
      DIRO=TWNPATH(1:LTWNPTH)
      ldiro=lenstr(diro)
      LPTH=LENSTR(PATHMP)
cc      print*, "=== twnpath ==="
cc       print '(a150)', twnpath(1:ltwnpth)
cc      print*, "=== diro  ==="
cc       print '(a150)', diro(1:ldiro)
cc      print*, "=== pathmp  ==="
cc       print '(a150)', pathmp(1:lpth)
      DO 500 L=1,NMAP
        TWNPATH=TWNPTH(L)
        LTWNPTH=LENSTR(TWNPATH)
cc      print*, "$$$ twnpath $$$"
cc       print '(a150)', twnpath
        SYSPATH=SYSPTH(L)
        LSYSPTH=LENSTR(SYSPATH)
cc      print*, "$$$ syspath $$$"
cc       print '(a150)', syspath
        IF(L.EQ.1) THEN
          LPTH=LENSTR(PATHMP)
          DIRO=PATHMP(1:LPTH)//'/'//SYSPATH(1:LSYSPTH)//'/'
     .                       //TWNPATH(1:LTWNPTH)//'/forecastdata'
          ldiro=lenstr(diro)
cc         print*, "l=", l, " diro"
cc          print '(a150)', diro
        ELSE
          DIRNAME=PATHMP(1:LPTH)//'/'//SYSPATH(1:LSYSPTH)//'/'
     .                          //TWNPATH(1:LTWNPTH)
          LDIRNAME=LENSTR(DIRNAME)
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
           CALL SYSTEM (UNIXCMD)
cc             CALL UEROR (LPP,1,-1)
cc             WRITE (LPP,2000) DIRNAME(1:LDIRNAME)
cc2000         FORMAT ('0**ERROR** DIRECTORY ',A,' NOT FOUND.')
cc             CALL ERROR
cc             IOPNMAP=1
          ENDIF
    
cc          ldirname=lenstr(dirname)
cc          print*, "l=", l, " dirname"
cc          print '(a150)', dirname

C  ...  copy folders to the directories
          UNIXCMD='cp -R -u '//DIRO(1:ldiro)//' '
     .            //DIRNAME(1:ldirname)
cc          print*, "unix command"
cc          print '(a150)', unixcmd
          CALL SYSTEM (UNIXCMD)
         ENDIF
  500 CONTINUE
      RETURN
      END

