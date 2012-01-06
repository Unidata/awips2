C MODULE SUDRVR
C-----------------------------------------------------------------------
C
C  DRIVER ROUTINE FOR PROGRAM PPINIT.
C
      SUBROUTINE SUDRVR (LPMFLD,PMFLD,LARRAY,ARRAY,ISTAT)
C
      CHARACTER*8 PRMS(8)
     *    /'TIMR    ','DBUG    ','TRCE    ','CONDCODE',
     *     'PROMPT  ','TSO     ','RGNWRN  ','        '/
      CHARACTER*20 CHK
      CHARACTER*100 PMFLD,CHAR,CHAR2
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ufreex'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/supagx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sutmrx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/sudrvr.f,v $
     . $',                                                             '
     .$Id: sudrvr.f,v 1.4 2001/06/13 14:04:37 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUDRVR'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG CODE
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
      NUMERR=0
      NUMWRN=0
C
      NTIMR=0
      ISTRCE=0
      ISALL=0
C
      MPRMS=8
C
      IRPRM=1
C
C  CHECK PARAMETER FIELD
      IF (LDEBUG.GT.0.AND.PMFLD.NE.' ') THEN
         WRITE (IOSDBG,220) PMFLD
         CALL SULINE (IOSDBG,2)
         ENDIF
      NFLD=(LPMFLD+3)/4
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,230) LPMFLD,NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (LPMFLD.EQ.0) GO TO 180
C
      IF (IRPRM.EQ.0) GO TO 180
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PARSE PARM FIELD
C
C  UNPACK PARM FIELD
      CALL UNPAKS (PMFLD,ICDBUF,25,100,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,240) IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  FIND FIELDS
      CALL UFFIND (1,LPMFLD,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,250)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  PROCESS EACH FIELD
      DO 170 IFIELD=1,NFIELD
         IF (IFSTOP(IFIELD).LT.IFSTRT(IFIELD)) THEN
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,260) IFIELD
               CALL SULINE (IOSDBG,1)
               ENDIF
            GO TO 170
            ENDIF
C     SET NUMBER OF CHARACTERS
         NCHAR=IFSTOP(IFIELD)-IFSTRT(IFIELD)+1
         CHAR=' '
         CHAR2=' '
         CALL UPACK1 (ICDBUF(IFSTRT(IFIELD):IFSTRT(IFIELD)),CHAR,NCHAR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,270) NFIELD,IFIELD,
     *         IFSTRT(IFIELD),IFSTOP(IFIELD)
            CALL SULINE (IOSDBG,1)
            WRITE (IOSDBG,280) NCHAR,CHAR
            CALL SULINE (IOSDBG,1)
            ENDIF
         NCHR=NCHAR
10       CALL UINDEX (CHAR,NCHR,'/',1,LSLASH)
         IF (LSLASH.GT.0) NCHR=LSLASH-1
         CALL SUBSTR (CHAR,1,NCHR,CHAR2,-1)
         CALL UINDEX (CHAR2,NCHR,'(',1,LLPAR)
         CALL UINDEX (CHAR2,NCHR,')',1,LRPAR)
C     CHECK FOR OPTION
         DO 20 IPRM=1,MPRMS
            CHK=' '
            IF (LLPAR.GT.0) CALL SUBSTR (CHAR2,1,LLPAR-1,CHK,1)
            IF (LLPAR.EQ.0) CALL SUBSTR (CHAR2,1,NCHR,CHK,1)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,290) IFIELD,CHK
               CALL SULINE (IOSDBG,1)
               ENDIF
            NCHK=LEN(PRMS(1))/4
            CALL SUCOMP (NCHK,CHK,PRMS(IPRM),IMATCH)
            IF (IMATCH.EQ.1) GO TO 30
20          CONTINUE
         IF (IFIELD.LE.10) GO TO 160
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,300) IFIELD,CHK
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 160
30       GO TO (40,50,70,90,140,150),IPRM
         GO TO 160
C     TIMR OPTION
40       NTIMR=1
         IF (NPSPAG.EQ.0) CALL SUPAGE
         WRITE (LP,310)
         CALL SULINE (LP,1)
         GO TO 160
C     DEBUG OPTION
50       IF (LLPAR.EQ.0) THEN
            LDBG=99
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,350) PRMS(IPRM),LDBG
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 60
            ENDIF
         IF (LRPAR.EQ.0) THEN
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,360) IFIELD
            CALL SULINE (LP,2)
            LRPAR=NCHR+1
            ENDIF
         CALL UFINFX (LDBG,IFSTRT(IFIELD),LLPAR+1,LRPAR-1,IERR)
         IF (IERR.GT.0) LDBG=99
60       IHCLDB=LDBG
         IPRDB=LDBG
         IPDDB=LDBG
         IPPDB=LDBG
         IUTLDB=LDBG
         IDEDB=LDBG
         ISALL=LDBG
         LDEBUG=LDBG
         ISDBGL=LDBG
         IF (NPSPAG.EQ.0) CALL SUPAGE
         WRITE (LP,320) LDBG
         CALL SULINE (LP,1)
         NPARM=(LPMFLD+3)/4
         WRITE (LP,330) PMFLD
         CALL SULINE (LP,1)
         GO TO 160
C     TRACE OPTION
70       IF (LLPAR.EQ.0) THEN
            LTRC=99
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,350) PRMS(IPRM),LTRC
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 80
            ENDIF
         IF (LRPAR.EQ.0) THEN
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,360) IFIELD
            CALL SULINE (LP,2)
            LRPAR=NCHR+1
            ENDIF
         CALL UFINFX (LTRC,IFSTRT(IFIELD),LLPAR+1,LRPAR-1,IERR)
         IF (IERR.GT.0) LTRC=99
80       IHCLTR=LTRC
         IPRTR=LTRC
         IPDTR=LTRC
         IPPTR=LTRC
         IUTLTR=LTRC
         IDETR=LTRC
         ISTRCE=LTRC
         IF (NPSPAG.EQ.0) CALL SUPAGE
         WRITE (LP,340) LTRC
         CALL SULINE (LP,1)
         GO TO 160
C     CONDCODE OPTION
90       IF (LLPAR.GT.0) GO TO 100
            LCOND=-1
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,350) PRMS(IPRM),LCOND
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 120
100      IF (LRPAR.GT.0) GO TO 110
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,360) IFIELD
            CALL SULINE (LP,2)
            LRPAR=NCHR+1
110      CALL UFINFX (LCOND,IFSTRT(IFIELD),LLPAR+1,LRPAR-1,IERR)
         IF (IERR.GT.0) LCOND=-1
120      IF (LCOND.GE.0.AND.LCOND.LE.16) GO TO 130
            IF (NPSPAG.EQ.0) CALL SUPAGE
            WRITE (LP,370) LCOND
            CALL SUERRS (LP,2,NUMERR)
            GO TO 160
130      IOPCND=LCOND
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,390) IOPCND
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 160
C     PROMPT OPTION
140      GO TO 160
C     TSO OPTION
150      IOPOVP=0
         IOPNWP=0
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,380)
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 160
160      IF (LSLASH.GT.0) THEN
            IFSTRT(IFIELD)=IFSTRT(IFIELD)+LSLASH
            IBEG=LSLASH+1
            NCHR=NCHAR-LSLASH
            CHAR2=' '
            CALL SUBSTR (CHAR,IBEG,NCHR,CHAR2,-1)
            CHAR=' '
            CALL SUBSTR (CHAR2,1,NCHR,CHAR,-1)
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,400) LSLASH,NCHAR,IBEG,NCHR
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,410) 'CHAR2',CHAR2
               CALL SULINE (IOSDBG,1)
               WRITE (IOSDBG,410) 'CHAR',CHAR
               CALL SULINE (IOSDBG,1)
               ENDIF
            GO TO 10
            ENDIF
170      CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  START CPU TIMER
180   IUNIT=-1
      CALL SUTIMR (IUNIT,ITMELA,ITMTOT)
C
C  GET CURRENT DATE AND TIME IN INTEGER FORM AND STORE IN COMMON BLOCK
      CALL UDATEI (NMO,NDA,NYR,NHRMIN,NSEC,JULDAY,IERR)
      ISTDAY=JULDAY
      IELDAY=ISTDAY
      NHR=NHRMIN/100
      NMIN=NHRMIN-NHR*100
      ISTHMS=(NHR*10000+NMIN*100)+(NSEC/100)
      IELHMS=ISTHMS
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,420) ISTDAY,ISTHMS
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  CHECK WHICH FILES ARE ALLOCATED
      CALL SUDDST (IERR)
C
C  SAVE ORIGINAL DATA BASE ALLOCATION INDICATORS (NEEDED IF NEWUSER
C  OPTION OF COMMAND SETOPT USED)
      DO 190 I=1,10
         IDBSAV(I)=IDBALC(I)
190      CONTINUE
C
C  OPEN USER PARAMETER FILE TO GET USER NAME
      CALL SUDOPN (1,'UPRM',IERR)
      IF (IERR.GT.0) GO TO 200
C
C  SET USER NAME IN COMMON SUPAGX
      CALL SUBSTR (HNAMRF,1,8,IDUSER,1)
C
C  SET USER NAME IN COMMON UPAGEX
      CALL SUDRVR2
C
200   IF (NPSPAG.EQ.0) CALL SUPAGE
C
C  READ CARD INPUT
      CALL SUCMDS (LPMFLD,PMFLD,LARRAY,ARRAY,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,430) IERR,NFLD
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUDRVR'
         CALL SULINE (IOSDBG,2)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
220   FORMAT ('0PMFLD = ',A)
230   FORMAT (' LPMFLD = ',I2,' NFLD = ',I2)
240   FORMAT (' UNPAKS CALLED : STATUS CODE=',I3)
250   FORMAT (' SUFREE CALLED')
260   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
270   FORMAT (' UPACK1 CALLED : NFIELD=',I3,3X,'IFIELD=',I3,3X,
     *     'IFSTRT=',I3,3X,'IFSTOP=',I3)
280   FORMAT (' NCHAR=',I3,3X,'CHAR=',A)
290   FORMAT (' IFIELD=',I3,3X,'CHK=',A)
300   FORMAT ('0*** WARNING - PARAMETER FOUND IN FIELD ',I2,
     *   ' OF PARM FIELD IS NOT RECOGNIZED : ',A)
310   FORMAT (' TIMR OPTION SET')
320   FORMAT (' DEBUG OPTION SET :  LEVEL=',I2)
330   FORMAT (' PARMS=',A)
340   FORMAT (' TRACE OPTION SET :  LEVEL=',I2)
350   FORMAT ('0*** WARNING - NO LEFT PARENTHESIS FOUND. ',A,
     *   ' OPTION SET TO ',I2,'.')
360   FORMAT ('0*** NOTE - RIGHT PARENTHESIS ASSUMED IN FIELD ',I2,
     *   '.')
370   FORMAT ('0*** ERROR - CONDCODE VALUE (',I2,') MUST BE LESS ',
     *   'THAN OR EQUAL TO 16.')
380   FORMAT (' TSO OPTIONS SPECIFIED - NEWPAGE AND OVERPRNT OPTIONS ',
     *   'SET TO ''NO''.')
390   FORMAT (' CONDCODE OPTION SET TO ',I2)
400   FORMAT (' LSLASH=',I3,3X,'NCHAR=',I3,3X,'IBEG=',I3,3X,
     *   'NCHR=',I3)
410    FORMAT (' ',A,'=',A)
420   FORMAT (' ISTDAY=',I4,3X,'ISTHMS=',I8)
430   FORMAT (' SUCMDS CALLED : STATUS CODE=',I2,3X,'NFLD=',I2)
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SUDRVR2
C
C  SET USER NAME - NEED TO DO IN SEPARATE ROUTINE BECAUSE VARIABLE 
C  NPSPAG IS IN COMMON UPAGEX AND SUPAGX
C
      INCLUDE 'upagex'
      INCLUDE 'hclcommon/hdflts'
C
      CALL SUBSTR (HNAMRF,1,8,PUSRID,1)
C
      END
