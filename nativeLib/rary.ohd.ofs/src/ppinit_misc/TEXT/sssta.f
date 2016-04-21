C MODULE SSSTA
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT STATION STATUS.
C
      SUBROUTINE SSSTA (LARRAY,ARRAY,NFLD,ISTAT)
C
      CHARACTER*4 DEGMIN,OTYPE
      CHARACTER*8 UNITS,RDISP
      CHARACTER*20 STRNG/' '/,STRNG2/' '/
C
      DIMENSION ARRAY(LARRAY)
      PARAMETER (LSIBUF=128)
      INTEGER*2 ISIBUF(LSIBUF)
C
      INCLUDE 'scommon/dimstan'
      INCLUDE 'scommon/dimpcpn'
      INCLUDE 'scommon/dimtemp'
      INCLUDE 'scommon/dimpe'
      INCLUDE 'scommon/dimrrs'
C
      DIMENSION UNUSED(10)
C
      INCLUDE 'uiox'
      INCLUDE 'upagex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/swrk2x'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_misc/RCS/sssta.f,v $
     . $',                                                             '
     .$Id: sssta.f,v 1.5 2002/02/11 21:05:02 dws Exp $
     . $' /
C    ===================================================================
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('STAT')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SSSTA'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STAT')
C
      ISTAT=0
C
      LSTRNG=LEN(STRNG)/4
      LSTRNG2=LEN(STRNG2)/4
C
      LSTAID=8
C
      ILPFND=0
      IRPFND=0
      NUMERR=0
      NUMWRN=0
      NPIFLD=0
      NPCFLD=NFLD
      IENDIN=0
      ISTRT=1
C
      IALL=0
      NSTAN=0
C
C  PRINT HEADER LINE
      IF (ISLEFT(10).GT.0) CALL SUPAGE
      WRITE (LP,170)
      CALL SULINE (LP,2)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  GET NEXT INPUT FIELD
C
10    CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
      IF (NFLD.EQ.-1) GO TO 30
      IF (LDEBUG.GT.0) THEN
         CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *      LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
         ENDIF
      IF (IERR.EQ.1) THEN
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,180) NFLD
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO 10
         ENDIF
C
C  CHECK FOR DEBUG
      CALL SUIDCK ('CMDS',STRNG,NFLD,0,ICMD,IERR)
      IF (ICMD.EQ.1) THEN
         CALL SBDBUG (NFLD,ISTRT,IERR)
         LDEBUG=ISBUG('STAT')
         GO TO 10
         ENDIF
C
C  CHECK FOR PAIRED PARENTHESES
      CALL SUPFND (ILPFND,IRPFND,NPIFLD,NPCFLD)
C
C  CHECK FOR PARENTHESIS IN FIELD
      IF (LLPAR.GT.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LLPAR-1,IERR)
      IF (LLPAR.EQ.0) CALL UFPACK (LSTRNG2,STRNG2,ISTRT,1,LENGTH,IERR)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'STRNG2=',STRNG2
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IF (STRNG2.EQ.'ALL') THEN
         IALL=1
         ELSE
C        CHECK FOR GROUP
            CALL SUIDCK ('STAT',STRNG2,NFLD,0,IKEYWD,IERR)
            IF (IERR.EQ.0) GO TO 20
            IF (NSTAN.EQ.0) THEN
               WRITE (LP,185)
               CALL SULINE (LP,2)
               IALL=1
               GO TO 20
               ENDIF
            GO TO 30
         ENDIF
C
C  CHECK FOR COMMAND
20    IF (LATSGN.EQ.0) GO TO 40
30       IENDIN=1
         IF (NSTAN.EQ.0) THEN
            WRITE (LP,185)
            CALL SULINE (LP,2)
            IALL=1
            GO TO 40
            ENDIF
         GO TO 160
C
C  CHECK LENGTH OF IDENTIFIER
40    IF (LENGTH.GT.LSTAID) THEN
         WRITE (LP,190) LENGTH,STRNG(1:LENSTR(STRNG)),LSTAID,LSTAID
         CALL SULINE (LP,2)
         ENDIF
      CALL SUBSTR (STRNG,1,LSTAID,STAID,1)
C
      WRITE (LP,200)
      CALL SULINE (LP,2)
C
C  OPEN DATA BASES
      CALL SUDOPN (1,'GOES',IERR)
      CALL SUDOPN (1,'SASM',IERR)
      CALL SUDOPN (1,'PPD ',IERR)
      CALL SUDOPN (1,'PPP ',IERR)
      CALL SUDOPN (1,'PRD ',IERR)
C
C  CHECK IF ALL STATIONS TO BE PROCESSED
      IF (IALL.EQ.1) THEN
         ISORT=1
         NWORDS=2
         MAXID=LSWRK2/2
         ISTATE=0
         IPNTRS=0
         NUMID=0
         IPRMSG=1
         IPRERR=0
         CALL SURIDS ('STAN',ISORT,MAXID,ISTATE,NWORDS,SWRK2,
     *      IPNTRS,NUMID,LARRAY,ARRAY,IPRMSG,IPRERR,IERR)
         NUMID2=0
         ENDIF
C
45    IF (IALL.EQ.1) THEN
         NUMID2=NUMID2+1
         CALL SUBSTR (SWRK2(NUMID2*2-1),1,8,STAID,1)
         ENDIF
C
      NSTAN=NSTAN+1
C
C -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  PRINT DATA ENTRY CONTROL FILE INFORMATION
C
      LEVEL=0
      DEGMIN='NO'
C
      WRITE (LP,210) 'GOES CONTROL FILE'
      CALL SULINE (LP,2)
C
      CALL SSGOES (PUSRID,LARRAY,ARRAY,LEVEL,STAID,IERR)
C
      WRITE (LP,210) 'SASM CONTROL FILE'
      CALL SULINE (LP,2)
C
      CALL SSSASM (PUSRID,LARRAY,ARRAY,LEVEL,DEGMIN,STAID,IERR)
C
C -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  PRINT PREPROCESSOR DATA BASE INFORMATION
C
      WRITE (LP,210) 'PREPROCESSOR DATA BASE'
      CALL SULINE (LP,2)
C
      CALL SSPPD2 (LARRAY,ARRAY,STAID,LSIBUF,ISIBUF,IERR)
C
C -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  PRINT PARAMETRIC DATA BASE INFORMATION
C
      WRITE (LP,210) 'PREPROCESSOR PARAMETRIC DATA BASE'
      CALL SULINE (LP,2)
C
C  PRINT STATION GENERAL PARAMETERS
      UNITS='ENGL'
      RDISP='OLD'
      IPTR=0
      IPRERR=0
      INCLUDE 'scommon/callsrstan'
      IF (IERR.GT.0) THEN
         IF (IERR.EQ.6) GO TO 150
         IF (IERR.EQ.2) GO TO 50
            CALL SRPPST (STAID,'STAN',IPTR,LARRAY,0,IPTRNX,IERR)
            WRITE (LP,220) 'STAN',IERR
            CALL SUERRS (LP,2,NUMERR)
            GO TO 150
50       WRITE (LP,230) STAID
         CALL SULINE (LP,2)
         GO TO 150
         ENDIF
C
C  CHECK IF STATION IS COMPLETE
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'ICSTAN=',ICSTAN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (ICSTAN.EQ.1) THEN
         WRITE (LP,240) STAID
         CALL SULINE (LP,2)
         ENDIF
C
C  SET INDICATORS FOR PARAMETER TYPES DEFINED
      IPCPN=0
      ITEMP=0
      IPE=0
      IRRS=0
      DO 60 I=1,NGPS
         IF (GPS(I).EQ.'PCPN') IPCPN=1
         IF (GPS(I).EQ.'TEMP') ITEMP=1
         IF (GPS(I).EQ.'PE') IPE=1
         IF (GPS(I).EQ.'RRS') IRRS=1
60       CONTINUE
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'STAID=',STAID,' STATE=',STATE,
     *      ' IPCPN=',IPCPN,' ITEMP=',ITEMP,' IPE=',IPE,' IRRS=',IRRS,
     *      ' IPTR=',IPTR
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      IPRNT=1
      INCLUDE 'scommon/callspstan'
C
C  CHECK IF STATION IS COMPLETE
      IF (ICSTAN.NE.0) GO TO 150
C
C  CHECK IF STATION HAS PRECIPITATION PARAMETERS
      IF (IPCPN.EQ.0) THEN
         IF (ISIBUF(8).GT.0) THEN
            WRITE (LP,270) STAID,'PCPN'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         ENDIF
      IF (IPCPN.EQ.1) THEN
         IPTR=0
         IPRERR=0
         IREAD=1
         INCLUDE 'scommon/callsrpcpn'
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) GO TO 70
               CALL SRPPST (STAID,'PCPN',IPTR,LARRAY,0,IPTRNX,IERR)
               WRITE (LP,220) 'PCPN',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 80
70          WRITE (LP,250) STAID,'PCPN'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 80
            ENDIF
         IF (ISIBUF(8).EQ.0) THEN
            WRITE (LP,260) STAID,'PCPN'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         LEVEL=1
         IPREST=0
         INCLUDE 'scommon/callsppcpn'
         ENDIF
C
C  CHECK IF STATION HAS TEMPERATURE PARAMETERS
80    IF (ITEMP.EQ.0) THEN
         IF (ISIBUF(9).GT.0) THEN
            WRITE (LP,270) STAID,'TEMP'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         ENDIF
      IF (ITEMP.EQ.1) THEN
         IPTR=0
         IPRERR=0
         IREAD=1
         INCLUDE 'scommon/callsrtemp'
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) GO TO 90
               CALL SRPPST (STAID,'TEMP',IPTR,LARRAY,0,IPTRNX,IERR)
               WRITE (LP,220) 'TEMP',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 100
90          WRITE (LP,250) STAID,'TEMP'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 100
            ENDIF
         IF (ISIBUF(9).EQ.0) THEN
            WRITE (LP,260) STAID,'TEMP'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         LEVEL=1
         INCLUDE 'scommon/callsptemp'
         ENDIF
C
      IPE2=0
      IRRS2=0
      NOTYPE=ISIBUF(10)
      IF (NOTYPE.GT.0) THEN
         JSIBUF=11
         DO 130 I=1,NOTYPE
            IPOS=JSIBUF+(I-1)*3
            CALL SUBSTR (ISIBUF(IPOS),1,4,OTYPE,1)
            IF (OTYPE.EQ.'EA24') IPE2=1
            IRRSTP=IPDCKR(OTYPE)
            IF (IRRSTP.GT.0) IRRS2=1
130         CONTINUE
         ENDIF
C
C  CHECK IF STATION HAS POTENTIAL EVAPORATION PARAMETERS
100   IF (IPE.EQ.0) THEN
         IF (IPE2.EQ.1) THEN
            WRITE (LP,270) STAID,'PE'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         ENDIF
      IF (IPE.EQ.1) THEN
         IPTR=0
         IPRERR=0
         INCLUDE 'scommon/callsrpe'
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) GO TO 110
               CALL SRPPST (STAID,'PE',IPTR,LARRAY,0,IPTRNX,IERR)
               WRITE (LP,220) 'PE',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 120
110         WRITE (LP,250) STAID,'PE'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 120
            ENDIF
         IF (IPE2.EQ.0) THEN
            WRITE (LP,260) STAID,'PE'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         INCLUDE 'scommon/callsppe'
         ENDIF
C
C  CHECK IF STATION HAS RIVER, RESERVIOR AND SNOW PARAMETERS
120   IF (IRRS.EQ.0) THEN
         IF (IRRS2.EQ.1) THEN
            WRITE (LP,270) STAID,'RRS'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         ENDIF
      IF (IRRS.EQ.1) THEN
         IPTR=0
         IPRERR=0
         IREAD=1
         INCLUDE 'scommon/callsrrrs'
         IF (IERR.GT.0) THEN
            IF (IERR.EQ.2) GO TO 140
               CALL SRPPST (STAID,'RRS',IPTR,LARRAY,0,IPTRNX,IERR)
               WRITE (LP,220) 'RRS',IERR
               CALL SUERRS (LP,2,NUMERR)
               GO TO 150
140         WRITE (LP,250) STAID,'RRS'
            CALL SUWRNS (LP,2,NUMWRN)
            GO TO 150
            ENDIF
         IF (IRRS2.EQ.0) THEN
            WRITE (LP,260) STAID,'RRS'
            CALL SUWRNS (LP,2,NUMWRN)
            ENDIF
         INCLUDE 'scommon/callsprrs'
         ENDIF
C
C -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -  -
C
C  PRINT PROCESSED DATA BASE INFORMATION
C
150   WRITE (LP,210) 'PROCESSED DATA BASE'
      CALL SULINE (LP,2)
C
      CALL SSPRD1 (LARRAY,ARRAY,STAID,NFLD,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (IALL.EQ.1.AND.NUMID2.LT.NUMID) GO TO 45
C
C  CHECK IF END OF INPUT FOR COMMAND
      IF (NFLD.EQ.-1.OR.IENDIN.EQ.1) GO TO 160
      NPCFLD=NFLD
      GO TO 10
C
160   WRITE (LP,280) NSTAN
      CALL SULINE (LP,2)
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SSSTA : ISTAT=',ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT ('0*--> PRINT STATION STATUS')
180   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
185   FORMAT ('0*** NOTE - NO STATION IDENTIFIERS FOUND. ',
     *   '''ALL'' ASSUMED.')
190   FORMAT ('0*** WARNING - NUMBER OF CHARACTERS (',I2,') IN ',
     *   'STATION IDENTIFIER ',A,' EXCEEDS ',I2,'. THE FIRST ',I2,
     *   ' CHARACTERS WILL BE USED.')
200   FORMAT ('0',132('#'))
210   FORMAT ('0- ATTRIBUTES OF STATION IN ',A,' -')
220   FORMAT ('0*** ERROR - IN SMSTA - UNSUCCESSFUL CALL TO SR',A,
     *   ' : STATUS CODE=',I2)
230   FORMAT ('0*** NOTE - STATION ',A,' NOT FOUND IN ',
     *   'PREPROCESSOR PARAMETRIC DATA BASE.')
240   FORMAT ('0*** NOTE - STATUS FOR STATION ',A,' IS INCOMPLETE.')
250   FORMAT ('0*** WARNING - STAN PARAMETERS FOR STATION ',A,
     *   ' INDICATE STATION HAS ',A,' PARAMETERS BUT THE ',
     *   'PARAMETERS ARE NOT DEFINED.')
260   FORMAT ('0*** WARNING - STATION ',A,' HAS ',A,
     *   ' PARAMETERS BUT THE DATA TYPE IS NOT ',
     *   'DEFINED IN THE PREPROCESSOR DATA BASE.')
270   FORMAT ('0*** WARNING - STATION ',A,' DOES NOT HAVE ',A,
     *   ' PARAMETERS BUT THE DATA TYPE IS ',
     *   'DEFINED IN THE PREPROCESSOR DATA BASE.')
280   FORMAT ('0*** NOTE - ',I4,' STATIONS PROCESSED.')
C
      END
