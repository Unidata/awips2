C MODULE SFPPCK
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK AVAILABLE SPACE IN PREPROCESSOR PARAMETRIC
C  DATA BASE.
C
      SUBROUTINE SFPPCK (STAID,NGPSN,GPSN,
     *   STSTAN,STPCPN,STTEMP,STPE,STRRS,
     *   LPSTAN,LPPCPN,LPTEMP,LPPE,LPRRS,
     *   IPCHARN,IPCHARO,IPMMNTN,IPMMNTO,
     *   NUMERR,NUMWRN,ISTAT)
C
      CHARACTER*4 TYPE,GPSN(NGPSN)
      CHARACTER*4 DISP,STSTAN,STPCPN,STTEMP,STPE,STRRS
      CHARACTER*8 STAID
      CHARACTER*20 STRING
C
      DIMENSION ICNTL(16)
      PARAMETER (MRUSD=5)
      DIMENSION NRUSD(MRUSD)
C
      INCLUDE 'uio'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfppck.f,v $
     . $',                                                             '
     .$Id: sfppck.f,v 1.1 1998/04/07 15:09:06 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET TRACE LEVEL
      LTRACE=ISTRC('STA ')
C
      IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,40)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('STA ')
C
      ISTAT=0
C
      IF (NMPPPF.GT.MRUSD) THEN 
         WRITE (LP,45) NMPPPF,MRUSD
         CALL SUERRS (LP,2,NUMERR)
         ISTAT=1
         GO TO 30
         ENDIF     
      CALL UMEMST (0,NRUSD,NMPPPF)
C
C  PROCESS EACH PARAMETER TYPE
      IPCPN=0
      ITEMP=0
      DO 10 I=1,NGPSN+1
C     CHECK FOR VALID DATA TYPE
         DISP='NEW'
         IF (I.EQ.1) THEN
            IF (STSTAN.EQ.'OLD') DISP='OLD'
            TYPE='GENL'
            LARRAY=LPSTAN
            ENDIF
         IF (I.GT.1) THEN
            IF (GPSN(I-1).EQ.'PCPN') THEN
               IF (STPCPN.EQ.'OLD') DISP='OLD'
               TYPE=GPSN(I-1)
               LARRAY=LPPCPN
               IPCPN=1
               ENDIF
            IF (GPSN(I-1).EQ.'TEMP') THEN
               IF (STTEMP.EQ.'OLD') DISP='OLD'
               TYPE=GPSN(I-1)
               LARRAY=LPTEMP
               ITEMP=1
               ENDIF
            IF (GPSN(I-1).EQ.'PE') THEN
               IF (STPE.EQ.'OLD') DISP='OLD'
               TYPE=GPSN(I-1)
               LARRAY=LPPE
               ENDIF
            IF (GPSN(I-1).EQ.'RRS') THEN
               IF (STRRS.EQ.'OLD') DISP='OLD'
               TYPE=GPSN(I-1)
               LARRAY=LPRRS
               ENDIF
            ENDIF
         IF (TYPE.EQ.' ') THEN
            WRITE (LP,50) GPSN(I-1),STAID
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 10
            ENDIF
         IF (LARRAY.EQ.0) THEN
            WRITE (LP,60) TYPE,STAID
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 10
            ENDIF
C     FIND TYPE IN DIRECTORY
         IDXDAT=IPCKDT(TYPE)
         IF (IDXDAT.EQ.0) THEN
            WRITE (LP,70) TYPE,STAID
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 10
            ENDIF
C     COMPUTE NUMBER OF WORDS NEEDED
         NWORDS=NPPHED+LARRAY
         IF (DISP.EQ.'OLD') THEN
C        CHECK IF NEW PARAMETER RECORD NOT LONGER THAN OLD
            IPTR=0
            CALL RPPREC (STAID,TYPE,IPTR,LSWORK,SWORK,NFILL,IPTRNX,IERR)
            IF (IERR.GT.0) THEN
               CALL SRPPST (STAID,TYPE,IPTR,LSWORK,NFILL,IPTRNX,IERR)
               ISTAT=1
               GO TO 10
               ENDIF
            IF (NWORDS.LE.NPPHED+NFILL) GO TO 10
            ENDIF
C     COMPUTE NUMBER OF RECORDS NEEDED
         NREC=IUNRCD(NWORDS,LRECPP)
C     SET FILE NUMBER
         IFILE=IPDTDR(2,IDXDAT)
C     SET MAXIMUM NUMBER OF RECORDS IN FILE    
         NRMAX=IPMCTL(1,IFILE)
C     SET NUMBER OF RECORDS USED IN FILE
         IF (NRUSD(IFILE).EQ.0) THEN
            NRUSD(IFILE)=IPMCTL(2,IFILE)
            ELSE
               NRUSD(IFILE)=NRUSD(IFILE)+NREC
            ENDIF
C     CHECK IF THERE IS ROOM IN FILE
         IF (NRUSD(IFILE).GT.NRMAX) THEN
            WRITE (LP,80) IFILE,TYPE,STAID
            CALL SUERRS (LP,2,NUMERR)
            ISTAT=1
            GO TO 10
            ENDIF
10       CONTINUE
C
C  CHECK IF STATION HAS PCPN DATA TYPE 
      IF (IPCPN.EQ.1) THEN
C     CHECK IF NEW DEFINITION HAS CHAR BUT OLD DOES NOT
         IF (IPCHARN.EQ.-1.AND.IPCHARO.EQ.0) THEN
C        CHECK FOR SPACE FOR PCPN CHARACTERISTICS
            IDXDAT=IPCKDT('CHAR')
            IREC=IPDTDR(3,IDXDAT)
            STRING='PCPN CHARACTERISTICS'
C        CHECK IF DEFINED
            IF (IREC.EQ.0) THEN
               WRITE (LP,90) STRING(1:LENSTR(STRING))
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 20
               ENDIF
C        READ CONTROL RECORD
            IUNIT=KPPRMU(IPDTDR(2,IDXDAT))
            CALL UREADT (IUNIT,IREC,ICNTL,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,100) IREC,IUNIT
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 20
               ENDIF
C       SET NUMBER OF SLOTS USED
            NSTA=ICNTL(7)
C       SET MAXIMUM NUMBER OF SLOTS
            MSTA=ICNTL(8)
C       COMPUTE NUMBER OF DELETED SLOTS
            NDEL=ICNTL(9)-ICNTL(7)
C        CHECK IF SLOT AVAILABLE
            IF (NSTA.EQ.MSTA.AND.NDEL.EQ.0) THEN
               WRITE (LP,110) STRING(1:LENSTR(STRING)),STAID
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               ENDIF
            ENDIF
         ENDIF
C
C  CHECK IF STATION HAS TEMP DATA TYPE 
20    IF (ITEMP.EQ.1) THEN
C     CHECK IF NEW DEFINITION HAS MMMT BUT OLD DOES NOT
         IF (IPMMNTN.EQ.-1.AND.IPMMNTO.EQ.0) THEN
C        CHECK FOR SPACE FOR TEMP MAX/MIN TEMPS
            IDXDAT=IPCKDT('MMMT')
            IREC=IPDTDR(3,IDXDAT)
            STRING='MEAN TEMPERATURES'
C        CHECK IF DEFINED
            IF (IREC.EQ.0) THEN
               WRITE (LP,90) STRING(1:LENSTR(STRING))
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 30
               ENDIF
C        READ CONTROL RECORD
            IUNIT=KPPRMU(IPDTDR(2,IDXDAT))
            CALL UREADT (IUNIT,IREC,ICNTL,IERR)
            IF (IERR.GT.0) THEN
               WRITE (LP,100) IREC,IUNIT
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               GO TO 30
               ENDIF
C       SET NUMBER OF SLOTS USED               
            NSTA=ICNTL(7)
C       SET MAXIMUM NUMBER OF SLOTS
            MSTA=ICNTL(8)
C       COMPUTE NUMBER OF DELETED SLOTS            
            NDEL=ICNTL(9)-ICNTL(7)
C        CHECK IF SLOT AVAILABLE
            IF (NSTA.EQ.MSTA.AND.NDEL.EQ.0) THEN
               WRITE (LP,110) STRING(1:LENSTR(STRING)),STAID
               CALL SUERRS (LP,2,NUMERR)
               ISTAT=1
               ENDIF
            ENDIF
         ENDIF
C
30    IF (LTRACE.GT.0) THEN
         WRITE (IOSDBG,120) ISTAT
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
40    FORMAT (' *** ENTER SFPPCK')
45    FORMAT ('0*** ERROR - IN SFPPCK - NUMBER OF PARAMETER FILES (',
     *   I2,' EXCEEDS SIZE OF ARRAY NRUSD (',I2,').')
50    FORMAT ('0*** ERROR - IN SFPPCK - ',A,
     *   ' IS AN INVALID DATA GROUP CODE ',
     *   'FOR STATION ',A,'.')
60    FORMAT ('0*** ERROR - IN SFPPCK - NUMBER OF WORDS IN ',A,
     *   ' PARAMETER ARRAY IS ZERO ',
     *   'FOR STATION ',A,'.')
70    FORMAT ('0*** ERROR - IN SFPPCK - PARAMETER TYPE ',A,
     *   ' NOT FOUND IN PREPROCESSOR PARAMETRIC DATA BASE DIRECTORY ',
     *   'FOR STATION ',A,'.')
80    FORMAT ('0*** ERROR - IN SFPPCK - NOT ENOUGH SPACE IN ',
     *   'PARAMETER FILE ',I2,' FOR ',A,' PARAMETERS ',
     *   'FOR STATION ',A,'.')
90    FORMAT ('0*** ERROR - IN SFPPCK - ',A,
     *   ' ARE NOT DEFINED IN PREPROCESSOR PARAMETRIC DATA BASE.')
100   FORMAT ('0*** ERROR - IN SFPPCK - ERROR READING RECORD ',I5,
     *    ' OF UNIT ',I2,'.')
110   FORMAT ('0*** ERROR - IN SFPPCK - NOT ENOUGH SPACE IN ',
     *   'PARAMETER FILE TO ADD ',A,' ',
     *   'FOR STATION ',A,'.')
120   FORMAT (' *** EXIT SFPPCK : ISTAT=',I2)
C
      END
