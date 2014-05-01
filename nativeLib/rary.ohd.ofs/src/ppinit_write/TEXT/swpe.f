C MODULE SWPE
C-----------------------------------------------------------------------
C
C  ROUTINE TO WRITE STATION PE PARAMETERS.
C
      SUBROUTINE SWPE (IVPE,STAID,NBRSTA,DESCRP,STATE,STALAT,
     *   ANEMHT,PFACT,ITYRAD,PECOR,PEB3,PECOEF,PESUM,NPESUM,
     *   JPESUM,UNSD,LARRAY,ARRAY,IPTR,DISP,
     *   IWRITE,NPOS,ISTAT)
C
      CHARACTER*4 DISP
C
      DIMENSION ARRAY(LARRAY)
C
      INCLUDE 'scommon/dimsta'
      INCLUDE 'scommon/dimpe'
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_write/RCS/swpe.f,v $
     . $',                                                             '
     .$Id: swpe.f,v 1.2 1998/04/07 18:34:11 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('PE  ')
C
      IF (LDEBUG.GT.0) THEN
          WRITE (IOSDBG,*)
     *      ' IVPE=',IVPE,
     *      ' UNSD=',UNSD,
     *      ' LARRAY=',LARRAY,
     *      ' '
          CALL SULINE (IOSDBG,1)
          ENDIF
C
      ISTAT=0
C
      IF (IWRITE.EQ.-1) GO TO 60
C
C  CHECK FOR SUFFICIENT SPACE IN PARAMETER ARRAY
      MINLEN=48
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) ' MINLEN=',MINLEN
         CALL SULINE (IOSDBG,1)
         ENDIF
      IF (MINLEN.GT.LARRAY) THEN
         WRITE (LP,100) LARRAY,MINLEN
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
C
      NPOS=0
C
C  STORE PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      ARRAY(NPOS)=IVPE+.01
C
C  STORE STATION IDENTIFIER
      NCHAR=4
      NWORDS=LEN(STAID)/NCHAR
      NCHK=2
      IF (NWORDS.NE.NCHK) THEN
         WRITE (LP,90) 'STAID',NWORDS,NCHK,STAID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
      DO 10 I=1,NWORDS
         NPOS=NPOS+1
         N=(I-1)*NCHAR+1
         CALL SUBSTR (STAID(N:N),1,4,ARRAY(NPOS),1)
10       CONTINUE
C
C  STORE USER SPECIFIED STATION NUMBER
      NPOS=NPOS+1
      ARRAY(NPOS)=NBRSTA+.01
C
C  STORE DESCRIPTIVE INFORMATION
      NCHAR=4
      NWORDS=LEN(DESCRP)/NCHAR
      NCHK=5
      IF (NWORDS.NE.NCHK) THEN
         WRITE (LP,90) 'DESCRP',NWORDS,NCHK,STAID
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
      DO 20 I=1,NWORDS
         NPOS=NPOS+1
         N=(I-1)*NCHAR+1
         CALL SUBSTR (DESCRP(N:N),1,NCHAR,ARRAY(NPOS),1)
20       CONTINUE
C
C  STORE LATITUDE
      NPOS=NPOS+1
      ARRAY(NPOS)=STALAT
C
C  STORE ANEMOMETER HEIGHT
      NPOS=NPOS+1
      ARRAY(NPOS)=ANEMHT
C
C  STORE P FACTOR
      NPOS=NPOS+1
      ARRAY(NPOS)=PFACT
C
C  STORE PRIMARY TYPE OF RADIATION DATA TO BE USED
      NPOS=NPOS+1
      ARRAY(NPOS)=ITYRAD+.01
C
C  STORE CORRECTION FACTOR
      NPOS=NPOS+1
      ARRAY(NPOS)=PECOR
C
C  STORE B3 PARAMETER
      NPOS=NPOS+1
      ARRAY(NPOS)=PEB3
C
C  SET STATE IDENTIFIER
      NPOS=NPOS+1
      ARRAY(NPOS)=STATE
C
C  THE NEXT POSITION IS UNUSED
      NPOS=NPOS+1
      ARRAY(NPOS)=UNSD
C
C  STORE FOURIER SERIES COEFFICIENTS
      DO 30 I=1,6
         NPOS=NPOS+1
         ARRAY(NPOS)=PECOEF(I)
30       CONTINUE
C
C  STORE PE SUM FOR LAST 12 MONTHS
      DO 40 I=1,12
         NPOS=NPOS+1
         ARRAY(NPOS)=PESUM(I)
40       CONTINUE
C
C  STORE NUMBER OF VALUES IN PESUM FOR EACH MONTH
      DO 50 I=1,12
         NPOS=NPOS+1
         ARRAY(NPOS)=NPESUM(I)+.01
50       CONTINUE
C
C  STORE JULIAN DATE OF LAST DAY INCLUDED IN PESUM
      NPOS=NPOS+1
      ARRAY(NPOS)=JPESUM+.01
C
      IF (IWRITE.EQ.0) GO TO 70
C
C  OPEN DATA BASE
60    CALL SUDOPN (1,'PPP ',IERR)
      IF (IERR.NE.0) THEN
         ISTAT=1
         GO TO 70
         ENDIF
C
C  WRITE PARAMTER RECORD
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'NPOS=',NPOS
         CALL SULINE (IOSDBG,1)
         ENDIF
      IPTR=0
      CALL WPPREC (STAID,'PE  ',NPOS,ARRAY,IPTR,IERR)
      IF (IERR.NE.0) THEN
         CALL SWPPST (STAID,'PE  ',NPOS,IPTR,IERR)
         WRITE (LP,110) IERR
         CALL SULINE (LP,2)
         ISTAT=3
         CALL SUERRS (LP,2,-1)
         ENDIF
C
      IF (LDEBUG.GT.0) CALL SUPDMP ('PE  ','BOTH',0,NPOS,ARRAY,ARRAY)
C
      IF (ISTAT.EQ.0) THEN
         IF (DISP.EQ.'NEW') THEN
            WRITE (LP,120) STAID
            CALL SULINE (LP,2)
            ENDIF
         IF (DISP.EQ.'OLD') THEN
            WRITE (LP,130) STAID
            CALL SULINE (LP,2)
            ENDIF
         CALL SUDWRT (1,'PPP ',IERR)
         ENDIF
      IF (ISTAT.GT.0) THEN
         WRITE (LP,140) STAID
         CALL SULINE (LP,2)
         ENDIF
C
70    IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,150)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SWPE')
90    FORMAT ('0*** ERROR - IN SWPE - NUMBER OF WORDS IN VARIABLE ',
     *   A,'(',I2,') IS NOT ',I2,' FOR STATION ',A,'.')
100   FORMAT ('0*** ERROR - IN SWPE - NOT ENOUGH SPACE IN PARAMETER ',
     *   'ARRAY: NUMBER OF WORDS IN PARAMETER ARRAY=',I5,3X,
     *   'NUMBER OF WORDS NEEDED=',I5)
110   FORMAT ('0*** ERROR - IN SWPE - UNSUCCESSFUL CALL TO WPPREC : ',
     *   'STATUS CODE=',I3)
120   FORMAT ('0*** NOTE - PE   PARAMETERS SUCCESSFULLY ',
     *   'WRITTEN FOR STATION ',A,'.')
130   FORMAT ('0*** NOTE - PE   PARAMETERS SUCCESSFULLY ',
     *   'UPDATED FOR STATION ',A,'.')
140   FORMAT ('0*** NOTE - PE   PARAMETERS NOT SUCCESSFULLY ',
     *   'WRITTEN FOR STATION ',A,'.')
150   FORMAT (' *** EXIT SWPE','.')
C
      END
