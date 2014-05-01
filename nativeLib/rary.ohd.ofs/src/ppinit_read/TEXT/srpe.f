C MODULE SRPE
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ STATION PE PARAMETERS.
C
      SUBROUTINE SRPE (IVPE,STAID,NBRSTA,DESCRP,STATE,STALAT,
     *   ANEMHT,PFACT,ITYRAD,PECOR,PEB3,PECOEF,PESUM,NPESUM,
     *   JPESUM,UNUSED,LARRAY,ARRAY,IPTR,IPRERR,IPTRNX,ISTAT)
C
      DIMENSION ARRAY(LARRAY)
      DIMENSION UNUSED(1)
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
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_read/RCS/srpe.f,v $
     . $',                                                             '
     .$Id: srpe.f,v 1.2 1998/04/07 18:07:18 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,80)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('PE  ')
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'LARRAY=',LARRAY
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      ISTAT=0
C
C  READ PARAMETER RECORD
      CALL SUDOPN (1,'PPP ',IERR)
      CALL RPPREC (STAID,'PE  ',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,
     *     IERR)
      IF (IERR.NE.0) THEN
         ISTAT=IERR
         IF (IPRERR.EQ.1) THEN
            CALL SRPPST (STAID,'PE  ',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            ENDIF
         GO TO 70
         ENDIF
C
      NPOS=0
C
C  SET PARAMETER ARRAY VERSION NUMBER
      NPOS=NPOS+1
      IVPE=ARRAY(NPOS)
C
C  SET STATION IDENTIFIER
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
         CALL SUBSTR (ARRAY(NPOS),1,4,STAID(N:N),1)
10       CONTINUE
C
      NPOS=3
C
C  SET USER SPECIFIED STATION NUMBER
      NPOS=NPOS+1
      NBRSTA=ARRAY(NPOS)
C
C  SET DESCRIPTIVE INFORMATION
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
         CALL SUBSTR (ARRAY(NPOS),1,NCHAR,DESCRP(N:N),1)
20       CONTINUE
C
C  SET LATITUDE (DECIMAL DEGREES)
      NPOS=NPOS+1
      STALAT=ARRAY(NPOS)
C
C  SET ANEMOMETER HEIGHT (FT)
      NPOS=NPOS+1
      ANEMHT=ARRAY(NPOS)
C
C  SET P FACTOR
      NPOS=NPOS+1
      PFACT=ARRAY(NPOS)
C
C  SET PRIMARY TYPE OF RADIATION DATA TO BE USED
      NPOS=NPOS+1
      ITYRAD=ARRAY(NPOS)
C
C  SET CORRECTION FACTOR
      NPOS=NPOS+1
      PECOR=ARRAY(NPOS)
C
C  SET B3 PARAMETER
      NPOS=NPOS+1
      PEB3=ARRAY(NPOS)
C
C  SET STATE IDENTIFIER
      NPOS=NPOS+1
      STATE=ARRAY(NPOS)
C
C  THE NEXT POSITION IS UNUSED
      NPOS=NPOS+1
      UNUSED(1)=ARRAY(NPOS)
C
C  SET FOURIER SERIES COEFFICIENTS
      DO 30 I=1,6
         NPOS=NPOS+1
         PECOEF(I)=ARRAY(NPOS)
30       CONTINUE
C
C  SET PE SUM FOR LAST 12 MONTHS
      DO 40 I=1,12
         NPOS=NPOS+1
         PESUM(I)=ARRAY(NPOS)
40       CONTINUE
C
C  SET NUMBER OF VALUES IN PESUM FOR EACH MONTH
      DO 50 I=1,12
         NPOS=NPOS+1
         NPESUM(I)=ARRAY(NPOS)
50       CONTINUE
C
C  SET JULIAN DATE OF LAST DAY INCLUDED IN PESUM
      NPOS=NPOS+1
      JPESUM=ARRAY(NPOS)
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,100) NPOS,NFILL,IPTRNX,IVPE
         CALL SULINE (IOSDBG,1)
         CALL SUPDMP ('PE  ','REAL',0,NPOS,ARRAY,ARRAY)
         ENDIF
C
70    IF (ISTRCE.GT.1) THEN
         WRITE (IOSDBG,130)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER SRPE')
90    FORMAT ('0*** ERROR - IN SRPE - NUMBER OF WORDS IN VARIABLE ',
     *   A,'(',I2,') IS NOT ',I2,' FOR STATION ',A,'.')
100   FORMAT (' NPOS=',I3,3X,'NFILL=',I3,3X,'IPTRNX=',I3,3X,
     *   'IVPE=',I3)
130   FORMAT (' *** EXIT SRPE')
C
      END
