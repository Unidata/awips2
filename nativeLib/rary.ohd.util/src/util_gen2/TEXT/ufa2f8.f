C MEMBER UFA2F8
C-----------------------------------------------------------------------
C
C  ROUTINE UFA2F8 CONVERTS FROM CHARACTERS TO A REAL*8 VALUE.
C
      SUBROUTINE UFA2F8 (CHAR,IBEG,NCHAR,NDEC,REAL,IPRERR,NPUNIT,
     *   ISTAT)
C
      CHARACTER*1 CHAR(1)
      REAL*8 REAL(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufa2f8.f,v $
     . $',                                                             '
     .$Id: ufa2f8.f,v 1.1 1995/09/17 19:02:28 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,10)
         ENDIF
C
      ISTAT=0
C
      REAL(1)=0.
C
      NVALUE=1
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,20) NVALUE,NDEC,IBEG,NCHAR,
     *      (CHAR(N),N=IBEG,IBEG+NCHAR-1)
         ENDIF
C
C  CHECK NUMBER OF CHARACTERS TO BE CONVERTED
      IF (NCHAR.LE.0) THEN
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (NPUNIT,30) NCHAR
            ENDIF
         ISTAT=1
         GO TO 5
         ENDIF
C
C
C  CHECK IF CHARACTER STRING HAS ONLY NUMBERS
      CALL UINDXC (CHAR(IBEG),NCHAR,'+- 0123456789.',14,'ONLY',IRETRN)
      IF (IRETRN.EQ.0) THEN
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (NPUNIT,35) (CHAR(N),N=IBEG,IBEG+NCHAR-1)
            ENDIF
         ISTAT=1
         GO TO 5
         ENDIF
C
C  CONVERT CHARACTERS
      CALL FFA2F8 (CHAR(IBEG),1,NCHAR,NDEC,NVALUE,REAL,IERR)
      IF (IERR.GT.0) THEN
         IF (IPRERR.GT.0) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (NPUNIT,40) (CHAR(N),N=IBEG,IBEG+NCHAR-1)
            ENDIF
         ISTAT=1
         REAL(1)=0.
         ENDIF
C
5     IF (ICMDBG.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,45) REAL
         ENDIF
       IF (ICMTRC.GT.1) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,50) ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER UFA2F8')
20    FORMAT (' NVALUE=',I3,3X,'NDEC=',I3,3X,'IBEG=',I3,3X,
     *   'NCHAR=',I3,3X,'CHAR(IBEG...IBEG+NCHAR-1)=',50A1)
30    FORMAT ('+*** ERROR - IN UFA2F8 - NUMBER OF CHARACTERS TO BE ',
     *   'CONVERTED (',I3,') IS NOT GREATER THAN ZERO.')
35    FORMAT ('+*** ERROR - IN UFA2F8 - CHARACTER STRING CONTAINS ',
     *   'NON-NUMERIC CHARACTERS : ',50A1)
40    FORMAT ('+*** ERROR - IN UFA2F8 - CONVERTING THE FOLLOWING ',
     *   'CHARACTER STRING : ',50A1)
45    FORMAT (' REAL=',G12.3)
50    FORMAT (' *** EXIT UFA2F8 - ISTAT=',I2)
C
      END
