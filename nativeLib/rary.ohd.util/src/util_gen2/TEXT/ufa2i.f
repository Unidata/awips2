C MEMBER UFA2I
C-----------------------------------------------------------------------
C
C  ROUTINE UFA2I CONVERTS FROM CHARACTERS TO AN INTEGER*4 VALUE.
C
      SUBROUTINE UFA2I (CHAR,IBEG,NCHAR,INTEGR,IPRERR,NPUNIT,
     *   ISTAT)
C
      CHARACTER*1 CHAR(1)
      INTEGER*4 INTEGR(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufa2i.f,v $
     . $',                                                             '
     .$Id: ufa2i.f,v 1.1 1995/09/17 19:02:30 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,10)
         ENDIF
C
      ISTAT=0
C
      INTEGR(1)=0
C
      IPUNIT=IABS(NPUNIT)
C
      NVALUE=1
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,20) NVALUE,IBEG,NCHAR,
     *      (CHAR(N),N=IBEG,IBEG+NCHAR-1)
         ENDIF
C
C  CHECK NUMBER OF CHARACTERS TO BE CONVERTED
      IF (NCHAR.LE.0) THEN
         IF (IPRERR.EQ.1) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (IPUNIT,30) NCHAR
            ENDIF
         ISTAT=1
         GO TO 5
         ENDIF
C
C  CHECK IF CHARACTER STRING HAS ONLY NUMBERS
      CALL UINDXC (CHAR(IBEG),NCHAR,'+- 0123456789',13,'ONLY',IRETRN)
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
      CALL FFA2I (CHAR(IBEG),1,NCHAR,NVALUE,INTEGR,IERR)
      IF (IERR.GT.0) THEN
         IF (IPRERR.GT.0) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (IPUNIT,40) (CHAR(N),N=IBEG,IBEG+NCHAR-1)
            ENDIF
         ISTAT=1
         INTEGR(1)=0
         ENDIF
C
5     IF (ICMDBG.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,45) INTEGR
         ENDIF
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,50) ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER UFA2I')
20    FORMAT (' NVALUE=',I3,3X,'IBEG=',I3,3X,'NCHAR=',I3,3X,
     *   'CHAR(IBEG...IBEG+NCHAR-1)=',50A1)
30    FORMAT ('+*** ERROR - IN UFA2I - NUMBER OF CHARACTERS TO BE ',
     *   'CONVERTED (',I3,') IS NOT GREATER THAN ZERO.')
35    FORMAT ('+*** ERROR - IN UFA2I - CHARACTER STRING CONTAINS ',
     *   'NON-NUMERIC CHARACTERS : ',50A1)
40    FORMAT ('+*** ERROR - IN UFA2I - CONVERTING THE FOLLOWING ',
     *   'CHARACTER STRING : ',50A1)
45    FORMAT (' INTEGR=',I10)
50    FORMAT (' *** EXIT UFA2I - ISTAT=',I2)
C
      END
