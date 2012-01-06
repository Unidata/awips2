C MODULE UFI2A
C-----------------------------------------------------------------------
C
C  ROUTINE UFI2A CONVERTS FROM AN INTEGER VALUE TO CHARACTERS.
C
      SUBROUTINE UFI2A (INTEGR,CHAR,IBEG,NCHAR,IPRERR,NPUNIT,
     *   ISTAT)
C
      CHARACTER*1 CHAR(1)
      DIMENSION INTEGR(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ufi2a.f,v $
     . $',                                                             '
     .$Id: ufi2a.f,v 1.2 2001/06/13 10:08:30 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UFI2A'
         ENDIF
C
      ISTAT=0
C
      NVALUE=1
C
      NACHAR=IABS(NCHAR)
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,20) NVALUE,IBEG,NCHAR,INTEGR
         ENDIF
C
C  CONVERT INTEGER
      CALL FFI2A (CHAR,IBEG,NACHAR,NVALUE,INTEGR)
      IF (CHAR(IBEG).EQ.'*') THEN
         IF (IPRERR.GT.0) THEN
            CALL UEROR (NPUNIT,1,-1)
            WRITE (NPUNIT,30) INTEGR,NACHAR
            ENDIF
         ISTAT=1
         ENDIF
C
C  CHECK IF BLANKS TO BE CHANGED TO ZEROS
      IF (NCHAR.LT.0) THEN
         DO 15 I=IBEG,IBEG+NACHAR-1
            IF (CHAR(I).EQ.' ') CHAR(I)='0'
15          CONTINUE
         ENDIF
C
      IF (ICMDBG.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,40)
     *      (CHAR(N),N=IBEG,IBEG+NACHAR-1)
         ENDIF
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UFI2A - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
20    FORMAT (' NVALUE=',I3,3X,'IBEG=',I3,3X,'NCHAR=',I3,3X,
     *   'INTEGR=',I7)
30    FORMAT ('+*** ERROR - IN UFI2A - INTEGER VALUE ',I7,
     *   ' CANNOT BE STORED IN ',I7,' CHARACTERS.')
40    FORMAT (' ',
     *   'CHAR(IBEG...IBEG+NACHAR-1)=',50A1)
C
      END
