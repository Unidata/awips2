C MODULE UFIEL2
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET NEXT FIELD FROM RECORDS STORED IN AN ARRAY.
C
      SUBROUTINE UFIEL2 (MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      CHARACTER*4 CHAR(1),ICARDS(1)
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufiel2.f,v $
     . $',                                                             '
     .$Id: ufiel2.f,v 1.2 1999/04/23 20:02:13 page Exp $
     . $' /
C    ===================================================================
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,10)
         ENDIF
C
      LMXCRD=MAXCRD
      IREAD=0
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LCHAR,CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,
     *   ISTAT)
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,20) ISTAT
         ENDIF
C
      RETURN
C
10    FORMAT (' *** ENTER UFIEL2')
20    FORMAT (' *** EXIT UFIEL2 - ISTAT=',I2)
C
      END
