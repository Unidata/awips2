C MODULE UFIELD
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET NEXT FIELD FROM RECORDS FROM FILE.
C
      SUBROUTINE UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      CHARACTER*4 STRNG(LSTRNG),ICARDS(1)
C
      INCLUDE 'ucmdbx'
      INCLUDE 'ufreex'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufield.f,v $
     . $',                                                             '
     .$Id: ufield.f,v 1.3 2001/06/13 08:29:14 mgm Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UFIELD'
         ENDIF
C
      LMXCRD=9999999
      IREAD=1
C
      CALL UGTFLD (IREAD,LMXCRD,MAXCRD,ICARDS,
     *   NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,
     *   LSTRNG,STRNG,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,ISTAT)
C
      IF (ICMDBG.GT.0) THEN
         IF (LSTRNG.GT.0) THEN
            WRITE (ICMPRU,*) 'STRNG=',(STRNG(I),I=1,LSTRNG)
            ELSE
               LSTRNG2=-LSTRNG/4
               WRITE (ICMPRU,*) 'STRNG=',(STRNG(I),I=1,LSTRNG2)
            ENDIF
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UFIELD - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
      END
