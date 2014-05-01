C MODULE USCAN2
C-----------------------------------------------------------------------
C
C  ROUTINE TO RETURN A WORD FROM A CHARACTER EXPRESSION USING THE
C  SPECIFIED DELIMETER CHARACTERS.
C
      SUBROUTINE USCAN2 (STRNG,DLIM,NSCAN,WORD,LWORD,ISTAT)
C
C  INPUT VARIABLES :
C     STRNG  - CHARACTER STRING TO BE CHECKED FOR DLIM
C     DLIM   - CHARACTER STRING CONTAINING DELIMETERS
C     NSCAN  - WORD NUMBER TO BE RETURNED
C
C  OUTPUT VARIABLES :
C     WORD   - CHARACTER STRING CONTAINING WORD
C              (WHEN NO MORE FIELDS ARE FOUND, SET TO BLANK)
C     LWORD  - NUMBER OF CHARACTERS IN WORD
C     ISTAT  - STATUS CODE (SEE USCAN STATUS CODES)
C
      CHARACTER*(*) STRNG,DLIM,WORD
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/uscan2.f,v $
     . $',                                                             '
     .$Id: uscan2.f,v 1.2 1999/07/06 13:00:37 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER USCAN2'
         ENDIF
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'STRNG=',STRNG,' DLIM=',DLIM,' NSCAN=',NSCAN
         ENDIF
C
      ISTAT=0
C
      CALL USCAN (STRNG,LEN(STRNG),
     *   DLIM,LEN(DLIM),
     *   NSCAN,
     *   WORD,LEN(WORD),LWORD,
     *   ISTAT)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'WORD=',WORD,' LWORD=',LWORD
         ENDIF
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT USCAN2 - ISTAT=',ISTAT
         ENDIF
C
      RETURN
C
      END
