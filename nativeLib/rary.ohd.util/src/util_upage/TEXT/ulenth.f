C MEMBER ULENTH
C-----------------------------------------------------------------------
C
      SUBROUTINE ULENTH (STRNG,LSTRNG,LENGTH)
C
C  ROUTINE ULENTH FINDS THE LOCATION OF THE RIGHT MOST NON-BLANK
C  CHARACTER IN A CHARACTER STRING.
C
C  INPUT VARIABLES -
C     STRNG  - CHARACTER STRING
C     LSTRNG - MAXIMUM NUMBER IF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING
C
C  OUTPUT VARIABLES -
C     LENGTH - NUMBER OF CHARACTERS IN CHARACTER STRING (LOCATION OF
C              RIGHT MOST NON-BLANK CHARACTER)
C
C
      CHARACTER*1 STRNG(1)
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_upage/RCS/ulenth.f,v $
     . $',                                                             '
     .$Id: ulenth.f,v 1.1 1995/09/17 19:05:12 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,10) LSTRNG
         ENDIF
C
      ISTAT=0
C
      LENGTH=0
      IF (LSTRNG.LE.0) GO TO 40
C
      LENGTH=LSTRNG
C
      DO 30 I=LSTRNG,1,-1
         IF (STRNG(I).NE.' ') GO TO 40
         LENGTH=I-1
30       CONTINUE
C
40    IF (ICMTRC.GT.1) THEN
         CALL ULINE2 (ICMPRU,1)
         WRITE (ICMPRU,50) LENGTH
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER ULENTH - LSTRNG=',I3)
50    FORMAT (' *** EXIT ULENTH - LENGTH=',I3)
C
      END
