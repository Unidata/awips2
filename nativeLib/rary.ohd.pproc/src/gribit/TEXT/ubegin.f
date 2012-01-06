C MODULE UBEGIN
C-----------------------------------------------------------------------
C
      SUBROUTINE UBEGIN (STRNG,LSTRNG,LBEGIN)
C
C  ROUTINE UBEGIN FINDS THE LOCATION OF THE LEFT MOST NON-BLANK
C  CHARACTER IN A CHARACTER STRING.
C
C  INPUT VARIABLES -
C     STRNG  - CHARACTER STRING
C     LSTRNG - MAXIMUM NUMBER OF CHARACTERS IN VARIABLE CONTAINING
C              CHARACTER STRING
C
C  OUTPUT VARIABLES -
C     LBEGIN - LOCATION OF LEFT MOST NON-BLANK CHARACTER
C
C
      INCLUDE 'uiox'
      INCLUDE 'ucmdbx'
C
      CHARACTER*1 STRNG(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/ubegin.f,v $
     . $',                                                             '
     .$Id: ubegin.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UBEGIN - LSTRNG=',LSTRNG
         ENDIF
C
      ISTAT=0
C
      LBEGIN=0
      IF (LSTRNG.EQ.0) GO TO 40
C
      DO 30 I=1,LSTRNG
         IF (STRNG(I).EQ.' ') GO TO 30
            LBEGIN=I
            GO TO 40
30       CONTINUE
C
40    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UBEGIN - LBEGIN=',LBEGIN
         ENDIF
C
      RETURN
C
      END
