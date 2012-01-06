C  =====================================================================
C  pgm: UPFNGN .. Eliminate the last non-blank char in a 32-char string
C
C  use:     CALL UPFNGN( string )
C
C  i/o: string ........ 32-char name to have last non-blank char
C   in:                 changed to a blank
C  =====================================================================
      SUBROUTINE UPFNGN(NAME)

      CHARACTER*32  NAME
      CHARACTER*1   BLANK,KHAR
      INTEGER       KK
      PARAMETER   ( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/upfngn.f,v $
     . $',                                                             '
     .$Id: upfngn.f,v 1.1 1995/09/17 19:00:50 dws Exp $
     . $' /
C    ===================================================================
C

        KK = 33
  100   KK = KK-1
        IF( KK .LE. 0 ) GO TO 110
          KHAR = NAME(KK:KK)
          IF( KHAR .EQ. BLANK ) GO TO 100
            NAME(KK:KK) = BLANK
  110     CONTINUE

      RETURN
      END
