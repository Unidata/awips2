C  =====================================================================
C  pgm: KKTRIM(STRING,IBEG,IEND) .. Get begin and end char loc in var
C
C   in: STRING ....... input string as character data - CHAR*(*)
C  out: IBEG ......... location of first non-blank char, else 1 - INT
C  out: IEND ......... location of last non-blank char, else 1 - INT
C
C  cmt: If input character variable is all blank then IBEG and IEND
C  cmt:   will both be set to 1.
C  =====================================================================
      SUBROUTINE KKTRIM(STRING,IBEG,IEND)


      INTRINSIC      LEN
      CHARACTER*(*)  STRING
      CHARACTER      BLANK
      INTEGER        IBEG,IEND,LEN,IBBB,IEEE
      PARAMETER      ( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kktrim.f,v $
     . $',                                                             '
     .$Id: kktrim.f,v 1.1 1995/09/17 19:02:08 dws Exp $
     . $' /
C    ===================================================================
C


        IBBB = 0
        IEEE = LEN(STRING)+1

  100   IEEE = IEEE-1
        IF( IEEE .LE. 1 ) GO TO 110
        IF( STRING(IEEE:IEEE) .EQ. BLANK ) GO TO 100

  110   IBBB = IBBB+1
        IF( IBBB .GE. IEEE ) GO TO 120
        IF( STRING(IBBB:IBBB) .EQ. BLANK ) GO TO 110

  120   IBEG = IBBB
        IEND = IEEE


      RETURN
      END
