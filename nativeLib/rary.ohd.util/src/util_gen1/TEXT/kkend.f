C  =====================================================================
C  pgm: KKEND .. Get end non-blank char location in string
C
C  use:     CALL KKEND(STRNG,IEND)
C
C   in: STRNG ... input string as character data - CHAR*(*)
C  out: IEND .... location of last non-blank char, else 1 - INT
C  =====================================================================
      SUBROUTINE KKEND(STRNG,IEND)

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*(*)  STRNG
      CHARACTER*1    BLANK
      INTEGER        IEND,MXSTR,NEXT

      PARAMETER( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kkend.f,v $
     . $',                                                             '
     .$Id: kkend.f,v 1.1 1998/10/14 14:11:01 page Exp $
     . $' /
C    ===================================================================
C

        MXSTR = 1
        NEXT = LEN(STRNG)
  100   IF (NEXT .LE. MXSTR) GOTO 110
          IF (STRNG(NEXT:NEXT) .NE. BLANK) MXSTR = NEXT
          NEXT = NEXT - 1
           GOTO 100
  110   CONTINUE

        IEND = MXSTR

      RETURN
      END
