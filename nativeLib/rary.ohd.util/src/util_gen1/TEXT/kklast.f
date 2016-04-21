C  =====================================================================
C  pgm: KKLAST .. Function to get the last non-blank char in a string
C
C  use:     LPOS = KKLAST(MIN,STRNG)
C
C   in: MIN ........ minimum value returnes for a blank string,
C   in:              usually 0 or 1 (1 for writing string) - INT
C   in: STRNG ...... character string - CHAR*(*)
C  out: KKLAST ..... number of location of last non-blank character
C  out:              in the given string, if all blank return MIN - INT
C  =====================================================================
      INTEGER FUNCTION KKLAST(MIN,STRNG)

      INTRINSIC      LEN
      INTEGER        LEN

      CHARACTER*(*)  STRNG
      INTEGER        MIN,II
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kklast.f,v $
     . $',                                                             '
     .$Id: kklast.f,v 1.1 2001/06/13 09:03:04 mgm Exp $
     . $' /
C    ===================================================================
C

        II = LEN(STRNG)
        KKLAST = MIN
  100   IF (II .LE. KKLAST) GOTO 110
          IF (STRNG(II:II) .NE. ' ') KKLAST = II
          II = II-1
          GOTO 100
  110   CONTINUE

      RETURN
      END
