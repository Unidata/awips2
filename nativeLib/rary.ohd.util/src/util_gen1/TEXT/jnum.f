C MODULE JNUM
C  ============================================================================
C  pgm: JNUM .. Get the integer value for a given character digit
C
C  use:     INTVAL = JNUM(CHRVAL)
C
C   in: CHRVAL ........ character value of a single digit - CHAR*1
C  out: INTVAL ........ integer value of the given char, else 0 - INT
C
C  cmt: Initialize JNUM to -9 or whatever if an error code number is needed.
C  ============================================================================
      INTEGER FUNCTION JNUM(CHR)

      CHARACTER*1  CHR,LET(10)
      INTEGER      NUM,NUMM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/jnum.f,v $
     . $',                                                             '
     .$Id: jnum.f,v 1.1 2000/09/27 16:05:12 page Exp $
     . $' /
C    ===================================================================
C

      DATA    LET / '0','1','2','3','4','5','6','7','8','9' /

        NUM  = 10
        JNUM = 0
  100   IF ( NUM .LE. JNUM ) GOTO 120
          NUMM = NUM-1
          IF ( CHR .EQ. LET(NUM) ) JNUM = NUMM
          NUM = NUMM
          GOTO 100
  120   CONTINUE

      RETURN
      END
