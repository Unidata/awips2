C  =====================================================================
C  pgm: FMTDIG .. Function to get the format digit for an F field
C
C  use:     FORMT(x:x) = FMTDIG(REALNM,MAXFLD)
C
C   in: REALNM ......... real value to determine the number of decimal
C   in:                  places
C   in: MAXFLD ......... total size of field (about 4 to max of 10)
C  out: FORMT(x:x) ..... single character digit in position x of an
C  out:                  F format field
C  =====================================================================
      CHARACTER*1 FUNCTION FMTDIG(VALUE,MAXFLD)

      REAL          VALUE,TMPVAL
      INTEGER       MAXFLD,MAXUSD,IDIG,JDIG
      CHARACTER*1   DIGITS(10)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/fmtdig.f,v $
     . $',                                                             '
     .$Id: fmtdig.f,v 1.1 2000/12/19 14:55:56 jgofus Exp $
     . $' /
C    ===================================================================
C

      DATA    DIGITS / '0','1','2','3','4','5','6','7','8','9' /

        MAXUSD = MAXFLD - 1
        IF (MAXUSD .GT. 9) MAXUSD = 9
        IF (MAXUSD .LT. 4) MAXUSD = 4

        TMPVAL = ABS(VALUE)
        IDIG   = 0
        IF (TMPVAL .GT. 0.0) THEN
          JDIG = 2 - INT( LOG10(TMPVAL) )
          IF (JDIG .GE. MAXUSD) JDIG = MAXUSD
          IF (JDIG .GT.      0) IDIG = JDIG
        ENDIF

        FMTDIG = DIGITS(IDIG+1)

      RETURN
      END
