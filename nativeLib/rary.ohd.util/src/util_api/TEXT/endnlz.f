C  =====================================================================
C  pgm: ENDNLZ .. Put a newline and zero at the end of a string for 'C'
C
C  use:     CALL ENDNLZ(INSTR,OUTSTR)
C
C   in: INSTR ...... input Fortran string with no nl or '\0' - CHAR*(*)
C  out: OUTSTR ..... output C string with a  nl and '\0' - CHAR*(*)
C  out:              (note - OUTSTR should be at least two greater in
C  out:              length than INSTR)
C  =====================================================================
      SUBROUTINE ENDNLZ(INSTR,OUTSTR)

      INTRINSIC      LEN,CHAR
      INTEGER        LEN
      CHARACTER*1    CHAR

      CHARACTER*(*)  INSTR,OUTSTR
      INTEGER        NEXT,LAST
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/endnlz.f,v $
     . $',                                                             '
     .$Id: endnlz.f,v 1.1 2002/10/10 20:09:01 dws Exp $
     . $' /
C    ===================================================================
C

        OUTSTR = INSTR
        NEXT = LEN(OUTSTR) - 2
        LAST = 1
   10   IF (NEXT .LT. LAST) GOTO 11
          IF (OUTSTR(NEXT:NEXT) .NE. ' ') LAST = NEXT+1
          NEXT = NEXT-1
          GOTO 10
   11   CONTINUE
        OUTSTR(LAST:LAST) = CHAR(10)
        OUTSTR(LAST+1:LAST+1) = CHAR(0)

      RETURN
      END
