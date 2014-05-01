C  =====================================================================
C  pgm: ENDZRO .. Put a '\0' at the end of a string
C
C  use:     CALL ENDZRO(INSTR,OUTSTR)
C
C   in: INSTR ...... input Fortran string with no '\0' - CHAR*(*)
C  out: OUTSTR ..... output C string with a '\0' placed after the last
C  out:              non-blank character - CHAR*(*)
C  out:              (note - OUTSTR should be at least one greater in
C  out:              length than INSTR)
C  =====================================================================
      SUBROUTINE ENDZRO(INSTR,OUTSTR)

      INTRINSIC      LEN,CHAR
      INTEGER        LEN
      CHARACTER*1    CHAR

      CHARACTER*(*)  INSTR,OUTSTR
      INTEGER        NEXT,LAST
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/endzro.f,v $
     . $',                                                             '
     .$Id: endzro.f,v 1.1 2002/10/10 20:10:20 dws Exp $
     . $' /
C    ===================================================================
C

        OUTSTR = INSTR
        NEXT = LEN(OUTSTR) - 1
        LAST = 1
   10   IF (NEXT .LT. LAST) GOTO 11
          IF (OUTSTR(NEXT:NEXT) .NE. ' ') LAST = NEXT+1
          NEXT = NEXT-1
          GOTO 10
   11   CONTINUE
        OUTSTR(LAST:LAST) = CHAR(0)

      RETURN
      END
