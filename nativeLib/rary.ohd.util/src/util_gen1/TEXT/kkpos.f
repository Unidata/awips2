C  =====================================================================
C  pgm: KKPOS .. Get positive (or zero) number from given string
C
C  use:     CALL KKPOS( string, number )
C
C   in: string ....... char string of varying length - CHAR*(*)
C  out: number ....... next positive number (or zero), else -1
C
C  cmt: Number ends with first non-digit char or string limit.
C  cmt: There is no check for too long a number.
C  =====================================================================
      SUBROUTINE KKPOS(STRNG,NUMBR)


      INTRINSIC      ICHAR,LEN
      INTEGER        ICHAR,LEN

      CHARACTER*(*)  STRNG
      CHARACTER*1    KHZERO
      INTEGER        NUMBR,NEXT,LIMIT,INZERO,NUMNEX,NUMTOT

      PARAMETER   ( KHZERO='0' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kkpos.f,v $
     . $',                                                             '
     .$Id: kkpos.f,v 1.1 1995/09/17 19:02:06 dws Exp $
     . $' /
C    ===================================================================
C


        INZERO = ICHAR(KHZERO)
        LIMIT  = LEN(STRNG)
        NUMTOT = 0
        NEXT   = 0

  100   IF (NEXT .GE. LIMIT ) GOTO 110

            NEXT  = NEXT + 1
            NUMNEX = ICHAR( STRNG(NEXT:NEXT) ) - INZERO

            IF (NUMNEX.GE.0 .AND. NUMNEX.LE.9) THEN
                NUMTOT = (10 * NUMTOT) + NUMNEX
              ELSE
                LIMIT = 0
            ENDIF

            GOTO 100
  110   CONTINUE

        NUMBR = NUMTOT

      RETURN
      END
