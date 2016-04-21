C  =====================================================================
C  pgm: KKNXPO .. Get next positive or zero number from char string
C
C  use:     CALL KKNXPO( line, last, limit, number )
C
C   in: line ......... array holding packed chars
C  i/o: last ......... position of last char used
C   in: limit ........ max number of chars in input array
C  out: number ....... next positive number, else -1
C  =====================================================================
      SUBROUTINE KKNXPO(LINE,LAST,LIMIT,NUM)


      INTRINSIC      ICHAR
      CHARACTER*(*)  LINE
      CHARACTER*1    ZERO,BLANK,KHAR
      INTEGER        LAST,LIMIT,NUM,NSUM,NXNU,NZERO
      INTEGER*4      ICHAR
      PARAMETER   ( ZERO='0', BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kknxpo.f,v $
     . $',                                                             '
     .$Id: kknxpo.f,v 1.1 1995/09/17 19:02:04 dws Exp $
     . $' /
C    ===================================================================
C


        NUM = -1
  100   IF( LAST .GE. LIMIT ) GO TO 130
          LAST = LAST+1
	  KHAR = LINE(LAST:LAST)
	  IF( KHAR .EQ. BLANK ) GO TO 100

	NZERO = ICHAR(ZERO)
	NSUM  = ICHAR(KHAR)-NZERO
        IF( NSUM.LT.0 .OR. NSUM.GT.9 ) GO TO 130
  110   IF( LAST .GE. LIMIT ) GO TO 120
          LAST = LAST+1
	  KHAR = LINE(LAST:LAST)
	  IF( KHAR .EQ. BLANK ) GO TO 120
	    NXNU = ICHAR(KHAR)-NZERO
            IF( NXNU.LT.0 .OR. NXNU.GT.9 ) GO TO 130
            NSUM = 10*NSUM+NXNU
              GO TO 110
  120     NUM = NSUM

  130   CONTINUE


      RETURN
      END
