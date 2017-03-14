C  =====================================================================
C  pgm: KKNXWD .. Get next word from char string
C
C  use:     CALL KKNXWD( line, last, limit, wrd, wrd.lim, wrd.len )
C
C   in: line ......... array holding packed chars
C  i/o: last ......... position of last char used
C   in: limit ........ max number of chars in input array
C  out: wrd .......... array for next word found else all blank, unused
C  out:                  spaces at end of array are filled with blanks
C   in: wrd.lim ...... max number of chars in output array
C  out: wrd.len ...... actual number of chars in found word else zero
C  out:                  (note, this can be larger than "wrd.lim" if
C  out:                  the found word is longer than the output array)
C
C  cmt: Note, if word is longer than the output word limit, the end of
C  cmt:   the word will be truncated.
C  cmt: Note, calling routine may check for "wrd.len" greater than
C  cmt:   "wrd.lim" for a possible error (output array will truncate).
C  =====================================================================
      SUBROUTINE KKNXWD(LINE,LAST,LIMIT,WORD,WLIM,WLEN)


      CHARACTER*(*)  LINE,WORD
      CHARACTER*1    KHAR,BLANK
      INTEGER        LAST,LIMIT,WLIM,WLEN,II
      PARAMETER   ( BLANK=' ' )
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kknxwd.f,v $
     . $',                                                             '
     .$Id: kknxwd.f,v 1.1 1995/09/17 19:02:05 dws Exp $
     . $' /
C    ===================================================================
C


        II = 0
  100   IF( LAST .GE. LIMIT ) GO TO 120
          LAST = LAST+1
	  KHAR = LINE(LAST:LAST)
	  IF( KHAR .EQ. BLANK ) GO TO 100

  110   II = II+1
	IF( II .LE. WLIM ) WORD(II:II) = KHAR
        IF( LAST .GE. LIMIT ) GO TO 120
          LAST = LAST+1
	  KHAR = LINE(LAST:LAST)
	  IF( KHAR .NE. BLANK ) GO TO 110

  120   WLEN = II

  130   IF( II .GE. WLIM ) GO TO 140
          II = II+1
	  WORD(II:II) = BLANK
            GO TO 130
  140   CONTINUE


      RETURN
      END
