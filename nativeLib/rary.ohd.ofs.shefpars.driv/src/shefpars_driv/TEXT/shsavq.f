C  =====================================================================
C  pgm: SHSAVQ .. Buffer and save/restore qualifier code
C
C  use:      CALL SHSAVQ(CMD,ITEM,KWAL)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer qualifier code to 'Z'
C   in:               'P' ... Put qualifier code in buffer
C   in:               'G' ... Get qualifier code from buffer
C   in:               'S' ... Save buffer qualifier code in array
C   in:               'R' ... Restore buffer qualifier code from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: KWAL ...... one character qualifier code - CHAR*1
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVQ(CMD,ITEM,KWAL)

      CHARACTER*1   CMD,KWAL,SVKWAL,SVARRQ(200)
      INTEGER       ITEM,II

      SAVE          SVKWAL,SVARRQ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavq.f,v $
     . $',                                                             '
     .$Id: shsavq.f,v 1.2 1997/12/31 20:51:27 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVKWAL / 'Z' /

        IF (CMD .EQ. 'I') THEN
            SVKWAL = 'Z'

        ELSEIF (CMD .EQ. 'P') THEN
            SVKWAL = KWAL

        ELSEIF (CMD .EQ. 'G') THEN
            KWAL = SVKWAL

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRQ(II) = SVKWAL

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVKWAL = SVARRQ(II)

        ENDIF

      RETURN
      END
