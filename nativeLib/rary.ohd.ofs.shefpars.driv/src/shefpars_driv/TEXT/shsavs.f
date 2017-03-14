C  =====================================================================
C  pgm: SHSAVS .. Buffer and save/restore daylight svg code
C
C  use:      CALL SHSAVS(CMD,ITEM,LSVG)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer daylight svg code to 0
C   in:               'P' ... Put daylight svg code into the buffer
C   in:               'G' ... Get daylight svg code from the buffer
C   in:               'S' ... Save buffer daylight svg code in array
C   in:               'R' ... Restore buffer daylite svg code from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: LSVG ...... daylight savings code - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVS(CMD,ITEM,LSVG)

      CHARACTER*1   CMD
      INTEGER       ITEM,LSVG,SVLSVG,SVARRA(200),II

      SAVE          SVLSVG,SVARRA
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavs.f,v $
     . $',                                                             '
     .$Id: shsavs.f,v 1.2 1997/12/31 20:51:46 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVLSVG / 0 /

        IF (CMD .EQ. 'I') THEN
            SVLSVG = 0

        ELSEIF (CMD .EQ. 'P') THEN
            SVLSVG = LSVG

        ELSEIF (CMD .EQ. 'G') THEN
            LSVG = SVLSVG

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRA(II) = SVLSVG

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVLSVG = SVARRA(II)

        ENDIF

      RETURN
      END
