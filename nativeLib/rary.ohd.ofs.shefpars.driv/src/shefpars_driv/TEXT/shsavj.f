C  =====================================================================
C  pgm: SHSAVJ .. Buffer and save/restore initial duration code
C
C  use:      CALL SHSAVJ(CMD,ITEM,ICOD)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer initl dur code to 0
C   in:               'P' ... Put initl duration code into the buffer
C   in:               'G' ... Get initl duration code from the buffer
C   in:               'S' ... Save buffer initl duration code in array
C   in:               'R' ... Restore buffer initl dur code from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: ICOD ...... initial duration code - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVJ(CMD,ITEM,ICOD)

      CHARACTER*1   CMD
      INTEGER       ITEM,ICOD,SVICOD,SVARRJ(200),II

      SAVE          SVICOD,SVARRJ
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavj.f,v $
     . $',                                                             '
     .$Id: shsavj.f,v 1.2 1997/12/31 20:36:22 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVICOD / 0 /

        IF (CMD .EQ. 'I') THEN
            SVICOD = 0

        ELSEIF (CMD .EQ. 'P') THEN
            SVICOD = ICOD

        ELSEIF (CMD .EQ. 'G') THEN
            ICOD = SVICOD

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRJ(II) = SVICOD

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVICOD = SVARRJ(II)

        ENDIF

      RETURN
      END
