C  =====================================================================
C  pgm: SHSAVD .. Buffer and save/restore data units code
C
C  use:      CALL SHSAVD(CMD,ITEM,KODU)
C
C   in: CMD ....... routine command to save or retrieve code - CHAR*1
C   in:               'I' ... Initialize buffer data units code to 1
C   in:               'P' ... Put data units code into the buffer
C   in:               'G' ... Get data units code from the buffer
C   in:               'S' ... Save buffer data units code in array
C   in:               'R' ... Restore buffer data units code from array
C   in: ITEM ...... field number of code to save or retrieve - INT
C  i/o: KODU ...... data units code (0 = English, 1= metric) - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVD(CMD,ITEM,KODU)

      CHARACTER*1   CMD
      INTEGER       ITEM,KODU,SVKODU,SVARRD(200),II

      SAVE          SVKODU,SVARRD
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavd.f,v $
     . $',                                                             '
     .$Id: shsavd.f,v 1.2 1997/12/31 20:35:41 page Exp $
     . $' /
C    ===================================================================
C

      DATA          SVKODU / 1 /

        IF (CMD .EQ. 'I') THEN
            SVKODU = 1

        ELSEIF (CMD .EQ. 'P') THEN
            SVKODU = KODU

        ELSEIF (CMD .EQ. 'G') THEN
            KODU = SVKODU

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRD(II) = SVKODU

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVKODU = SVARRD(II)

        ENDIF

      RETURN
      END
