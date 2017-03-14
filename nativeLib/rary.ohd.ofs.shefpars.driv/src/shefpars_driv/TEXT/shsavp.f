C  =====================================================================
C  pgm: SHSAVP .. Buffer and save/restore parameter code
C
C  use:      CALL SHSAVP(CMD,ITEM,PARCOD)
C
C   in: CMD ....... routine command to save or retrieve p-code - CHAR*1
C   in:               'I' ... Initialize buffer parameter code to blank
C   in:               'P' ... Put parameter code into the buffer
C   in:               'G' ... Get parameter code from the buffer
C   in:               'S' ... Save buffer parameter code in array
C   in:               'R' ... Restore buffer parameter code from array
C   in: ITEM ...... field number of p-code to save or retrieve - INT
C  i/o: PARCOD .... parameter code - CHAR*8
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVP(CMD,ITEM,PARCOD)

      CHARACTER*1   CMD
      INTEGER       ITEM,II
      CHARACTER*8   PARCOD,SVPCOD,SVARRP(200)

      SAVE      SVPCOD,SVARRP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavp.f,v $
     . $',                                                             '
     .$Id: shsavp.f,v 1.2 1997/12/31 20:51:01 page Exp $
     . $' /
C    ===================================================================
C

      DATA      SVPCOD / ' ' /

        IF (CMD .EQ. 'I') THEN
            SVPCOD = ' '
 
        ELSEIF (CMD .EQ. 'P') THEN
            SVPCOD = PARCOD
 
        ELSEIF (CMD .EQ. 'G') THEN
            PARCOD = SVPCOD

        ELSEIF (CMD .EQ. 'S') THEN
             II = ITEM
            SVARRP(II) = SVPCOD

        ELSEIF (CMD .EQ. 'R') THEN
             II = ITEM
            SVPCOD = SVARRP(II)

        ENDIF

      RETURN
      END
