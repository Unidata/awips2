C  =====================================================================
C  pgm: SHSAVK .. Buffer and save/restore creation dates (K-dates)
C
C  use:      CALL SHSAVK(CMD,ITEM,KYR,KMO,KDA,KHR,KMN,KSE)
C
C   in: CMD ....... routine command to save or retrieve dates - CHAR*1
C   in:               'I' ... Initialize buffer K-date to 0
C   in:               'P' ... Put (y,m,d,h,n,s) into the buffer K-date
C   in:               'G' ... Get (y,m,d,h,n,s) from the buffer K-date
C   in:               'S' ... Save buffer K-date in an array
C   in:               'R' ... Restore buffer K-date from array
C   in: ITEM ...... field number of dates to save or retrieve - INT
C  i/o: KYR ....... year number (1753-2199) - INT
C  i/o: KMO ....... month number (1-12) - INT
C  i/o: KDA ....... day number (1-31) - INT
C  i/o: KHR ....... hour number (0-24) - INT
C  i/o: KMN ....... minute number (0-59) - INT
C  i/o: KSE ....... second number (0-59) - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVK(CMD,ITEM,KYR,KMO,KDA,KHR,KMN,KSE)

      CHARACTER*1   CMD
      INTEGER       ITEM,KYR,KMO,KDA,KHR,KMN,KSE,II
      INTEGER       SVKYR,SVKMO,SVKDA,SVKHR,SVKMN,SVKSE,SVARRK(1200)

      SAVE          SVKYR,SVKMO,SVKDA,SVKHR,SVKMN,SVKSE,SVARRK
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavk.f,v $
     . $',                                                             '
     .$Id: shsavk.f,v 1.3 1998/04/07 19:15:36 page Exp $
     . $' /
C    ===================================================================
C

      DATA    SVKYR,SVKMO,SVKDA,SVKHR,SVKMN,SVKSE / 0,0,0,0,0,0 /

        IF (CMD .EQ. 'I') THEN
            SVKYR = 0
            SVKMO = 0
            SVKDA = 0
            SVKHR = 0
            SVKMN = 0
            SVKSE = 0
 
        ELSEIF (CMD .EQ. 'P') THEN
            SVKYR = KYR
            SVKMO = KMO
            SVKDA = KDA
            SVKHR = KHR
            SVKMN = KMN
            SVKSE = KSE

        ELSEIF (CMD .EQ. 'G') THEN
            KYR = SVKYR
            KMO = SVKMO
            KDA = SVKDA
            KHR = SVKHR
            KMN = SVKMN
            KSE = SVKSE

        ELSEIF (CMD .EQ. 'S') THEN
             II = (ITEM-1)*6
            SVARRK(II+1) = SVKYR
            SVARRK(II+2) = SVKMO
            SVARRK(II+3) = SVKDA
            SVARRK(II+4) = SVKHR
            SVARRK(II+5) = SVKMN
            SVARRK(II+6) = SVKSE

        ELSEIF (CMD .EQ. 'R') THEN
             II = (ITEM-1)*6
            SVKYR = SVARRK(II+1)
            SVKMO = SVARRK(II+2)
            SVKDA = SVARRK(II+3)
            SVKHR = SVARRK(II+4)
            SVKMN = SVARRK(II+5)
            SVKSE = SVARRK(II+6)

        ENDIF

      RETURN
      END
