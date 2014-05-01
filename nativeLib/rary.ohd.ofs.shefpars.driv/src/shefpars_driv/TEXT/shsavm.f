C  =====================================================================
C  pgm: SHSAVM .. Buffer and save/restore M-date increments
C
C  use:      CALL SHSAVM(CMD,ITEM,MYR,MMO,MDA,MHR,MMN,MSE,MEN)
C
C   in: CMD ....... routine command to save or retrieve data - CHAR*1
C   in:               'I' ... Initialize buffer M-date to 0
C   in:               'P' ... Put (y,m,d,h,n,s) into the buffer M-date
C   in:               'G' ... Get (y,m,d,h,n,s) from the buffer M-date
C   in:               'S' ... Save buffer M-date in an array
C   in:               'R' ... Restore buffer M-date from array
C   in: ITEM ...... field number of incrmnts to save or retrieve - INT
C  i/o: MYR ....... year number increment - INT
C  i/o: MMO ....... month number increment - INT
C  i/o: MDA ....... day number increment - INT
C  i/o: MHR ....... hour number increment - INT
C  i/o: MMN ....... minute number increment - INT
C  i/o: MSE ....... second number increment - INT
C  i/o: MEN ....... end of month increment - INT
C
C  cmt: Hardwired for 200 fields (or items).
C  =====================================================================
      SUBROUTINE SHSAVM(CMD,ITEM,MYR,MMO,MDA,MHR,MMN,MSE,MEN)

      CHARACTER*1   CMD
      INTEGER       ITEM,MYR,MMO,MDA,MHR,MMN,MSE,MEN,II
      INTEGER       SVMYR,SVMMO,SVMDA,SVMHR,SVMMN,SVMSE,SVMEN
      INTEGER       SVARRM(1400)

      SAVE          SVMYR,SVMMO,SVMDA,SVMHR,SVMMN,SVMSE,SVMEN,SVARRM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shsavm.f,v $
     . $',                                                             '
     .$Id: shsavm.f,v 1.3 1998/06/30 15:22:57 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  SVMYR,SVMMO,SVMDA,SVMHR,SVMMN,SVMSE,SVMEN / 0,0,0,0,0,0,0 /

        IF (CMD .EQ. 'I') THEN
            SVMYR = 0
            SVMMO = 0
            SVMDA = 0
            SVMHR = 0
            SVMMN = 0
            SVMSE = 0
            SVMEN = 0

        ELSEIF (CMD .EQ. 'P') THEN
            SVMYR = MYR
            SVMMO = MMO
            SVMDA = MDA
            SVMHR = MHR
            SVMMN = MMN
            SVMSE = MSE
            SVMEN = MEN
 
        ELSEIF (CMD .EQ. 'G') THEN
            MYR = SVMYR
            MMO = SVMMO
            MDA = SVMDA
            MHR = SVMHR
            MMN = SVMMN
            MSE = SVMSE
            MEN = SVMEN

        ELSEIF (CMD .EQ. 'S') THEN
             II = (ITEM-1)*7
            SVARRM(II+1) = SVMYR
            SVARRM(II+2) = SVMMO
            SVARRM(II+3) = SVMDA
            SVARRM(II+4) = SVMHR
            SVARRM(II+5) = SVMMN
            SVARRM(II+6) = SVMSE
            SVARRM(II+7) = SVMEN

        ELSEIF (CMD .EQ. 'R') THEN
             II = (ITEM-1)*7
            SVMYR = SVARRM(II+1)
            SVMMO = SVARRM(II+2)
            SVMDA = SVARRM(II+3)
            SVMHR = SVARRM(II+4)
            SVMMN = SVARRM(II+5)
            SVMSE = SVARRM(II+6)
            SVMEN = SVARRM(II+7)

        ENDIF

      RETURN
      END
