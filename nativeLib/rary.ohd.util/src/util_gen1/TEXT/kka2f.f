C  =====================================================================
C  pgm: KKA2F .. Convert a string to a real number
C
C  cmt: Note, if no decimal is in the string, an F20.4 format is used
C  cmt:  for the conversion so 4 decimal places are assumed.
C  =====================================================================
      SUBROUTINE KKA2F(WORD,LWORD,FNUM,ISTAT)

      CHARACTER*(*)   WORD
      INTEGER         LWORD,ISTAT
      REAL            FNUM,FDEFLT
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/kka2f.f,v $
     . $',                                                             '
     .$Id: kka2f.f,v 1.1 2001/06/13 09:16:53 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA      FDEFLT / 0.0 /

        ISTAT = 2
        IF (LWORD .GT. 0) THEN
          READ(WORD(1:LWORD),'(F20.4)',IOSTAT=ISTAT) FNUM
          IF (ISTAT .NE. 0) FNUM = FDEFLT
        ENDIF

      RETURN
      END
