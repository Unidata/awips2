C$PRAGMA C (ENTERRTN)
C  =====================================================================
C  pgm: ENTERRTNF .. api routine called by all subrtns upon entrance
C
C  use:     CALL ENTERRTNF(SNAM,SVER)
C
C   in: SNAM ..... name of calling routine as a string - CHAR*(*)
C   in:            (note, first 33 chars used)
C   in: SVER ..... version or other info as a string - CHAR*(*)
C   in:            (note, first 69 chars used)
C
C  rqd: subrtns - ENDZRO, ENTERRTN
C  =====================================================================
      SUBROUTINE ENTERRTNF(SNAM,SVER)

      CHARACTER*(*)  SNAM,SVER
      CHARACTER*34   TMSNAM
      CHARACTER*70   TMSVER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/enterrtnf.f,v $
     . $',                                                             '
     .$Id: enterrtnf.f,v 1.1 2002/10/10 20:11:29 dws Exp $
     . $' /
C    ===================================================================
C

        CALL ENDZRO(SNAM,TMSNAM)
        CALL ENDZRO(SVER,TMSVER)
        CALL ENTERRTN(TMSNAM,TMSVER)

      RETURN
      END
