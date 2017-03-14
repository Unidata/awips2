C$PRAGMA C (LEAVERTN)
C  =====================================================================
C  pgm: LEAVERTNF .. api routine called by all subrtns upon exiting
C
C  use:     CALL LEAVERTNF(SNAM)
C
C   in: SNAM ..... name of calling routine as a string - CHAR*(*)
C   in:            (note, first 33 chars used)
C
C  rqd: subrtns - ENDZRO, LEAVERTN
C  =====================================================================
      SUBROUTINE LEAVERTNF(SNAM)

      CHARACTER*(*)  SNAM
      CHARACTER*34   TMSNAM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/leavertnf.f,v $
     . $',                                                             '
     .$Id: leavertnf.f,v 1.1 2002/10/10 20:12:42 dws Exp $
     . $' /
C    ===================================================================
C

        CALL ENDZRO(SNAM,TMSNAM)
        CALL LEAVERTN(TMSNAM)

      RETURN
      END
