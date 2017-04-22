C$PRAGMA C (WRITEMSGC)
C  =====================================================================
C  pgm: WRITEMSGF .. Fortran wrapper routine to output an event message
C
C  use:     CALL WRITEMSGF(BITCOD,LINE)
C
C   in: BITCOD ..... an integer mask describing the type of message -
C   in:              INTEGER
C   in: LINE ....... a string containing the message - CHAR*(*)
C
C  rqd: subrtns - ENDNLZ, WRITEMSGC    (writemsgc is a 'C++' rtn)
C
C  cmt: The dimension of TMLINE at 1024 means the longest line possible
C  cmt:  will be 2 less than the dimension or 1022.
C  =====================================================================
      SUBROUTINE WRITEMSGF(BITCOD,LINE)

      INTEGER        BITCOD
      CHARACTER*(*)  LINE
      CHARACTER*1024 TMLINE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/writemsgf.f,v $
     . $',                                                             '
     .$Id: writemsgf.f,v 1.1 2002/10/10 20:13:36 dws Exp $
     . $' /
C    ===================================================================
C

        CALL ENDNLZ(LINE,TMLINE)
        CALL WRITEMSGC(BITCOD,TMLINE)

      RETURN
      END
