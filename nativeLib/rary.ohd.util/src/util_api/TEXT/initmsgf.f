C$PRAGMA C (INITMSGC)
C  =====================================================================
C  pgm: INITMSGF .. Fortran wrapper routine to initialize messg params
C
C  use:     CALL INITMSGF(EVNUM,EVENTS,FILNAM,RQDLVS,EVLEN)
C
C   in: EVNUM ..... number of event types (currently 11) - INTEGER
C   in: EVENTS .... 10-char strings naming each type of event - CHAR*110
C   in: FILNAM .... pathname of output file for event messages
C   in: RQDLVS .... priority number of message event type (lower number
C   in:             has higher priority of being printed) - INTEGER
C   in: EVLEN ..... max number of characters in an event name - INTEGER
C
C  rqd: subrtns - ENDZRO, INITMSGC    (initmsgc is a 'C++' rtn)
C  =====================================================================
      SUBROUTINE INITMSGF(EVNUM,EVENTS,FILNAM,RQDLVS,EVLEN)

      INTEGER          EVNUM,EVLEN,RQDLVS(*)
      CHARACTER*(*)    FILNAM,EVENTS
      CHARACTER*160    TFILNM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_api/RCS/initmsgf.f,v $
     . $',                                                             '
     .$Id: initmsgf.f,v 1.1 2002/10/10 20:12:09 dws Exp $
     . $' /
C    ===================================================================
C

        CALL ENDZRO(FILNAM,TFILNM)
        CALL INITMSGC(EVNUM,EVENTS,TFILNM,RQDLVS,EVLEN)

      RETURN
      END
