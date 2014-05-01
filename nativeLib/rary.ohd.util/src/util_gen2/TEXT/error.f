C MODULE ERROR
C-----------------------------------------------------------------------
C
      SUBROUTINE ERROR
C
      CHARACTER*6 CALLER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/error.f,v $
     . $',                                                             '
     .$Id: error.f,v 1.3 2000/12/18 20:42:42 jgofus Exp $
     . $' /
C    ===================================================================
C
C
      CALLER='ERROR'
C
      CALL ERRWRN (CALLER)
C
      RETURN
C
      END
