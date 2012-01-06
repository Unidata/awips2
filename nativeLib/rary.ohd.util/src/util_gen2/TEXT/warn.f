C MODULE WARN
C-----------------------------------------------------------------------
C
      SUBROUTINE WARN
C
      CHARACTER*6 CALLER
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/warn.f,v $
     . $',                                                             '
     .$Id: warn.f,v 1.3 2000/12/18 14:25:43 edwin Exp $
     . $' /
C    ===================================================================
C
C
      CALLER='WARN'
C
      CALL ERRWRN (CALLER)
C
      RETURN
C
      END
