C MEMBER FUN003
C  (from old member FUN002A3)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/16/95.13:38:27 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE FUN003
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fun003.f,v $
     . $',                                                             '
     .$Id: fun003.f,v 1.2 1996/01/14 17:43:01 page Exp $
     . $' /
C    ===================================================================
C
C
      IFUNCT=2
C
      CALL FUN2A3 (IFUNCT)
C
      RETURN
C
      END
