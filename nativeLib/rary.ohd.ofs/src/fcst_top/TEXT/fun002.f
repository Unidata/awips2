C MEMBER FUN002
C  (from old member FUN002A3)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 10/16/95.13:38:27 BY $WC20SV
C
C @PROCESS LVL(77)
C
C  THIS MODULE CONTAINS FUNCTIONS 2 AND 3.
C
C  ROUTINE FUN002 PROTECTS A DATE OF CARRYOVER
C  ROUTINE FUN003 FREES A DATE OF CARRYOVER
C
      SUBROUTINE FUN002
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fun002.f,v $
     . $',                                                             '
     .$Id: fun002.f,v 1.2 1996/01/14 17:42:48 page Exp $
     . $' /
C    ===================================================================
C
C
      IFUNCT=1
C
      CALL FUN2A3 (IFUNCT)
C
      RETURN
C
      END
