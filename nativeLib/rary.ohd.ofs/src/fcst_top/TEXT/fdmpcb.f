C MEMBER FDMPCB
C  (from old member FCDMPCBS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/31/94.15:45:43 BY $WC20SV
C
      SUBROUTINE FDMPCB(A,B,C,D,E,F)
C
      LOGICAL A,B,C,D,E,F
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fdmpcb.f,v $
     . $',                                                             '
     .$Id: fdmpcb.f,v 1.1 1995/09/17 19:08:13 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF(A)CALL FPRCBA
      IF(B)CALL FPRCBB
      IF(C)CALL FPRCBC
      IF(D)CALL FPRCBD
      IF(E)CALL FPRCBE
      IF(F)CALL FPRCBF
C
      RETURN
      END
