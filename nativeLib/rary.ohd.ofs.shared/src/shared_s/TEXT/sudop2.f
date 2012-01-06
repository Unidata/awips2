C MEMBER SUDOP2
C  (from old member SUDOPN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 04/04/95.10:30:26 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SUDOP2 (NLSTZH,LOCALH)
C
C  THIS ROUTINE IS NEEDED BECAUSE VARIABLES IN COMMON BLOCKS
C  HDFLTS AND FCTIME ARE THE SAME
C
      INCLUDE 'uio'
      INCLUDE 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sudop2.f,v $
     . $',                                                             '
     .$Id: sudop2.f,v 1.1 1995/09/17 19:21:48 dws Exp $
     . $' /
C    ===================================================================
C
C
C
C  SET VARIABLES IN COMMON FCTIME
C
CCC   WRITE (LP,*) 'NLSTZH=',NLSTZH
      NLSTZ=NLSTZH
CCC   WRITE (LP,*) 'NLSTZ=',NLSTZ
C
CCC   WRITE (LP,*) 'LOCALH=',LOCALH
      LOCAL=LOCALH
CCC   WRITE (LP,*) 'LOCAL=',LOCAL
C
      RETURN
C
      END
