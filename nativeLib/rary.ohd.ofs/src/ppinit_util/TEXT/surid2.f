C MEMBER SURID2
C  (from old member SURIDS)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 12/22/94.15:40:05 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SURID2 (NWORDS,IPOS,ISSTA,ISTATE,ISRTBY,ISPTR,IPNTRS)
C
      INTEGER*2 ISTATE(1)
C
      DIMENSION ISRTBY(NWORDS,1),IPNTRS(1)
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/surid2.f,v $
     . $',                                                             '
     .$Id: surid2.f,v 1.1 1995/09/17 19:15:55 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ISSTA.EQ.0) GO TO 10
         ITEMP=ISTATE(IPOS+1)
         ISTATE(IPOS+1)=ISTATE(IPOS)
         ISTATE(IPOS)=ITEMP
C
10    DO 20 N=1,NWORDS
         ITEMP=ISRTBY(N,IPOS+1)
         ISRTBY(N,IPOS+1)=ISRTBY(N,IPOS)
         ISRTBY(N,IPOS)=ITEMP
20       CONTINUE
C
      IF (ISPTR.EQ.0) GO TO 30
         ITEMP=IPNTRS(IPOS+1)
         IPNTRS(IPOS+1)=IPNTRS(IPOS)
         IPNTRS(IPOS)=ITEMP
C
30    RETURN
C
      END
