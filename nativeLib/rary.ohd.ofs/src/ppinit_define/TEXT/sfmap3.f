C MEMBER SFMAP3
C  (from old member SFMAP)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/09/95.12:18:40 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE SFMAP3 (NPCPN,ID5,ID6,IFLAG)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sworkx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_define/RCS/sfmap3.f,v $
     . $',                                                             '
     .$Id: sfmap3.f,v 1.1 1995/09/17 19:11:09 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IFLAG.LE.0) GO TO 30
      I=1
      KID=1
      ICOUNT=(NPCPN*2)+1
10    IF (I.LE.ICOUNT) THEN
         WRITE(LP,20)SWORK(ID5+KID-1),SWORK(ID5+KID),SWORK(ID6+I-1)
         KID=KID+2
         I=I+1
         GO TO 10
      ENDIF
20    FORMAT(2X,A4,A4,3X,F7.2)
30    CONTINUE
C
      RETURN
C
      END
