C MEMBER FGCOX1
C  (from old member FCCOX1)
C
      SUBROUTINE FGCOX1(PO,CO,PN,CN)
C.......................................
C     THIS IS THE CARRYOVER TRANSFER SUBROUTINE FOR THE FROZEN GROUND
C          PORTION OF THE 'SAC/SMA' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ....
C           ERIC ANDERSON - HRL  APRIL 1981
C.......................................
      DIMENSION PO(1),CO(1),PN(1),CN(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_cox/RCS/fgcox1.f,v $
     . $',                                                             '
     .$Id: fgcox1.f,v 1.1 1995/09/17 18:47:35 dws Exp $
     . $' /
C    ===================================================================
C
C.......................................
C     KEEP OLD VALUES FOR ALL VARIABLES.
      NC=7
      LN=PN(5)
      LO=PO(5)
      IF((LN.GT.0).AND.(LO.GT.0)) NC=8
      DO 100 I=1,8
  100 CN(I)=CO(I)
C.......................................
      RETURN
      END
