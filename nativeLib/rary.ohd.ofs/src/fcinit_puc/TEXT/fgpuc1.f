C MEMBER FGPUC1
C  (from old member FCPUC1)
C-----------------------------------------------------------------------
C                             LAST UPDATE: 10/31/95.11:00:49 BY $WC21DT
C
C @PROCESS LVL(77)
C
      SUBROUTINE FGPUC1(PF,CF)
C.......................................
C     THIS IS THE PUNCHED CARD SUBROUTINE FOR THE FROZEN GROUND PORTION
C          OF THE 'SAC-SMA' OPERATION.
C.......................................
C     SUBROUTINE INITIALLY WRITTEN BY ....
C           ERIC ANDERSON - HRL   APRIL 1981
C.......................................
      DIMENSION PF(*),CF(*)
      DIMENSION CHAR(5)
C
C     COMMON BLOCKS
      COMMON/IONUM/IN,IPR,IPU
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_puc/RCS/fgpuc1.f,v $
     . $',                                                             '
     .$Id: fgpuc1.f,v 1.2 1996/01/17 22:16:51 page Exp $
     . $' /
C    ===================================================================
C
C
C     DATA STATEMENTS
      DATA BLANK/4H    /
C.......................................
C     PUNCH CARD NO. 1
      ITTA=PF(4)
      LWE=PF(5)
      IF (LWE.EQ.0) GO TO 101
      CHAR(1)=PF(LWE)
      CHAR(2)=PF(LWE+1)
      CHAR(3)=PF(LWE+2)
      ITWE=PF(LWE+3)
      GO TO 100
  101 DO 102 I=1,3
  102 CHAR(I)=BLANK
      ITWE=0
  100 LFI=PF(6)
      IF(LFI.EQ.0) GO TO 103
      CHAR(4)=PF(LFI)
      CHAR(5)=PF(LFI+1)
      ITFI=PF(LFI+2)
      GO TO 105
  103 CHAR(4)=BLANK
      CHAR(5)=BLANK
      ITFI=0
  105 WRITE(IPU,901)(PF(I),I=1,3),ITTA,(CHAR(I),I=1,3),ITWE,CHAR(4),
     1CHAR(5),ITFI
  901 FORMAT(2X,2A4,1X,A4,3X,I2,17X,2A4,1X,A4,3X,I2,2X,2A4,3X,I2)
C.......................................
C     PUNCH CARD NO. 2
      WRITE(IPU,902) (PF(I),I=8,17)
  902 FORMAT(3F5.3,F5.2,F5.1,F5.3,F5.1,3F5.0)
C.......................................
C     PUNCH CARD NO. 3
      NX=PF(7)
      NC=NX+6
      WRITE(IPU,903) (CF(I),I=1,NC)
  903 FORMAT(F5.1,7F5.0)
C.......................................
      RETURN
      END
