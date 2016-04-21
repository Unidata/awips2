      SUBROUTINE IDDB55(I,J,NUMLAD,LAD,KL,K1,K16)
C
C  THIS SUBROUTINE CORRELATES DAM/BRIDGE NUMBER WITH CROSS SECTION NUMBER
C
      INCLUDE 'common/fdbug'

      DIMENSION NUMLAD(K1),LAD(K16,K1),SNAME(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/iddb55.f,v $
     . $',                                                             '
     .$Id: iddb55.f,v 1.1 1999/04/23 18:08:34 dws Exp $
     . $' /
C    ===================================================================
C
C
      DATA SNAME/4HIDDB,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)
      NUM=NUMLAD(J)
      DO 323 L=1,NUM
      KL=L
      IF(I.EQ.IABS(LAD(L,J))) GO TO 327
  323 CONTINUE
  327 RETURN
      END
