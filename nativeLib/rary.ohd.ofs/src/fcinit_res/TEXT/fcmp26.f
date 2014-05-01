C MEMBER FCMP26
C  (from old member FCSPRP26)
C
      SUBROUTINE FCMP26(CHRIN,NWORDS,CHROUT,NCHAR)
C
C  SUBROUTINE COUNTS THE NUMBER OF CHARACTERS IN 'NWORDS' OF
C  CHRIN AND RETURNS THE NUMBER IN 'NCHAR'.
C
C  PROGRAMMED BY KAY KROUSE  MAY 1983
C***********************************************************************
      EQUIVALENCE (BLCHK4,BLCHK(1))
      LOGICAL*1 CHRIN,CHROUT,BLCHK
      DIMENSION CHRIN(1),CHROUT(1),BLCHK(4)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/fcmp26.f,v $
     . $',                                                             '
     .$Id: fcmp26.f,v 1.1 1995/09/17 18:51:33 dws Exp $
     . $' /
C    ===================================================================
C
      DATA BLNK4/4H    /, BLCHK4/4H    /
C
      MXCHAR=NWORDS*4
      DO 10 I=1,MXCHAR
 10   CHROUT(I)=CHRIN(I)
      DO 20 I=1,MXCHAR
      J=MXCHAR+1-I
      BLCHK(1)=CHROUT(J)
      IF(BLCHK4.EQ.BLNK4)GO TO 20
      GO TO 30
 20   CONTINUE
      NCHAR=0
      GO TO 50
 30   NCHAR=J
 50   CONTINUE
      RETURN
      END
