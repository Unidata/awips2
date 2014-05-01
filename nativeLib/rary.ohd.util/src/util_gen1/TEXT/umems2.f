C MEMBER UMEMS2
C-----------------------------------------------------------------------
C
      SUBROUTINE UMEMS2 (IVAL,IARRAY,ISTART,NUM)
C
C  INITIALIZE INTEGER*2 VARIABLE WITH SPECIFIED VALUE BEGINNING AT THE
C  POSITION ISTART
C
      INTEGER*2 IARRAY(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/umems2.f,v $
     . $',                                                             '
     .$Id: umems2.f,v 1.1 1995/09/17 19:02:16 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (NUM.LE.0) GO TO 20
C
      IEND=ISTART+NUM-1
      IF (IEND.LT.ISTART) GO TO 20
C
      DO 10 I=ISTART,IEND
         IARRAY(I)=IVAL
10       CONTINUE
C
20    RETURN
C
      END
