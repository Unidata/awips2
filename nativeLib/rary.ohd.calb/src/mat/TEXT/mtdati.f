C MODULE MTDATI
C-----------------------------------------------------------------------
C
      SUBROUTINE MTDATI (MXSTA,NSTA,MONUM,LAST,NTMO,TMM,ITUNIT)
C
C  ROUTINE TO READ STATION TEMPERATURE DATA FOR PROGRAM MAT.
C
      DIMENSION TEMP(62),TMM(MXSTA,66)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/mtdati.f,v $
     . $',                                                             '
     .$Id: mtdati.f,v 1.2 1998/07/02 14:03:59 page Exp $
     . $' /
C    ===================================================================
C
C
C  READ CURRENT MONTH PLUS ONE DAY FROM FOLLOWING MONTH
C
      DO 60 IRG=1,NSTA
         NUM=IRG+(MONUM-1)*NSTA
         READ (UNIT=ITUNIT,REC=NUM) TEMP
         IL=(LAST*2)+2
         DO 10 I=3,IL
            J=I-2
            TMM(IRG,I)=TEMP(J)
10          CONTINUE
         IF (MONUM.EQ.NTMO) GO TO 40
         NUM=IRG+MONUM*NSTA
         READ (UNIT=ITUNIT,REC=NUM) TEMP
         II=IL+1
         IL=IL+2
         DO 20 I=II,IL
            J=I-II+1
            TMM(IRG,I)=TEMP(J)
20          CONTINUE
         IF (MONUM.GT.1) GO TO 60
         DO 30 I=1,2
            TMM(IRG,I)=TMM(IRG,(I+2))
30          CONTINUE
         GO TO 60
40       II=IL+1
         IL=IL+2
         DO 50 I=II,IL
            TMM(IRG,I)=TMM(IRG,(I-2))
50          CONTINUE
60       CONTINUE
C
      RETURN
C
      END
