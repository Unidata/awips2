C
C-----------------------------------------------------------------------
C
C @PROCESS LVL(77)
C
      SUBROUTINE MEDATI (NSTA,MONUM,LAST,NTMO,PTPE,ITUNIT)
C
C  ROUTINE TO READ CURRENT MONTH OF PTPE DATA
C
      DIMENSION EVAP(31),PTPE(25,31)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mape/RCS/medati.f,v $
     . $',                                                             '
     .$Id: medati.f,v 1.2 1998/10/14 13:28:50 page Exp $
     . $' /
C    ===================================================================
C
C
      DO 20 IRG=1,NSTA
         NUM=IRG+(MONUM-1)*NSTA
         READ (ITUNIT,REC=NUM) EVAP
         DO 10 I=1,LAST
            PTPE(IRG,I)=EVAP(I)
10          CONTINUE
20       CONTINUE
C
      RETURN
C
      END
