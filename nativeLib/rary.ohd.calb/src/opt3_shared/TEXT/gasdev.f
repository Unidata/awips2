C MEMBER GASDEV
C  (from old member OSCEUA)
C
C===============================================================
C @PROCESS LVL(77)
      FUNCTION GASDEV(IDUM)
      SAVE
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/gasdev.f,v $
     . $',                                                             '
     .$Id: gasdev.f,v 1.2 1997/06/25 11:51:21 page Exp $
     . $' /
C    ===================================================================
C
      DATA ISET / 0 /
      IF (ISET .EQ. 0) THEN
    1 V1 = (2. * RAN1(IDUM)) - 1.
      V2 = (2. * RAN1(IDUM)) - 1.
      R = (V1 ** 2) + (V2 ** 2)
      IF (R .GE. 1.) GOTO 1
      FAC = SQRT(- ((2. * ALOG(R)) / R))
      GSET = V1 * FAC
      GASDEV = V2 * FAC
      ISET = 1
      ELSE
      GASDEV = GSET
      ISET = 0
      END IF
      RETURN
      END
