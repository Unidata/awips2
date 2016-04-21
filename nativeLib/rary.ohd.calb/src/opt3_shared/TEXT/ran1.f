C MEMBER RAN1
C  (from old member OSCEUA)
C
C=========================================================
C @PROCESS LVL(77)
      FUNCTION RAN1(IDUM)
      DIMENSION R(97)
      SAVE
      PARAMETER (M1 = 259200, IA1 = 7141, IC1 = 54773, RM1 =
     &3.8580247E-6)
      PARAMETER (M2 = 134456, IA2 = 8121, IC2 = 28411, RM2 =
     &7.4373773E-6)
      PARAMETER (M3 = 243000, IA3 = 4561, IC3 = 51349)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/opt3_shared/RCS/ran1.f,v $
     . $',                                                             '
     .$Id: ran1.f,v 1.2 1997/06/25 11:54:55 page Exp $
     . $' /
C    ===================================================================
C
      DATA IFF / 0 /
      IF ((IDUM .LT. 0) .OR. (IFF .EQ. 0)) THEN
      IFF = 1
      IX1 = MOD(IC1 - IDUM,M1)
      IX1 = MOD((IA1 * IX1) + IC1,M1)
      IX2 = MOD(IX1,M2)
      IX1 = MOD((IA1 * IX1) + IC1,M1)
      IX3 = MOD(IX1,M3)
      DO 11 J = 1, 97
      IX1 = MOD((IA1 * IX1) + IC1,M1)
      IX2 = MOD((IA2 * IX2) + IC2,M2)
      R(J) = (FLOAT(IX1) + (FLOAT(IX2) * RM2)) * RM1
   11 CONTINUE
      IDUM = 1
      END IF
      IX1 = MOD((IA1 * IX1) + IC1,M1)
      IX2 = MOD((IA2 * IX2) + IC2,M2)
      IX3 = MOD((IA3 * IX3) + IC3,M3)
      J = 1 + ((97 * IX3) / M3)
      IF ((J .GT. 97) .OR. (J .LT. 1)) PAUSE
      RAN1 = R(J)
      R(J) = (FLOAT(IX1) + (FLOAT(IX2) * RM2)) * RM1
      RETURN
      END
