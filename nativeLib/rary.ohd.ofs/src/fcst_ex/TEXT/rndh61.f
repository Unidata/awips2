C*********************************************************************
      SUBROUTINE RNDH61 (GHM,M)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ex/RCS/rndh61.f,v $
     . $',                                                             '
     .$Id: rndh61.f,v 1.1 1998/07/02 17:30:10 page Exp $
     . $' /
C    ===================================================================
C
C
C          ROUND MAXIMUMS TO NEXT HIGHER HALF FOOT
C          AND MINIMUMS TO NEXT LOWER HALF FOOT
C
C               M =  1 FOR MAXIMUM
C               M = -1 FOR MINIMUM
C
      S=SIGN(1.,GHM)
      AGHM=ABS(GHM)
      IGHM=INT(AGHM)
      DIFF=AGHM-REAL(IGHM)
      IF(DIFF.EQ.0.) RETURN
      IF((M.GE.0.AND.S.EQ.1.).OR.(M.LT.0.AND.S.EQ.-1.)) THEN
C
C               (THIS GROUP HANDLES POSITIVE MAXIMUMS
C                AND NEGATIVE MINIMUMS)
C
            IF(DIFF.GT.0.5) THEN
                  GHM=S*(REAL(IGHM)+1.)
               ELSE
                  GHM=S*(REAL(IGHM)+0.5)
            END IF
C
         ELSE
C
C               (THIS GROUP HANDLES NEGATIVE MAXIMUMS
C                AND POSITIVE MINIMUMS)
C
            IF(DIFF.GT.0.5) THEN
                  GHM=S*(REAL(IGHM)+0.5)
               ELSE
                  GHM=S*REAL(IGHM)
            END IF
C
      END IF
      RETURN
      END
