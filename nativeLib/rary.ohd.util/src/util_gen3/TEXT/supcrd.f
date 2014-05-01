C MODULE SUPCRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT AN INPUT CARD.
C
      SUBROUTINE SUPCRD
C
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/supcrd.f,v $
     . $',                                                             '
     .$Id: supcrd.f,v 1.2 1998/07/02 19:38:03 page Exp $
     . $' /
C    ===================================================================
C
C
      CALL UPRCRD (LP)
C
      RETURN
C
      END
