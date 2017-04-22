C MODULE CCLOSL
C-----------------------------------------------------------------------
C
C  ROUTINE TO CLOSE ALL DATACARD FILES.
C
      SUBROUTINE CCLOSL
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/cclosl.f,v $
     . $',                                                             '
     .$Id: cclosl.f,v 1.4 1999/04/22 13:42:17 page Exp $
     . $' /
C    ===================================================================
C
      IUNIT=0
      CALL CLFILE ('DATACARD-TS',IUNIT,IERR)
C
      RETURN
C      
      END
