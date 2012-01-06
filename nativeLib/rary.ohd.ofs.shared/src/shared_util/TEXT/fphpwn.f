C MODULE FPHPWN
C-----------------------------------------------------------------------
C
C  THIS ROUTINE IS USED AFTER A CALL TO HPASTA OR HPAST TO PRINT A
C  WARNING MESSAGE AND CALL WARN IF THE STATUS CODE IS GREATER THAN
C  ZERO.
C
      SUBROUTINE FPHPWN (ISTAT,TECHNM)
C
      CHARACTER*8 TECHNM
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/fphpwn.f,v $
     . $',                                                             '
     .$Id: fphpwn.f,v 1.2 1999/04/23 19:46:47 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTAT.EQ.0) GO TO 50
C
      IF (ISTAT.EQ.1) THEN
         WRITE (IPR,10) TECHNM,TECHNM
10    FORMAT('0**WARNING** THE ARRAY PASSED TO ROUTINE HPASTA ',
     1    'TO HOLD ARGUMENTS FOR TECHNIQUE ',A,
     *    ' IS NOT LARGE ENOUGH OR ' /
     2 13X,'ROUTINE HPAST WAS CALLED AND TECHNIQUE ',A,
     3    ' HAS SOME ARGUMENTS.')
         GO TO 40
         ENDIF
C
      IF (ISTAT.EQ.2) THEN
         WRITE (IPR,20) TECHNM
20    FORMAT ('0**WARNING** TECHNIQUE ',A,' IS NOT DEFINED ',
     1 'FOR THIS FUNCTION.')
         GO TO 40
        ENDIF
C
      WRITE (IPR,30) ISTAT,TECHNM
30    FORMAT('0**WARNING** STATUS CODE OF ',I3,
     1 ' RETURNED FROM HPASTA OR HPAST FOR TECHNIQUE ',A,'.')
C
40    CALL WARN
C
50    RETURN
C
      END
