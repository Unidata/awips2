C     MODULE INTFRC
C     ----------------------------------------------------------------
      SUBROUTINE INTFRC(RCID,STATUS);
C
C     ORIGINALLY CREATED AUG 2007
C        DARRIN SHARP, RIVERSIDE TECHNOLOGY
C
C     THIS SUBROUTINE IS A WRAPPER BETWEEN RES-J C++ CODE AND NWSRFS
C     FORTRAN.
C
      INTEGER STATUS
      DIMENSION RCID(2)

      INCLUDE 'common/frcfil'
      INCLUDE 'common/fratng'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source$
     . $',                                                             '
     .$Id$
     . $' /
C    ===================================================================
C

C     ERROR FLAGGING AND REPORTING HANDLED IN FGETRC
C     FGETRC LOADS THE RATING CURVE INTO THE FRATNG COOMON BLOCK
C     FOR USE BY STAGE-Q
      CALL FGETRC(RCID,STATUS)

      RETURN 
      END 
