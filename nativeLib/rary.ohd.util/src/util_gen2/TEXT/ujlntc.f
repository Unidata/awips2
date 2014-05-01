C MODULE UJLNTC
C---------------------------------------------------------------------
C THIS ROUTINE WILL COMPUTE THE JULIAN HOUR MODIFIED BY THE TIME
C ZONE CODE IN ITZC SO THAT IT IS THE JULIAN HOUR EXPECTED BY THE
C FORECAST COMPONENT.  IT USES THE VARIABLES 'LOCAL AND NLSTZ' IN
C COMMON /HDFLTS/ WHICH ARE THE OFFSET BETWEEN LOCAL TIME AND
C INTERNAL TIME, AND LOCAL TIME AND 0Z RESPECTIVELY, TO MODIFY THE
C JULIAN HOUR THAT IS COMPUTED BY UJLIAN.
C---------------------------------------------------------------------
      SUBROUTINE UJLNTC (MO,ID,IY,IHR,IJUL,ITZC)

      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'udsi'
      INCLUDE 'uio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ujlntc.f,v $
     . $',                                                             '
     .$Id: ujlntc.f,v 1.2 1998/07/02 16:31:19 page Exp $
     . $' /
C    ===================================================================
C

      CALL DDGCH2 (IJUL,IY,MO,ID,IHR)

      IF (ITZC .LE. 20) THEN
        IJUL=IJUL+NLSTZ-LOCAL+ITZC
        IF (NOBUG.EQ.3) WRITE (LPD,2000) IJUL,NLSTZ,LOCAL,ITZC
2000    FORMAT (' SUB UJLNTC-IJUL=',I10,' NLSTZ=',I4,' LOCAL=',I4,
     $    ' ITZC=',I4)
      ENDIF

      RETURN
      END
