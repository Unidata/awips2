C MODULE SUBLID
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/30/95.07:55:26 BY $WC20SV
C
C @PROCESS LVL(77)
C
C DESC: ROUTINE TO SET BLANK IDENTIFIER TO '**NONE**'
C
      SUBROUTINE SUBLID (ID,ISTAT)
C
C
      CHARACTER*8 ID
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/sublid.f,v $
     . $',                                                             '
     .$Id: sublid.f,v 1.1 1995/09/17 19:21:38 dws Exp $
     . $' /
C    ===================================================================
C
C
C
      ISTAT=0
C
C  CHECK IF IDENTIFIER IS BLANK
      IF (ID.EQ.' ') THEN
         ID='**NONE**'
         ELSE
            ISTAT=1
         ENDIF
C
      RETURN
C
      END
