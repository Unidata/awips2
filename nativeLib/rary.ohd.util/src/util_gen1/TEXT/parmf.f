C MODULE PARMF
C-----------------------------------------------------------------------
C
C  ROUTINE TO SET THE CONTENTS OF THE PARAMETER FIELD.
C
      SUBROUTINE PARMF (LPARM,PARM)
C
      INCLUDE 'uiox'
      INCLUDE 'upvrsx'
C
      CHARACTER*(*) PARM
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen1/RCS/parmf.f,v $
     . $',                                                             '
     .$Id: parmf.f,v 1.2 2001/06/13 13:40:49 dws Exp $
     . $' /
C    ===================================================================
C
C
      PARM=' '
C
      IF (PGMNAM.EQ.'REORDER' ) THEN
         PARM='CHNG  OLD  NEW'
         ENDIF
C
      IF (PGMNAM.EQ.'FILECRAT' ) THEN
          PARM='NEW'
          ENDIF
C
      LPARM=LENSTR(PARM)
C
      RETURN
C
      END
