C MODULE UGTPRM
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET THE CONTENTS OF THE PARAMETER FIELD.
C
      SUBROUTINE UGTPRM (LPMFLD,PMFLD)
C
      CHARACTER*(*) PMFLD
C
      INCLUDE 'ucmdbx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen2/RCS/ugtprm.f,v $
     . $',                                                             '
     .$Id: ugtprm.f,v 1.2 2001/06/13 13:42:09 dws Exp $
     . $' /
C    ===================================================================
C
C
C  GET PARM FIELD
      CALL PARMF (LPMFLD,PMFLD)

      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT UGTPRM - ',
     *      ' LPMFLD=',LPMFLD,
     *      ' PMFLD=',PMFLD
         ENDIF
C
      RETURN
C
      END
