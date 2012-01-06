C MODULE UFLDRD
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ INPUT FIELD.
C
      SUBROUTINE UFLDRD (NFLD,CHAR,LCHAR,IPRINT,IERR)
C
      DIMENSION CHAR(1)
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'ufielx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/new_alt/RCS/ufldrd.f,v $
     . $',                                                             '
     .$Id: ufldrd.f,v 1.1 1998/07/06 12:50:43 page Exp $
     . $' /
C    ===================================================================
C
C
C  READ INPUT FIELD
      CALL UFIELD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
C
C  PRINT INPUT FIELD ATTRIBUTES
      IF (IPRINT.GT.0)
     * CALL UPRFLD (NFLD,ISTRT,LENGTH,ITYPE,NREP,INTEGR,REAL,LCHAR,
     *   CHAR,LLPAR,LRPAR,LASK,LATSGN,LAMPS,LEQUAL,IERR)
C
      RETURN
C
      END
