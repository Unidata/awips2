C MODULE PPINMAIN
C-----------------------------------------------------------------------
C
      SUBROUTINE PPINMAIN_MAIN
C
C  MAIN ROUTINE FOR PROGRAM PPINIT.
C
      CHARACTER*100 PMFLD/' '/
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/sarryx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit/RCS/ppinmain.f,v $
     . $',                                                             '
     .$Id: ppinmain.f,v 1.6 2004/08/11 14:18:02 hank Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
      INCLUDE 'cluprimo'
C
C  BE SURE UTIL_TO_PPINIT ROUTINE LOADED BEFORE UTIL ROUTINES
      CALL UTIL_TO_PPINIT
C
C  SET OPTIONS FOR UTILITY ROUTINES
      CALL USETO1 ('NOPAGHDR',IERR)
C
C  GET PARAMETER FIELD
      CALL UGTPRM (LPMFLD,PMFLD)
C
C  CALL DRIVER ROUTINE
      CALL SUDRVR (LPMFLD,PMFLD,LARRAY,ARRAY,IERR)
      IF (ISTRCE.GT.0) WRITE (IOSDBG,*) 'SUDRVR CALLED : IERR=',IERR
C
C  END PROGRAM EXECUTION
      CALL OFSCLN
      CALL SUEND
C
      STOP
C
      END
