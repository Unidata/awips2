C MODULE MTMAIN
C-----------------------------------------------------------------------
C
      SUBROUTINE MAT_MAIN
C
C  MAIN ROUTINE FROM PROGRAM MAT3
C
      INCLUDE 'uiox'
      INCLUDE 'scommon/sudbgx'
C
      INTEGER   ITUNIT
      CHARACTER FILNAM*1,FILFMT*1
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mat/RCS/mtmain.f,v $
     . $',                                                             '
     .$Id: mtmain.f,v 1.3 2004/08/10 15:04:30 dsa Exp $
     . $' /
C    ===================================================================
C
C
C     Subroutine ARGVER outputs the version/date info and exits the
C      program if the first command line argument is "-version"
C
      CALL ARGVER()
C
C  DEFINE VARIABLES IN COMMON BLOCKS - MOSTLY I/O UNITS
      CALL UPRIMO_MAT()
C
C  SET OPTIONS FOR UTILITY ROUTINES AND PRINT PAGE HEADER
      CALL USETO1 ('NOOVERPRINT',IERR)
      CALL USETO1 ('NOPAGNUM',IERR)
      CALL UPAGE (LP)
      CALL USETO1 ('NOPAGHDR',IERR)
C
      ISTRCE=0
C
C  OPEN TEMPORARY DIRECT ACCESS FILE
      ITUNIT=19
      FILNAM=' '
      FILFMT='U'
      CALL UPOPEN (ITUNIT,FILNAM,62,FILFMT,IERR)
C
      CALL MATMN (ITUNIT)
C
      CALL UPCLOS (ITUNIT,' ',ISTAT)
C
      IUSTOP=0
      CALL USTOP (LP,IUSTOP)
C
      END
