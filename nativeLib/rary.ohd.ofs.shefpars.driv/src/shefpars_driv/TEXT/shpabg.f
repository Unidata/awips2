C  =====================================================================
C  pgm: SHPABG .. Find the next parameter set in the shef-parameter file
C
C  use:     CALL SHPABG(LUNP,KHFIND,IERR)
C
C   in: LUNP ...... logical unit number of "shefparm" file - INT
C   in: KHFIND .... characters defining the desired parm set - CHAR*2
C   in:             (examples "*1", "*2", "**")
C  out: IERR ...... search status: 0 found; neg not found; pos err - INT
C   in: (file) .... sequential access file called "shefparm" - INT
C  =====================================================================
      SUBROUTINE SHPABG(LUNP,KHFIND,IERR)

      CHARACTER*2    KHFIND,KHNEXT
      INTEGER        LUNP,IERR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shefpars_driv/RCS/shpabg.f,v $
     . $',                                                             '
     .$Id: shpabg.f,v 1.4 1998/07/22 12:34:42 page Exp $
     . $' /
C    ===================================================================
C

        KHNEXT = '  '

        IERR = 0
        IF (LUNP .LT. 0) IERR = 99
        IF (IERR .EQ. 0) REWIND (LUNP,IOSTAT=IERR)
        IF (IERR .EQ. 0) READ(LUNP,'(A2)',IOSTAT=IERR) KHNEXT

  120   IF (IERR.NE.0 .OR. KHNEXT.EQ.KHFIND) GOTO 130
            READ(LUNP,'(A2)',IOSTAT=IERR) KHNEXT
            GOTO 120
  130   CONTINUE

      RETURN
      END
