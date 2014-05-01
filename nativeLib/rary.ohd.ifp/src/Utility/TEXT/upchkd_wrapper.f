C  =====================================================================
C  pgm: upchkd_wrapper .. Wrapper routine to call upchkd from a "c" pgm
C
C  use:     (void) upchkd_wrapper(glodir,&lenstr,&istat);
C
C   in: glodir ........ string of words indicating which tokens to use
C   in:                 to see if its directory exists; capital letters
C   in:                 may be used; example: 'syst  OPER' - CHAR*(*)
C   in:                      word         token
C   in:                      ----         ---------------
C   in:                      syst         rfs_sys_dir
C   in:                      oper         ofs_fs5files
C   in:                      mods         ofs_mods_dir
C   in:                      grid         ofs_griddb_dir
C   in:                      reor         ofs_reorder_dir
C   in: lenstr ........ length of the string, glodir (not counting
C   in:                 the trailing "\0" - INT
C  out: istat .........output status; - INT
C  out:                    0 = all pathname directories exist
C  out:                    1 = pathname does not exist for input word
C  out:                    2 = bad input word
C  out:                    4 = a pathname directory does not exist
C
C  rqd: upchkd
C  =====================================================================
      subroutine upchkd_wrapper(glodir,lenstr,istat)

      character*(*)  glodir
      integer        lenstr,istat
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/upchkd_wrapper.f,v $
     . $',                                                             '
     .$Id: upchkd_wrapper.f,v 1.1 1999/07/06 20:23:01 page Exp $
     . $' /
C    ===================================================================
C

        call upchkd(glodir(1:lenstr),istat)
        if (istat .ne. 0) then
          write(*,'(''status returned from upchkd = '',I5)') istat
          write(*,'(''cannot continue with program'')')
        endif

      return
      end
