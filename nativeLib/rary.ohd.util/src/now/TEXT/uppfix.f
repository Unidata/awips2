C$PRAGMA C (GET_APPS_DEFAULTS)
C MODULE UPCLOS
C  =====================================================================
C  pgm: UPPFIX .. Makes full pathname using command line dir and given
C  pgm:           filename (or change the operational directories)
C
C  use:     CALL UPPFIX( cntl.key, inp.filnam, out.pathnam, num.chars )
C
C   in: cntl.key ...... key for action in subrtn, caps or smls: - CHAR*4
C   in:                   = 'OPER' to prefix operational file pathnames
C   in:                   = 'SYST' to prefix system file pathnames
C   in:                   = 'REOR' to prefix reorder file pathnames
C   in:                   = 'GRID' to prefix grid data base file path
C   in:                   = 'MODS' to prefix MODS file pathnames
C   in:                 (keys to change directories, caps or smalls):
C   in:                   = 'OPNW' to change operation directory
C   in:                   = 'SYNW' to change system directory
C   in:                   = 'RENW' to change reorder directory
C   in:                   = 'GRNW' to change grid data base directory
C   in:                   = 'MONW' to change MODS directory
C   in: inp.filnam .... input filename for OFS file - CHAR*32
C   in:                 (or default dir key-word such as "ifp_fs5files"
C   in:                  used to change a directory, max of 31 chars)
C  out: out.pathnam ... pathname of prefixed output file - CHAR*128
C  out:                 (no output if directory change key is given)
C  out: num.chars ..... number of chars in output file, else 0 - INT
C  out:                 (no output if directory change key is given)
C
C  rqd: get_apps_defaults,kkcaps,uppfx1,uppfx2
C  rqd: common: UPDAIO
C
C  cmt: ************* NOTE, there is a limit of 128 characters in the
C  cmt:                     input directroy names (but they should be
C  cmt:                     even less to account for the "/" and final
C  cmt:                     filename used elsewhere!
C
C  cmt: The output variables are not touched if the input "cntl.key" is
C  cmt:  bad.  Thus, they can be initialized before entering this rtn
C  cmt:  in order to be checked after calling this routine (ie. set
C  cmt:  "num.chars" to 0 and then check after calling this routine to
C  cmt:  see if it is still 0 for no results).
C
C  cmt: This rtn obtains the system level directory and the operational
C  cmt:  files level directory names, etc., from environmental vars.
C  =====================================================================
      SUBROUTINE UPPFIX(key,inname,otname,nofc)

      INTRINSIC     char,len

CC AV pgf90 port 7/2/01      EXTERNAL      get_apps_defaults,kkcaps,uppfx1,uppfx2
      EXTERNAL      kkcaps,uppfx1,uppfx2

      CHARACTER*1   null,char
      CHARACTER*4   key,keyx
      CHARACTER*32  innam
      CHARACTER*160 otnam
      CHARACTER*(*) inname,otname
      INTEGER       nofc,iend,ln,len

      INCLUDE 'updaio'
 
      INTEGER       cntl
      INTEGER       lensys,lenope,lenreo,lenmod,lengri
      CHARACTER*128 dirsys,dirope,dirreo,dirmod,dirgri

      SAVE          cntl
      SAVE          dirsys,dirope,dirreo,dirmod,dirgri
      SAVE          lensys,lenope,lenreo,lenmod,lengri
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/uppfix.f,v $
     . $',                                                             '
     .$Id: uppfix.f,v 1.4 2002/02/11 16:35:23 dws Exp $
     . $' /
C    ===================================================================
C

      DATA          cntl / -1 /



C             If "cntl" is -1, then initialize directory pathnames

        if ( cntl .eq. -1 ) then

           innam = 'rfs_sys_dir'
          call get_apps_defaults(innam,11,dirsys,lensys)
           innam = 'ofs_fs5files'
          call get_apps_defaults(innam,12,dirope,lenope)
           innam = 'ofs_reorder_dir'
          call get_apps_defaults(innam,15,dirreo,lenreo)
           innam = 'ofs_mods_dir'
          call get_apps_defaults(innam,12,dirmod,lenmod)
           innam = 'ofs_griddb_dir'
          call get_apps_defaults(innam,14,dirgri,lengri)

          call uppfx1(dirsys,lensys)
          call uppfx1(dirope,lenope)
          call uppfx1(dirreo,lenreo)
          call uppfx1(dirmod,lenmod)
          call uppfx1(dirgri,lengri)

          if ( UE .ge. 0 ) then
            write(UE,109)
            write(UE,101) dirsys(1:lensys)
            write(UE,102) dirope(1:lenope)
            write(UE,103) dirreo(1:lenreo)
            write(UE,104) dirmod(1:lenmod)
            write(UE,105) dirgri(1:lengri)
            write(UE,109)
          endif

  101       format(' directory for system files:   ',a)
  102       format(' directory for oper files:     ',a)
  103       format(' directory for reorder files:  ',a)
  104       format(' directory for mod files:      ',a)
  105       format(' directory for grid files:     ',a)
  109       format(' ')

          cntl = 1

        endif



        innam = inname
        otnam = ' '
        nofc  = 0

        null = char(0)
        ln = 0
  210   ln = ln+1
        if ( ln .gt. 32 ) go to 220
          if ( innam(ln:ln) .eq. ' '  ) go to 220
          if ( innam(ln:ln) .ne. null ) go to 210
  220     iend = ln-1

        keyx = key
        call kkcaps(keyx)

        if ( keyx .eq. 'OPER' ) go to 410
        if ( keyx .eq. 'SYST' ) go to 420
        if ( keyx .eq. 'REOR' ) go to 430
        if ( keyx .eq. 'MODS' ) go to 440
        if ( keyx .eq. 'GRID' ) go to 450
 
        if ( keyx .eq. 'OPNW' ) go to 310
        if ( keyx .eq. 'SYNW' ) go to 320
        if ( keyx .eq. 'RENW' ) go to 330
        if ( keyx .eq. 'MONW' ) go to 340
        if ( keyx .eq. 'GRNW' ) go to 350

          go to 900



  310   call get_apps_defaults(innam,iend,dirope,lenope)
        call uppfx1(dirope,lenope)
        if ( UE .ge. 0 ) write(UE,102) dirope(1:lenope)
          go to 900

  320   call get_apps_defaults(innam,iend,dirsys,lensys)
        call uppfx1(dirsys,lensys)
        if ( UE .ge. 0 ) write(UE,101) dirsys(1:lensys)
          go to 900

  330   call get_apps_defaults(innam,iend,dirreo,lenreo)
        call uppfx1(dirreo,lenreo)
        if ( UE .ge. 0 ) write(UE,103) dirreo(1:lenreo)
          go to 900

  340   call get_apps_defaults(innam,iend,dirmod,lenmod)
        call uppfx1(dirmod,lenmod)
        if ( UE .ge. 0 ) write(UE,104) dirmod(1:lenmod)
          go to 900

  350   call get_apps_defaults(innam,iend,dirgri,lengri)
        call uppfx1(dirgri,lengri)
        if ( UE .ge. 0 ) write(UE,105) dirgri(1:lengri)
          go to 900



  410   call uppfx2(dirope,lenope,innam,iend,otnam,nofc)
          go to 800

  420   call uppfx2(dirsys,lensys,innam,iend,otnam,nofc)
          go to 800

  430   call uppfx2(dirreo,lenreo,innam,iend,otnam,nofc)
          go to 800

  440   call uppfx2(dirmod,lenmod,innam,iend,otnam,nofc)
          go to 800
 
  450   call uppfx2(dirgri,lengri,innam,iend,otnam,nofc)

  800   ln = len(otname)
        if ( nofc .gt. ln ) nofc = ln


  900   continue
        otname = otnam

      return
      END
