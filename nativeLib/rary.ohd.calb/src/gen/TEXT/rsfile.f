c$pragma c setres
C$PRAGMA C (GETUNO)
C MODULE RSFILE
C-----------------------------------------------------------------------
C  Routine to reserve a file unit number for later use.
C-----------------------------------------------------------------------
C  notes: (1) This subroutine does not open the file.  It just
C             reserves a unit number for the file.
C         (2) Once a unit number has been reserved, it CANNOT be
C             used to open a file.  This function is designed to
C             "lock out" unit numbers from use.  In particular, it
C             is used to reserve unit numbers that are in the middle
C             of a range of available unit numbers.  The ability to
C             open a reserved file may be added in the future.  This
C             requires saving information about the unit number, like
C             the "key".
C         (3) Unit numbers are set in the FILEUNIT file under the
C             NWSRFS system files.
C         (4) The NWSRFS "IFILES" array is used to indicate the
C             status of a file.  Originally, the flags for this
C             variable were:
C                0 = file unused
C                1 = file is direct access
C                2 = file is sequential
C             In order to add the capability to reserve files, the
C             "IFILES" variable is treated as a bit mask, where the
C             above values apply and additionally:
C                4 = file is reserved
C         (5) One-word Fortran strings that are used must end in a
C             space.  The space will be replaced with a NULL by any
C             C routines that are called.
C         (6) It is assumed that unit number zero can be used for
C             standard error only. 
C-----------------------------------------------------------------------
C variables:
C
C filekeys      .... array of "key" values for use in other routines
C i             .... loop counter
C ierr          .... error status variable (1 if error, 0 if not)
C ifiles        .... array used to indicate file status (taken from 
C                    "ufiles"common block)
C istderr       .... unit number for standard error (taken from "sionum"
C                    common block)
C key           .... keyword that is used to look up unit number for 
C                    file last letter as passed in must be a space)
C mfiles        .... size of "ifiles" array (taken from "ufiles" common
C                    block)
C pgmnam        .... name of the program that is reserving the file 
C                    (taken from the "upvrsx" common block)
C setres        .... function to reserve unit number
C istatus       .... return status
C iunit         .... unit number that will be reserved for "key"
C-----------------------------------------------------------------------

      subroutine rsfile ( key, iunit, ierr )

      include  'ufiles'
      include  'upvrsx'
      include  'common/ionum'
      include  'common/fdbug'
      include  'common/sionum'
      include  'common/unitno'
      include  'common/where'

      character*(*) key
      integer  setres
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/rsfile.f,v $
     . $',                                                             '
     .$Id: rsfile.f,v 1.4 2002/02/11 13:29:33 michaelo Exp $
     . $' /
C    ===================================================================
C

      call umemov ('RSFILE  ',opname,2)

      if ( itrace .ge. 1 ) write (iodbug,*) 'ENTER RSFILE'

      ierr = 0

C  get the unit number to use for the file...
      call getuno ( mfiles, ifiles, pgmnam, key, iunit, ierr )
      if ( ierr .ne. 0 ) then
         write (istderr,20) key(1:lenstr(key))
20    format ('0**ERROR** Unable to get unit number for key ',a,'.')
         ierr = 1
         go to 99
         endif

C  reserve the unit number
      if ( iunit .gt. 0 ) then
         istatus = setres ( ifiles(iunit) )
         if ( istatus .ne. 0 ) then
            write (istderr,40) iunit, key(1:lenstr(key))
40    format('0**ERROR** Unable to reserve unit number ',i3,
     + ' for key ',a,'.')
            ierr = 1
            go to 99
            endif
         endif

C  save key
      filekeys(iunit) = key

99    if ( itrace .ge. 1 ) write (iodbug,*) 'EXIT RSFILE'

      return

      end
