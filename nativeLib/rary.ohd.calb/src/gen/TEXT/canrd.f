C$PRAGMA C (GETUNO)
C MODULE CANRD
C-----------------------------------------------------------------------
C  Routine to see if a file is readable
C-----------------------------------------------------------------------
C  notes: (1) This routine checks to see whether a file is readable.
C             If it is 1 is returned; otherwise 0 is returned.
C-----------------------------------------------------------------------
C variables:
C
C fname         .... name of file to check
c ex            .... indicates whether file exists
c fname         .... name of file which is to be checked
C ierr          .... error code
c iflag         .... return flag
c iunit        .... unit number that is to be used for check
C istat         .... return status
C-----------------------------------------------------------------------

      subroutine canrd ( fname, istat )

      include  'ufiles'
      include  'upvrsx'
      include  'common/sionum'

      character*(*) fname
      character*10 key
      logical exist
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/gen/RCS/canrd.f,v $
     . $',                                                             '
     .$Id: canrd.f,v 1.4 2002/02/11 13:27:38 michaelo Exp $
     . $' /
C    ===================================================================
C
C
      istat=1
C
C  check if file exists
      inquire ( FILE=fname, EXIST=exist )
      if ( .not. exist ) go to 20
C
C  check if readable
      key='GENTMP'
      call getuno (mfiles,ifiles,pgmnam,key,iunit,ierr)
      if ( ierr .ne. 0 ) then
         write (istderr,5) key(1:lenstr(key))
5     format ('0**ERROR** Cannot get unit number key ',a,'.')
         istat=0
         go to 20
         endif
C
C  check if can open file for reading
      open ( UNIT=iunit, FILE=fname, STATUS='OLD', ERR=10 )
      close (iunit)
      go to 20
C
C  file is not readable
10    istat=0
C
   20 return
C
      end
