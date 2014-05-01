      Subroutine close_all_open_files
      Logical   is_opened
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Utility/RCS/close_all_open_files.f,v $
     . $',                                                             '
     .$Id: close_all_open_files.f,v 1.3 2001/06/13 12:06:07 mgm Exp $
     . $' /
C  =====================================================================
C
c
c  Function to close files open during NWSRFS execution
c   for a single segment in the ifp program.
c  Originally written by G. Smith, HRL, Nov. 1992.
c
c  Modified to add call to closal during port of ifp
c   to HP GDP workstations.
c  G. Smith, May (and August) 1993.
c 
c  uclosl (replaces closal) closes all files opened through the
c   NWSRFS "file bookkeeping" system.
c
c  Then all remaining FORTRAN files are rewound and
c   closed.
c
      call uclosl()
c
      Do 10 i = 10, 99
c
      INQUIRE (UNIT=i, OPENED=is_opened)
c
      if(is_opened) then
         Rewind (UNIT=i, IOSTAT=IOS, ERR=10)
         Close (UNIT=i, IOSTAT=IOS, ERR=10)
      endif
 10   Continue
c
      return
      end
