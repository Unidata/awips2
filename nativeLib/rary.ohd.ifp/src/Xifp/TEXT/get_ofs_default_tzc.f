c  module get_ofs_default_tzc.f
c
c  desc 'fill HDFLTS common block and return the default time zone code'
c
      Subroutine get_ofs_default_tzc(time_zone)
c...................................................................
c  argument list:
c
c    time_zone:  default time zone code returned from
c                 NWSRFS OFS files
c...................................................................
c
c  routine originally written by George Smith, HRL, November 14, 1992
c
c  modified to call readit with 60 word long buffer (itemp) and then
c   copy the first 25 words from itemp into /HDFLTS/ common
c   George Smith, HRL, August 17, 1993
c...................................................................
c
      INCLUDE 'uunits'                                                  RTi
      COMMON /HDFLTS/ IHDFLT(25)
c
      Dimension itemp(60)
c
      Character * 4 time_zone
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/get_ofs_default_tzc.f,v $
     . $',                                                             '
     .$Id: get_ofs_default_tzc.f,v 1.2 1999/01/20 21:11:02 page Exp $
     . $' /
C  =====================================================================
C
c
C  HDFLTS
C      Read HDFLTS common block from USERPARM
C-------
      CALL UREADT(KUPARM,1,itemp,ISTAT)
C
      Call umemov(itemp(3), time_zone, 1)
c
      Call umemov(itemp, IHDFLT, 25)
      Return
      End
