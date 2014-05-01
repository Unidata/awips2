c  This subroutine returns the segment description that is currently
c   in the FCSEGN common block.
c
c  Needed to fill the title bar heading for segments without rating curves.
c
c  Written by George Smith, 10/22/91
c
      Subroutine get_segment_description(description)
c
      INCLUDE 'common/fcsegn'                                           RTi     
      Dimension description(5)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/getSegDescr.f,v $
     . $',                                                             '
     .$Id: getSegDescr.f,v 1.1 1995/09/08 15:00:24 page Exp $
     . $' /
C  =====================================================================
C
c
      do 10 i = 1,5
 10   description(i) = sgdscr(i)
c
      return
      end
