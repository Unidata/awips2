c  subroutine to set NLSTZ and LOCAL in fctime common
c
      subroutine set_fctime_cb
c
      INCLUDE 'common/fctime'                                           RTi     
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/setFCTIMEcb.f,v $
     . $',                                                             '
     .$Id: setFCTIMEcb.f,v 1.1 1995/09/08 15:01:32 page Exp $
     . $' /
C  =====================================================================
C
c
      local = 5
      nlstz = -7
c
      return
      end
