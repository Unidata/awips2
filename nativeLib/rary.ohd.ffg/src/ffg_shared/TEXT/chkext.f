c  =====================================================================
c
      subroutine chkext (icaller)
c
c.......................................................................
c
c  Routine to check and/or print user control maximum and minimum
C  extremum for Gridded and Headwater FFG values.
c
c.......................................................................
c
      include 'ffg_inc/iuws'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/chkext.f,v $
     . $',                                                             '
     .$Id: chkext.f,v 1.1 2004/01/30 17:45:33 scv Exp $
     . $' /
C    ===================================================================
C
c
c.......................................................................
c
c
      call prbug ('chkext',1,1,ibug)
ccc      ibug = 1
c
c  check values
      igetinf = 1
      iprint = 0
      icheck = 1
      call usrext (igetinf,icaller,iprint,icheck,ifound,istat)
      if (istat.eq.0) then
         if (ifound.eq.1) then
c        print values
            igetinf = 0
            iprint = 1
            icheck = 0
            call usrext (igetinf,icaller,iprint,icheck,ifound,istat)
            endif
         endif
c
       return
c
       end
