c  =====================================================================
c  pgm:  inigrd (idr,nurow,nucol,mxd,mxr,mxc,gar,istat)
c
c   in: idr    .... duration number
c   in: nurow  .... number of grid rows used
c   in: nucol  .... number of grid columns used
c   in: mxd    .... maximum number of durations
c   in: mxr    .... maximum number of grid rows
c   in: mxc    .... maximum number of grid columns
c  out: gar    .... grid array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine inigrd (idr,nurow,nucol,mxd,mxr,mxc,gar,istat)
c
c.......................................................................
c
c  This routine initializes array gar with missing values.
c
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
c
      dimension gar(mxd,mxr,mxc)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/inigrd.f,v $
     . $',                                                             '
     .$Id: inigrd.f,v 1.3 2003/08/20 13:11:18 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('inigrd',1,1,ibug)
c
      istat=0
c
      if (ibug.eq.1) write (iud,*) ' idr=',idr,
     +   ' nurow=',nurow,' nucol=',nucol,
     +   ' mxd=',mxd,' mxr=',mxr,' mdc=',mxc
c
      if (idr.gt.mxdurg) then
         write (iutw,10) idr,mxdurg
         if (iupr.ne.iutw) write (iupr,10) idr,mxdurg
10    format (' WARNING: in inigrd - duration index value (',i1,
     +   ') exceeds maximum durations (',i1,').')
         endif
c
      do 30 irow=1,nurow
         do 20 icol=1,nucol
            gar(idr,irow,icol) = -9.90
20          continue
30       continue
c
      return
c
      end
