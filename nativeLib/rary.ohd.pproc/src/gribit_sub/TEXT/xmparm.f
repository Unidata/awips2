c  =====================================================================
c  pgm:  xmparm (iupr,ibug,line,num,istat)
c
c   in: iupr   .... print output unit number
c   in: ibug   .... debug indicator
c   in: line   .... input array
c   in: num    .... sequence number of parameter
c  out: istat  .... status
c  =====================================================================
c
      subroutine xmparm_sub (iupr,ibug,line,num,istat)
c
c.......................................................................
c
c  Routine to parse parameters defined for xmrg files.
c
c.......................................................................
c  Initially written by
c     Tim Sweeney, HL                                       Apr 2000
c.......................................................................
c
      character*(*) line
c
      dimension ipm(10)
c
      include 'xmrg_tbl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/xmparm.f,v $
     . $',                                                             '
     .$Id: xmparm.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
c  xmrg parameter
      iptr = 1
      nwid = 8
      call uffch (line,iptr,nwid,xmproc(num),nxt,istat)
      if (xmproc(num)(1:4).eq.'NONE') go to 20
c
c  grib parameter
      iptr = nxt
      nwid = 5
      call uffch (line,iptr,nwid,gribpm(num),nxt,istat)
c
c  WMO identfier
      iptr = nxt
      nwid = 6
      call uffch (line,iptr,nwid,xwmo(num),nxt,istat)
c
c  additional GRIB parameters
      nwid = 4
      do 10 i=1,10
         iptr = nxt
         call uffir (line,iptr,nwid,ipm(i),r,nxt,istat)
10       continue
c
      xmodlid(num) = ipm(1)
      xngrid(num)  = ipm(2)
      xtunit(num)  = ipm(3)
      xnturef(num) = ipm(4)
      xntufc(num)  = ipm(5)
      xtrang(num)  = ipm(6)
      xwidth(num)  = ipm(7)
      xbinf(num)   = ipm(8)
      xdec(num)    = ipm(9)
      xpkflg(num)  = ipm(10)
c
20    return
c
      end

