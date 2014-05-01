c  =====================================================================
c  pgm:  calcp (ident,jdur,po,tro,ffg,istat)
c
c   in: ident  .... identifier
c   in: jdur   .... duration sequence:
c                     1=1 hr
c                     2=3 hr
c                     3=6 hr
c                     4=12 hr
c                     5=24 hr
c   in: po     .... parameter array
c   in: tro    .... threshold runoff
c  out: ffg    .... computed flash flood guidance
c  out: istat  .... status code
c  =====================================================================
c
      subroutine calcp (ident,jdur,po,tro,ffg,istat)
c
c.......................................................................
c
c  calculate ffg from rainfall-runoff curve
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
c
      character*8 ident
c
      dimension po(*)
c
c  rainfall-runoff values for each duration
      parameter (numv=8)
      dimension rr(numv)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/calcp.f,v $
     . $',                                                             '
     .$Id: calcp.f,v 1.3 2004/01/30 17:43:49 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('calcp',1,5,ibug)
ccc      ibug = 1
c
      istat = 0
c
      ffg = 0.0
c
c  rainfall-runoff curves start in position 18 - get curve for
c  specified duration
      ibeg = 17 + numv*(jdur-1)
      iokay = 1
      do 20 j=1,numv
         ipos=ibeg+j
         if (ibug.eq.1) write (iutw,*) 'in calcp - ipos=',ipos,
     *      'po(ipos)=',po(ipos)
         rr(j) = po(ipos)
         if (rr(j).lt.0.0) then
            if (iokay.eq.1) then
               write (iutw,10) idurt(jdur),ident
               if (iupr.ne.iutw) write (iupr,10) idurt(jdur),ident
10    format (' ERROR: Flash Flood Guidance Operation never run for ',
     +   i2,' hour duration for FFG identifier ',a,'.')
               nerr = nerr + 1
               istat = 1
               iokay = 0
               endif
            endif
20       continue
c
      if (ibug.eq.1) write (iutw,30) iokay,numv,rr
30    format (' in calcp - iokay=',i1,' numv=',i1,' rr=',8(f6.3,1x))
c
      if (iokay.eq.1) then
c     linear interpolation and extrapolation
         iconv = 1
         call linint (iconv,numv,rr,ffg,tro)
         endif
c
      if (ibug.eq.1) write (iutw,*) 'in calcp - tro=',tro,' ffg=',ffg
c
      return
c
      end
