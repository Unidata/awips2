c  =====================================================================
c  pgm:  gethiq (iqctl,iqopt,taq,qtsid,qtstp,ndur,q,istat)
c
c   in: iqctl  .... high flow adjust global control
c   in: iqopt  .... high flow adjust option
c   in: taq    .... adjustment time for high flows
c   in: qtsid  .... forecast flow time series identifier
c   in: qtstp  .... forecast flow time series data type code
c   in: ndur   .... number of durations
c  out: q      .... forecast flows at specified times
c  out: istat  .... status code
c  =====================================================================
c
      subroutine gethiq (iqctl,iqopt,taq,qtsid,qtstp,ndur,q,istat)
c
c.......................................................................
c
c  get high flow values from OFS forecast time series
c
c.......................................................................
c  Initially written by
c         Tim Sweeney, HRL                               March 1992
c
c  Restructured as routine
c         Tim Sweeney, HRL                               Nov 1997
c.......................................................................
c
      character*4 units,qtstp
      character*8 qtsid
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
c
      dimension q(5),taq(5),thq(5)
      dimension ihead(22)
      parameter (maxx=1)
      dimension xbuf(maxx)
      parameter (lwkbuf=500)
      dimension iwkbuf(lwkbuf)
      parameter (lbuf=500)
      dimension buf(lbuf)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/gethiq.f,v $
     . $',                                                             '
     .$Id: gethiq.f,v 1.2 2003/03/14 18:15:02 dws Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gethiq',1,1,ibug)
c
c  initialize base flows to zero
      do 10 i=1,ndur
         q(i) = 0.0
10       continue
c
      if (iqctl.ne.1.or.(iqopt.lt.1.or.iqopt.gt.3)) go to 60
c
      if (ibug.gt.0) write (iud,*) 'qtsid=',qtsid,' qtstp=',qtstp
c
c  read forecast flow time series header
      call rprdh (qtsid,qtstp,maxx,ihead,numx,xbuf,iftsid,istat)
      if (istat.ne.0) then
         write (iutw,20) 'header',qtsid,qtstp,'RPRDH',istat
         if (iupr.ne.iutw) then
            write (iupr,20) 'header',qtsid,qtstp,'RPRDH',istat
            endif
20    format (' ERROR: cannot read forecast flow time series ',a,
     +   ' for identifier ',a,' and type ',a,'. ',a,' istat=',i2)
         nerr = nerr + 1
         go to 60
         endif
c
c  determine starting position in forecast flow time series
      itstep = ihead(2)
      nreg = (ihead(7) - ihead(6)) / ihead(3)
      jhour = (nreg-1)*ihead(2) + ihead(14)
      if (ibug.gt.0) write (iud,*) 'jhour=',jhour,' itstep=',itstep,
     +   ' nreg=',nreg
      num = ihead(5) - nreg
c
c  get forecast time series - jhour is LSTCMPDY so first item in buf
c  array is at this time
      units = 'CFS '
      rmiss=-999.9
      call rprdd (qtsid,qtstp,jhour,itstep,num,units,rmiss,lbuf,buf,
     +   ifptr,lwkbuf,iwkbuf,istat)
      if (istat.ne.0) then
         write (iutw,20) 'data',qtsid,qtstp,'RPRDD',istat
         if (iupr.ne.iutw) then
            write (iupr,20) 'data',qtsid,qtstp,'RPRDD',istat
            endif
         nerr = nerr + 1
         go to 60
         endif
      if (ibug.gt.0) write (iud,*) ' num=',num,
     +   '(buf(i),i=1,num)=',(buf(i),i=1,num)
c
c  expand times to adjust for high flow
      thq(1) = taq(1)
      if (thq(1).lt.1.0) thq(1) = 12.
      do 30 i=2,ndur
         thq(i) = taq(i)
         if (thq(i).lt.1.0) thq(i) = thq(1)
30       continue
c
      if (ibug.gt.0) write (iud,*) 'iqopt=',iqopt,
     +   '(thq(i),i=1,ndur)=',(thq(i),i=1,ndur)
c
c  for each duration pick flows from the forecast flow time series
      do 50 i=1,ndur
         iel = thq(i)/itstep
         j = thq(i) - iel*itstep
         iel = iel + j/(itstep/2 + 1) + 1
         if (iqopt.eq.1) then
c        (1) forecast Q at time equal to thq(i) hours
            q(i) = buf(iel)
         else
c        (2) determine highest forecast Q over next thq(i) hours
c        (3) determine highest forecast Q in time series
            if (iqopt.eq.3) iel = ihead(5) - nreg
            if (iel.ge.2) then
               fmxq = 0.0
               do 40 k=2,iel
                  if (buf(k).gt.fmxq) fmxq = buf(k)
40                continue
               else
                  fmxq = buf(1)
               endif
            q(i) = fmxq
            endif
50       continue
c
      if (ibug.gt.0) write (iud,*) '(q(i),i=1,ndur)=',(q(i),i=1,ndur)
c
60    return
c
      end
