      subroutine fcshft(ipos,ratcuv)

      include 'common/rcnew'
      include 'common/fratng'
c
      common/rctemp/nloop,rcname(2,10),iset,npoint,nptrc(10),          
     * qtemp(10,744,112),htemp(10,744,112),ncount
c
      character*4 name(2)
      character*8 ratcuv
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/PlotTulsa/RCS/fcshft.f,v $
     . $',                                                             '
     .$Id: fcshft.f,v 1.4 2002/02/11 19:48:12 dws Exp $
     . $' /
C    ===================================================================
C
c
      loc=0
      do 10 i=1,ncount
         call umemov(rcname(1,i),name(1),1)
         call umemov(rcname(2,i),name(2),1)
         if (ratcuv(1:4).eq.name(1)) then
            if (ratcuv(5:8).eq.name(2)) then 
               loc=i
               goto 20
               endif
            endif
   10 continue

      
   20 if (ipos.le.0.and.ipos.gt.npoint) goto 999
      if (loc.ge.0) then
c
Cc      20 if (ipos.le.0.or.ipos.gt.npoint) goto 999
c
C     ibegq=locq
C      ibegh=loch
C      if (ibegq.le.0) goto 999
C      if (ibegh.le.0) goto 999
c
cc      do 50 i=1,nptrc(loc)
       do 50 i=1,qtemp(loc,ipos,1)
C         xrc(ibegq+i-1)=qtemp(loc,ipos,i)
C         xrc(ibegh+i-1)=htemp(loc,ipos,i)
         qmod(i)=qtemp(loc,ipos,i+1)
         hmod(i)=htemp(loc,ipos,i+1)
   50 continue
      nrcpmd = qtemp(loc,ipos,1)

CC      else
CC      ibegq=locq
CC      ibegh=loch
CC      if (ibegq.le.0) goto 999
CC      if (ibegh.le.0) goto 999
CC      do 60 i=1,nptrc(loc)
CC         qmod(i)=xrc(ibegq+i-1)
CC         hmod(i)=xrc(ibegh+i-1)
CC   60 continue
      end if
c
  999 continue
c
      return
      end
