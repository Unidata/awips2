
      Subroutine StrmPak(U,V,Work,mnx,nx,ny,asize,xpoints,ypoints,
     &                   npoints,minspc,maxspc,badlo,badhi)
 
C  This routine draws a set of streamlines.  Works in area defined by
C  current 

C PROGRAM HISTORY:   
c   Unknown original author
c   99-03-25  S.O'Donnell; modifications for Linux/g77 port.
c                      added Implicit None; define undeclared variables;
c                      include interface to 
c                                 g77 unimplemented implicit functions.
C  Inputs:
C    U(mnx,*)     Real*4   Array of U components
C    V(mnx,*)     Real*4   Array of V components
C    Work(mnx,*)  Int*4    Workspace.
C    mnx          Int*4    First dimension of data arrays.
C    nx           Int*4    Inner dimension of grid.
C    nx           Int*4    Outer dimension of grid.
C    asize        Int*4    Size of arrows in MVI.
C    minspc       Real*4   If greater than one, no two streamlines will
C                          approach any closer than this number of cells.
C                          If less than one, a streamline will terminate if
C                          it runs through 1/minspc consecutive already
C                          occupied cells.
C    maxspc       Real*4   No streamline will be started any closer than this
C                          number of cells to an existing streamline.
 
 
      Implicit None
        INCLUDE 'IntrinsicFunctions.inc'
c declare formal arguments
      Integer*4 mnx, nx, ny, Work(nx,ny)
      Real*4    U(mnx,*), V(mnx,*)
      Real*4    xpoints(*), ypoints(*)
      Real*4    asize
      Integer*4 npoints
      Real*4    minspc, maxspc, badhi, badlo
C
 
      Integer*4 ill,iur,jll,jur,ium,jum
      Real*4    asiz
      Common /StrmPakCmn/ ill,iur,jll,jur,ium,jum,asiz
 
      Integer*4 i,j,ii,jj,
     -          i1,i2,j1,j2,k
      Real*4    ri0,rj0,mymax
      Logical   again

        REAL*4     minmag,maxmag,checkval
        Common    /vecLimCmn/minmag,maxmag,checkval

        REAL*4    minmag2,maxmag2,mag2

c  Initialize environment of streamline output.
      npoints = 0
      minmag2 = minmag*minmag
      maxmag2 = maxmag*maxmag
      ill=1
      jll=1
      iur=nx
      jur=ny
      ium=nx-1
      jum=ny-1
      asiz = asize

c  Initialize work arrays.
      Do 5 j=jll,jur
      Do 5 i=ill,iur
5     Work(i,j)=0
      Do 6 j=jll,jum
      jj=j+1
      Do 6 i=ill,ium
      ii=i+1
      If (.not.(U(i,j).lt.badlo .or. U(i,j).gt.badhi) .or.
     -    .not.(U(i,jj).lt.badlo .or. U(i,jj).gt.badhi) .or.
     -    .not.(U(ii,j).lt.badlo .or. U(ii,j).gt.badhi) .or.
     -    .not.(U(ii,jj).lt.badlo .or. U(ii,jj).gt.badhi) .or.
     -    .not.(V(i,j).lt.badlo .or. V(i,j).gt.badhi) .or.
     -    .not.(V(i,jj).lt.badlo .or. V(i,jj).gt.badhi) .or.
     -    .not.(V(ii,j).lt.badlo .or. V(ii,j).gt.badhi) .or.
     -    .not.(V(ii,jj).lt.badlo .or. V(ii,jj).gt.badhi)) Then
           Work(i,j)=-1
           Goto 6
      EndIf
      if (minmag.le.0.0 .and. maxmag.ge.badlo) Goto 6
      mag2 = U(i,j)*U(i,j)+V(i,j)*V(i,j)
      if (mag2.ge.minmag2 .and. mag2.le.maxmag2) Goto 6
      mag2 = U(ii,j)*U(ii,j)+V(ii,j)*V(ii,j)
      if (mag2.ge.minmag2 .and. mag2.le.maxmag2) Goto 6
      mag2 = U(i,jj)*U(i,jj)+V(i,jj)*V(i,jj)
      if (mag2.ge.minmag2 .and. mag2.le.maxmag2) Goto 6
      mag2 = U(ii,jj)*U(ii,jj)+V(ii,jj)*V(ii,jj)
      if (mag2.ge.minmag2 .and. mag2.le.maxmag2) Goto 6
      Work(i,j)=-1
 6    Continue

      k=jnint(maxspc*4)
      if (k.lt.1) k = 1

776   i1=(ill+iur)/2
      i2=i1+1
      j1=(jll+jur)/2
      j2=j1+1
 
777   again=.false.
      mymax=maxspc
      if (k.gt.mymax) mymax = k
 
      If (j1.ge.jll) Then
          rj0=float(j1)
          Do 45 i=i1,i2-1,k
          ri0=float(i)+.5
          Call StrmLin(U,V,xpoints,ypoints,npoints,Work,mnx,ri0,rj0,
     &                 minspc,mymax)
45        Continue
          j1=j1-k
          again=.true.
         End If
 
      If (i1.ge.ill) Then
          ri0=float(i1)
          Do 65 j=j1,j2-1,k
          rj0=float(j)+.5
          Call StrmLin(U,V,xpoints,ypoints,npoints,Work,mnx,ri0,rj0,
     &                 minspc,mymax)
65        Continue
          i1=i1-k
          again=.true.
         End If
 
      If (j2.lt.jur) Then
          rj0=float(j2)
          Do 55 i=i1,i2-1,k
          ri0=float(i)+.5
          Call StrmLin(U,V,xpoints,ypoints,npoints,Work,mnx,ri0,rj0,
     &                 minspc,mymax)
55        Continue
          j2=j2+k
          again=.true.
         End If
 
      If (i2.le.iur) Then
          ri0=float(i2)
          Do 75 j=j1,j2-1,k
          rj0=float(j)+.5
          Call StrmLin(U,V,xpoints,ypoints,npoints,Work,mnx,ri0,rj0,
     &                 minspc,mymax)
75        Continue
          i2=i2+k
          again=.true.
         End If
 
      If (again) Goto 777

      k = k/2
      if (k.ge.1) Goto 776

      Return
      End

      Subroutine StrmLin(U,V,xpoints,ypoints,npoints,Work,mnx,ri0,rj0,
     &                   minspc,maxspc)

C  This routine draws a single streamline through the point ri0,rj0.
C  ri0 are rj0 real numbers in array index space.

C  Inputs:
C    U(mnx,*)     Real*4   Array of U components
C    V(mnx,*)     Real*4   Array of V components
C   Work(2,mnx,*) Int*2    Workspace which keeps track of how many streamlines
C                          have been drawn in each cell.  A value of -1
C                          designates a cell as having bad or missing data.
C                          1 is for previously drawn streamlines, 2 includes
C                          the streamline currently being drawn.
C    mnx          Int*4    First dimension of array to be countoured.
C    ri0,rj0      Real*4   Location to draw the streamline through.
C                          Coordinates are in array index space.
C    minspc       Real*4   If greater than one, no two streamlines will
C                          approach any closer than this number of cells.
C                          If less than zero, a streamline will terminate if
C                          it runs through 1/minspc consecutive already
C                          occupied cells.
C    maxspc       Real*4   No streamline will be started any closer than this
C                          number of cells to an existing streamline.
 
      Implicit None
      INCLUDE 'IntrinsicFunctions.inc'

      Integer*4           ill,iur,jll,jur,ium,jum
      Real*4              asiz
      Common /StrmPakCmn/ ill,iur,jll,jur,ium,jum,asiz

c declare formal arguments
      Integer*4 mnx
      Real*4    U(mnx,*),V(mnx,*), ri0,rj0,minspc,maxspc
      Real*4    xpoints(*), ypoints(*)
      Integer*2 Work(2,iur,jur)
      Integer*4 mp
      Integer*4 npoints
      Parameter (mp=4000)

      Real*4    px(3),py(3),Ipnt(-mp:mp),Jpnt(-mp:mp),im,ip,jm,jp
      Integer*4 kpnt,kpnt1,kpnt2,dkpnt,km,kp
      Byte      LCpnt(-mp:mp)

      Integer*4 i,j,ii,jj,iii,jjj,icheck,btrack,
     -          l,narrow,ntot,
     -          dii,djj,track,ntrack,
     -          itrack(2000),jtrack(2000),ovrlap,ntb,i0,j0,i1,j1,
     -          kstrm,k,kk,kkk,kkkk,side0,
     -          SgSide(8),nsg,nin,nout,loopct
      Real*4    qi0,qj0,rpxi,rpyj,
     -          curloc,outloc,curSF,
     -          x,y,dx,dy,
     -          x1,y1,x2,y2,
     -          xx,yy,xy,mult,
     -          dirflg,mag,influx,outflux,
     -          SgLoc(0:8),SgSF(0:8),Flux(8),SgCont(8)
      Logical   flxflg,forward,done

      Data    LCPnt/mp*1,1,mp*1/

      Integer*4 wgt1,wgt2,npass
c      Common   /StrmSmthCmn/npass,wgt1,wgt2
Common block `strmsmthcmn' initialized by another program unit
c   only one program unit may specify initial values for a 
c   particular common block
c      Data      npass/0/
      Data ovrlap / 0 /            ! initialized to remove warnings


c  Determine if a streamline can be started here.
      If (ri0.lt.float(ill) .or. ri0.gt.float(iur) .or.
     -    rj0.lt.float(jll) .or. rj0.gt.float(jur)) Return

c      Write (*,*) ' '
c      Write (*,*) '**************************************************'
c      Write (*,*) '**************************************************'
c      Write (*,*) 'Starting one at ',ri0,rj0

c  Initialize some variables.
      ntb=1+min0(iur-ill,jur-jll)/4
      track=0
      track=0
      ntrack=jnint(maxspc)
      if (ntrack.lt.ntb) ntrack = ntb
      qi0=jnint(ri0)
      qj0=jnint(rj0)
      kpnt1=0
      kpnt2=0

c      Write (*,*) '**************************************************'
c      Write (*,*) 'ntb,qi0,qj0 ',ntb,qi0,qj0

c  Loop for trying streamlines in both directions from this point.
      Do 7779 kstrm=1,2
      kpnt=0
      dkpnt=0
      btrack=track+1

c  Determine which side and which cell we are starting with
      If (abs(ri0-qi0).lt.abs(rj0-qj0)) Then
          If (kstrm.eq.1) Then
              side0=4
              i=jnint(ri0)
              j=jint(rj0)
                  k=max0(jnint(maxspc)-1,0)
                  Do 4 jjj=max0(jll,j-k),min0(jum,j+k)
                  Do 4 iii=max0(ill,i-k-1),min0(ium,i+k)
4                 If (Work(1,iii,jjj).gt.0) Return
              If (i.gt.ium) Goto 7775
          Else
              side0=2
              i=jnint(ri0)-1
              j=jint(rj0)
              If (i.lt.1) Goto 7775
          End If
          x=qi0-float(i)
          y=rj0-float(j)
      Else
          If (kstrm.eq.1) Then
              side0=1
              i=jint(ri0)
              j=jnint(rj0)
                  k=max0(jnint(maxspc)-1,0)
                  Do 5 jjj=max0(jll,j-k),min0(jum,j+k)
                  Do 5 iii=max0(ill,i-k-1),min0(ium,i+k)
5                 If (Work(1,iii,jjj).gt.0) Return
              If (j.gt.jum) Goto 7775
          Else
              side0=3
              i=jint(ri0)
              j=jnint(rj0)-1
              If (j.lt.1) Goto 7775
          End If
          x=ri0-float(i)
          y=qj0-float(j)
      End If
      ii=i+1
      jj=j+1

c      Write (*,*) 'Starting one at ',ri0,rj0

c  Check if cell has missing values.
      If (Work(1,i,j).eq.-1) Goto 7777

c      Write (*,*) 'i,j,ii,jj ',i,j,ii,jj
c      Write (*,*) 'x,y ',x,y
c      Write (*,*) 'side0 ',side0

c  Determine whether we are working with or against the flow.
      If (side0.eq.1) Then
          influx=V(i,j)*(1.0-x)+V(ii,j)*x
      Else If (side0.eq.2) Then
          influx=-(U(ii,j)*(1.0-y)+U(ii,jj)*y)
      Else If (side0.eq.3) Then
          influx=-(V(i,jj)*(1.0-x)+V(ii,jj)*x)
      Else!If (side0.eq.4) Then
          influx=U(i,j)*(1.0-y)+U(i,jj)*y
      End If
      If (influx.lt.0.0) Then
          dirflg=-1.0
          dkpnt=-1
      Else If (influx.gt.0.0) Then
          dkpnt=+1
          dirflg=+1.0
      Else
          Goto 7777
      End If

c  Set some initialize values at streamline start point.
      rpxi=i
      rpyj=j
      narrow=(ntb+3)*3/4
      done=.false.
      ntot=(narrow+1)/2
      loopct=0
      IPnt(kpnt)=rpxi+x
      JPnt(kpnt)=rpyj+y

c      Write (*,*) 'dirflg ',dirflg

      i1=0
      j1=0
      i0=0
      j0=0
      icheck = 1

c  Start process of crossing this cell, check if we have missing data. 
15    If (Work(1,i,j).eq.-1) Goto 7777

c      Write (*,*) ' '
c      Write (*,*) 'i,j,ii,jj ',i,j,ii,jj
c      Write (*,*) 'x,y,side0 ',x,y,side0
c      Write (*,*) 'rpxi,rpyj ',rpxi,rpyj
c      Write (*,*) U(i,jj)*dirflg,V(i,jj)*dirflg,'  ||  ',
c     &        U(ii,jj)*dirflg,V(ii,jj)*dirflg
c      Write (*,*) U(i,j)*dirflg,V(i,j)*dirflg,'  ||  ',
c     &        U(ii,j)*dirflg,V(ii,j)*dirflg

C  Determine if there are already too many streamlines around.
      If (track.gt.btrack) Then
          If (minspc.lt.1.5) Then
              If (Work(icheck,i,j).eq.0) Then
                  ovrlap=0
              Else
                  ovrlap=ovrlap+Work(icheck,i,j)
                  If (ovrlap.ge.jnint(1./minspc)) Goto 7777
              End If
              icheck = 2
          Else
              If (Work(icheck,i,j).gt.0) Goto 7777
              Do 20 djj=-1,0,1
              Do 20 dii=-1,0,1
              If (dii.eq.0 .and. djj.eq.0) Goto 20
              If (dii.ne.0 .and. djj.ne.0) Then
                  k=jnint(minspc*.707)-1
              Else
                  k=jnint(minspc)-1
              End If
              if (k.gt.track-btrack) k=track-btrack
              If (k.lt.1) Goto 20
              iii=i
              jjj=j
              Do 18 l=1,k
              iii=iii+dii
              If (iii.lt.ill .or. iii.gt.ium) Goto 20
              jjj=jjj+djj
              If (jjj.lt.jll .or. jjj.gt.jum) Goto 20
              If (Work(1,iii,jjj).gt.0) Goto 7777
              If (Work(2,iii,jjj).le.0) Goto 18
              Do 17 kkkk=1+track-k,track
17            If (itrack(kkkk).eq.iii .and. 
     &            jtrack(kkkk).eq.jjj) Goto 18
              Goto 7777
18            Continue
20            Continue
          End If
          icheck = 2
      End If

C  Determine flux contributions from each component.
      Flux(1)=(-dirflg)*V( i, j)
      Flux(2)=(-dirflg)*V(ii, j)
      Flux(3)= +dirflg *U(ii, j)
      Flux(4)= +dirflg *U(ii,jj)
      Flux(5)= +dirflg *V(ii,jj)
      Flux(6)= +dirflg *V( i,jj)
      Flux(7)=(-dirflg)*U( i,jj)
      Flux(8)=(-dirflg)*U( i, j)

c      Write (*,*) 'Flux'
c      Write (*,*) Flux

c  Count total number of in, out, and zero contributions to net flux.
      nin=0
      nout=0
      Do 25 k=1,8
      If (Flux(k).lt.0.0) Then
          nin=nin+1
      Else If (Flux(k).gt.0.0) Then
          nout=nout+1
      End If
25    Continue
      If (nin.eq.0) Goto 7777

c  Check if there are no exit points in this cell.
      If (nout.eq.0) Then

c  Determine termination point within this cell.
          x1=-Flux(8)-Flux(7)
          x2=-Flux(4)-Flux(3)
          If (x1+x2.le.0.0) Then
              xx=0.5
          Else
              xx=x1/(x1+x2)
          End If
          y1=-Flux(5)-Flux(6)
          y2=-Flux(1)-Flux(2)
          If (y1+y2.le.0.0) Then
              yy=0.5
          Else
              yy=y1/(y1+y2)
          End If
          done=.true.
c          Write (*,*) 'terminated at ',xx,yy
          Goto 77 !  go to drawing portion
      End If

c  Make a list of segments on cell border with like contribution to flux.
c  Record location, side, and flux contribution.
      influx=0.0
      outflux=0.0
      nsg=0
      SgLoc(0)=0.0
      Do 35 k=1,4
      kkk=k+k
      kk=kkk-1
      If (Flux(kk).lt.0.0) Then
          flxflg=(Flux(kkk).gt.0.0)
      Else If (Flux(kk).gt.0.0) Then
          flxflg=(Flux(kkk).lt.0.0)
      Else
          flxflg=.false.
      End If
      If (flxflg) Then
          xy=Flux(kk)/(Flux(kk)-Flux(kkk))
          If (xy.gt.0.0) Then
              nsg=nsg+1
              SgSide(nsg)=k
              SgLoc(nsg)=float(k-1)+xy
              SgCont(nsg)=xy*Flux(kk)/2
              If (SgCont(nsg).lt.0.0) Then
                  influx=influx+SgCont(nsg)
              Else
                  outflux=outflux+SgCont(nsg)
              End If
          End If
          If (xy.lt.1.0) Then
              nsg=nsg+1
              SgSide(nsg)=k
              SgCont(nsg)=(1.0-xy)*Flux(kkk)/2
          End If
          SgLoc(nsg)=float(k)
      Else
          nsg=nsg+1
          SgLoc(nsg)=float(k)
          SgSide(nsg)=k
          SgCont(nsg)=Flux(kk)+Flux(kkk)
      End If
      If (SgCont(nsg).lt.0.0) Then
          influx=influx+SgCont(nsg)
      Else
          outflux=outflux+SgCont(nsg)
      End If
35    Continue

c  Adjust the magnitude of the flux segments to make total flux integrated
c  around the cell zero.  Integrate to get stream function values.
      mult=sqrt((-outflux)/influx)
      SgSF(0)=0.0
      Do 40 k=1,nsg
      If (SgCont(k).gt.0.0) Then
          SgSF(k)=SgSF(k-1)+SgCont(k)/mult
      Else
          SgSF(k)=SgSF(k-1)+SgCont(k)*mult
      End If
c      Write (*,*) SgLoc(k),SgSide(k),SgCont(k),SgSF(k)
40    Continue
      SgSF(nsg)=0.0

c  Based on side of entry, determine circular location of endpoint
c  and direction to search for exit point.
      If (side0.eq.1) Then
          curloc=x
          forward=dirflg*(U(i,j)*(1.0-x)+U(ii,j)*x).gt.0.0
      Else If (side0.eq.2) Then
          curloc=1.0+y
          forward=dirflg*(V(ii,j)*(1.0-y)+V(ii,jj)*y).gt.0.0
      Else If (side0.eq.3) Then
          curloc=3.0-x
          forward=dirflg*(U(i,jj)*x+U(ii,jj)*(1.0-x)).lt.0.0
      Else!If (side0.eq.4) Then
          curloc=4.0-y
          forward=dirflg*(V(i,j)*y+V(i,jj)*(1.0-y)).lt.0.0
      End If

c      Write (*,*) 'curloc,forward ',curloc,forward

c  Determine stream function value of entry location.
      Do 45 kk=1,nsg-1
45    If (SgLoc(kk).gt.curloc) Goto 50
50    k=kk-1
c      Write (*,*) 'k,kk ',k,kk
      If (SgCont(kk).ge.0.0) Goto 7777
c      If (SgLoc(kk).eq.SgLoc(k)) Goto 7777
      curSF=SgSF(k)+
     &      (SgSF(kk)-SgSf(k))*(curloc-SgLoc(k))/(SgLoc(kk)-SgLoc(k))
      If (k.lt.1) k=nsg
c      Write (*,*) 'k,kk,curSF ',k,kk,curSF

c  Search for next occurence of this value of the stream function.
      kkk=k
      If (forward) Then
55        k=kk
          kk=kk+1
          If (kk.gt.nsg) kk=1
          If (k.eq.kkk) Goto 7777
          If (SgCont(kk).le.0.0) Goto 55
          If ((SgSf(k).le.curSF) .neqv. (curSF.lt.SgSf(kk))) Goto 55
      Else
60        kk=k
          k=k-1
          If (k.lt.1) k=nsg
          If (k.eq.kkk) Goto 7777
          If (SgCont(kk).le.0.0) Goto 60
          If ((SgSf(k).le.curSF) .neqv. (curSF.lt.SgSf(kk))) Goto 60
      End If
      If (k.eq.nsg) k=0
c      If (SgSf(kk).eq.SgSf(k)) Goto 7777
      outloc=SgLoc(k)+
     &      (SgLoc(kk)-SgLoc(k))*(curSF-SgSF(k))/(SgSF(kk)-SgSf(k))
      side0=SgSide(kk)

c      Write (*,*) 'k,kk,outloc ',k,kk,outloc,side0

C  Based upon exit side, figure out location in x/y space.
      If (side0.eq.1) Then
          xx=outloc
          yy=0.0
      Else If (side0.eq.2) Then
          xx=1.0
          yy=outloc-1.0
      Else If (side0.eq.3) Then
          xx=3.0-outloc
          yy=1.0
      Else!If (side0.eq.4) Then
          xx=0.0
          yy=4.0-outloc
      End If

c      Write (*,*) 'xx,yy ',xx,yy

C  Record plotting location for this cell.
77    ntot=ntot+1
      kpnt=kpnt+dkpnt
      If (ntot.le.narrow) Goto 79
      If (Work(2,i,j).ne.0) Goto 79
      dx=xx-x
      dy=yy-y
      mag=sqrt(dx*dx+dy*dy)
      If (mag.lt.0.2) Goto 79
      LCPnt(kpnt)=0
      ntot=0
79    IPnt(kpnt)=rpxi+xx
      JPnt(kpnt)=rpyj+yy

c  Patch to prevent infinite loop.
      i1=i0
      j1=j0
      i0=i
      j0=j

c  Keep track of cells used so far.
      Work(2,i,j)=Work(2,i,j)+1
      track=track+1
      itrack(track)=i
      jtrack(track)=j

C  Based on exit side, figure out stuff for next cell.
      If (done) Goto 7775
      If (side0.eq.1) Then
          side0=3
          jj=j
          j=j-1
          If (j.lt.1) Goto 7775
          rpyj=rpyj-1.0
          x=xx
          y=1.0
      Else If (side0.eq.2) Then
          side0=4
          i=ii
          ii=ii+1
          If (i.gt.ium) Goto 7775
          rpxi=rpxi+1.0
          x=0.0
          y=yy
      Else If (side0.eq.3) Then
          side0=1
          j=jj
          jj=jj+1
          If (j.gt.jum) Goto 7775
          rpyj=rpyj+1.0
          x=xx
          y=0.0
      Else If (side0.eq.4) Then
          side0=2
          ii=i
          i=i-1
          If (i.lt.1) Goto 7775
          rpxi=rpxi-1.0
          x=1.0
          y=yy
      End If

c  Patch to prevent infinite loop.
      If (i.eq.i1 .and. j.eq.j1) Then
c          Write (*,*) 'WARNING, WARNING, WARNING!!!!!!!!'
c          Write (*,*) 'Infinite loop detected.'
          loopct=loopct+1
          If (loopct.ge.3) Goto 7777
      Else
          loopct=0
      End If

      Goto 15  !  loop back to continue streamline

c  Escape point for streamline hitting grid border, source, or sink.
7775  ntrack=(ntrack+1)/2 ! allow shorter streamlines in this case.

c  Escape point for all others...record kpnt value.
7777  If (dkpnt.eq.-1) Then
          kpnt1=kpnt
      Else If (dkpnt.eq.1) Then
          kpnt2=kpnt
      End If

7779  Continue  !  Loop for streamline directions

C  Don't draw this streamline if it is too short.
      If (track.lt.ntrack) Then
          Do 995 k=1,track
995       Work(2,itrack(k),jtrack(k))=Work(2,itrack(k),jtrack(k))-1
          Do 996 k=kpnt1,kpnt2
996       LCPnt(k)=1
          Return
      Else
          Do 997 k=1,track
997       Work(1,itrack(k),jtrack(k))=Work(1,itrack(k),jtrack(k))+1
      End If
      npass = 5
      wgt1 = 6
      wgt2 = 88
c  Do smoothing.
      If (npass.gt.0) Then  !  we are going to smooth the streamlines.
          Do 9011 kkkk=1,npass
          k=kpnt1+1
          im=IPnt(kpnt1)
          xx=IPnt(k)
          jm=JPnt(kpnt1)
          yy=JPnt(k)
          Do 9009 kp=kpnt1+2,kpnt2
          ip=IPnt(kp)
          jp=JPnt(kp)
          IPnt(k)=(wgt1*(ip+im)+wgt2*xx)/100
          JPnt(k)=(wgt1*(jp+jm)+wgt2*yy)/100
          im=xx
          jm=yy
          xx=ip
          yy=jp
9009      k=kp
9011      Continue
      End If
c  Draw streamline.
c      Call FGVectors(Ipnt(kpnt1),Jpnt(kpnt1),0,kpnt2+1-kpnt1)
      npoints = npoints + 1
      xpoints(npoints) = -99999.0
      ypoints(npoints) = -99999.0

      do 1324 i = kpnt1, kpnt2
         npoints = npoints + 1
         xpoints(npoints) = Ipnt(i)
         ypoints(npoints) = Jpnt(i)
1324  continue

c  Draw arrows.
      If (kpnt1.lt.0) Then
          kkk=kpnt1+2
          LCPnt(kkk)=iand(LCPnt(kpnt1),LCPnt(kpnt1+1))
      Else
          kkk=kpnt1
      End If
c     Call fgrotate(1)
      Do 9019 k=kkk,kpnt2
        If (LCpnt(k).ne.0) Goto 9019
        LCPnt(k)=1

        If (k.lt.0) Then
          km=k
          kp=k+1
        Else
          km=k-1
          kp=k
        End If

        dx=real(IPnt(kp)-IPnt(km))
        dy=real(JPnt(kp)-JPnt(km))
        mag=sqrt(dx*dx+dy*dy)
        If (mag.eq.0.0) Goto 9019
        mag = mag/asiz
        dx=dx/mag
        dy=dy/mag
        x = (IPnt(km)+IPnt(kp))/2
        y = (JPnt(km)+JPnt(kp))/2
c       Call FGrelOrigin(x, y)
        px(1)=+dy-dx
        py(1)=-dy-dx
        px(2)=0
        py(2)=0
        px(3)=py(1)
        py(3)=-px(1)
        npoints = npoints + 1
        xpoints(npoints) = -99999.0
        ypoints(npoints) = -99999.0
        npoints = npoints + 1
        xpoints(npoints) = x - (+dy-dx)
        ypoints(npoints) = y - (-dy-dx)
        npoints = npoints + 1
        xpoints(npoints) = x
        ypoints(npoints) = y
        npoints = npoints + 1
        xpoints(npoints) = x - (-dy-dx)
        ypoints(npoints) = y + (+dy-dx)

c       Call FGVectors(px,py,1,3)
9019  Continue
c     Call fgrotate(0)

      Return
      End

      Subroutine StrmSmth(smoothness,npasses)

C  Inputs:
C   smoothness  R*4  Must be from 0.0 to 2.0.  1.0 is nominal.
C   npasses     I*4  Number of smoothing passes.  0 to 10.

      Implicit  None
      INCLUDE 'IntrinsicFunctions.inc'

c declare formal arguments
      Real*4    smoothness
      Integer*4 npasses
c

      Integer*4 wgt1,wgt2,npass
      Common   /StrmSmthCmn/npass,wgt1,wgt2
Common block `strmsmthcmn' initialized by another program unit
c   only one program unit may specify initial values for a 
c   particular common block
c      Data      npass/0/

      If (smoothness.lt.0.0 .or. smoothness.gt.2.0) Return
      If (npasses.lt.0 .or. npasses.gt.10) Return
      If (smoothness.eq.0.0 .or. npasses.eq.0) Then
          npass=0
      Else
          npass=npasses
          wgt1=jnint(25.0*smoothness)
          wgt2=100-2*wgt1
      End If

      Return
      End
