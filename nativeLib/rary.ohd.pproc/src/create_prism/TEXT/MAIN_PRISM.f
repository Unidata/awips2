       SUBROUTINE MAIN_PRISM_MAIN
#include "pragma.inc"
C
c**********************************************************************
c main_prism.f
c                This is the main driver function. Generate the monthly
c                long term climatological mean precipitation data, it
c                will improve multisensor precipitation estimates
c                especially in mountainous regions which are subject
c                to orographic influences. The input files are
c                coord_xxx.dat and us_ppt_mm.gz, where xxx stands for
c                the site id and mm stands for the months such as '01'.
c
c Last updated: 08/29/01
c History: Jay Breidenbach
c          Modified by Jingtao Deng
c                -- Error handling on file opening
c          Cham Pham - 3/11/02
c                -- utilize Command-line arguments
c                -- call get_apps_defaults
c**********************************************************************
c
      integer       kode,xmin,ymin
      character     officeid*3
      character*156 COORDPATH,COORDNAME
c
c** Check command-line
      no_of_arg=iargc()
      if(no_of_arg .ne. 1) then
         write(6,*) 'Usage: create_prism officeid'
	 call exit(1)
      else
         k=1
	 call getarg(k,officeid)
      endif

c** Open geo_data
      call get_apps_defaults('mpe_sitecoord_dir',17,COORDPATH,
     *                        LCOORDPATH)
      COORDNAME=COORDPATH(1:LCOORDPATH)//'coord_'//officeid//'.dat'

      open (2, file=COORDNAME, status='old',iostat=kode)
      if (kode .NE. 0) then
	write(6,*) file, ' cannot be opened'
	go to 999
      end if

	read(2,*)xmin
	read(2,*)ymin
	read(2,*)nx
	read(2,*)ny

	call PRISMGEN(xmin,ymin,nx,ny,officeid)

  999 continue
      close(2)

      stop
      end
c
c***********************************************************************
      SUBROUTINE PRISMGEN(xmin,ymin,nx,ny,officeid)
c***********************************************************************
c
#include "pragma.inc"

      parameter (inum=1465)
      dimension iarray(inum), precip(nx,ny)
      integer   xmin,ymin,kode,lenpf,opersys
      integer*2 knt(nx,ny)
      character ccols*4,crows*4,cllx*9,clly*8,csize*8,cnull*2,
     *          cmon(13)*2,mosaicname*156,
     *          user*8,datetimes*20,proc_flag*8,datetimev*20,
     *          officeid*3,PRISMPATH*156,PRISMNAME*156,
     *          operuser*10
      character cm*2
      data cmon/'01','02','03','04','05','06','07','08','09','10',
     *            '11','12','14'/

      call get_apps_defaults('mpe_prism_dir',13,PRISMPATH,LPRISMPATH)

      xmax=xmin+nx-1
      ymax=ymin+ny-1
c
c** Process for each month
c
      do 190 k=1,13
c
c** Initialize the arrays
c
      do 100 i=1,nx
      do 110 j=1,ny
	knt(i,j)=0
	precip(i,j)=0.
  110 continue
  100 continue
c
c** Construct the input file name
c
      PRISMNAME=PRISMPATH(1:LPRISMPATH)//'us_ppt_'//cmon(k)//'.gz'
      open(10,file=PRISMNAME,status='old', iostat=kode)
      if (kode .NE. 0) then
        write(6,*) file, ' cannot be opened'
        go to 190
      end if
c
c** Read the parameteric info
c
      read(10,20) ccols,ncols
   20 format(a5,i5)
      read(10,30) crows,nrows
   30 format(a5,i4)
      read(10,40) cllx,xll
   40 format(a9,f18.0)
      read(10,40) clly,yll
      read(10,60) csize,cellsize
   60 format(a8,f18.0)
      read(10,50) cnull,null
   50 format(a12,i6)
c********************************************************************
c ncols    - number of columns
c nrows    - number of rows
c xll      - longitude (in fractional degrees) of the lowerleft corner
c yll      - latitude (in fractional degrees) of the lowerleft corner
c cellsize - grid size (in fractional degrees)
c null     - null value
c********************************************************************
c** Get the latitude (in fractional degrees) of the upperleft corner
c
      yul=yll+cellsize*nrows
c
c** Process data row by row
c
      do 70 i=1,nrows
	fi=i
c	if(fi/10.-i/10.eq.0.) write(6,*) 'mon,i ',cmon(k),i
	  flat=yul-(i-0.5)*cellsize
	  read(10,*) (iarray(j),j=1,ncols)
      do 80 j=1,ncols
	if(iarray(j).lt.0) go to 80
	  flon=xll+(j-0.5)*cellsize
	  flon=-flon
c
c** Convert to HRAP
c
	  call latlon_to_hrap(flon,flat,xhrap,yhrap)
c
c** Truncate and translate
c
	  ihrap=xhrap
	  ihrap=ihrap-xmin+1
	  jhrap=yhrap
	  jhrap=jhrap-ymin+1
c
c** Check range
c
      if(ihrap.lt.1) go to 80
      if(ihrap.gt.nx) go to 80
      if(jhrap.lt.1) go to 80
      if(jhrap.gt.ny) go to 80
c
c** Count the number of data points that fall within each HRAP box, and
c   sum the amounts
c
	knt(ihrap,jhrap)=knt(ihrap,jhrap)+1
	precip(ihrap,jhrap)=precip(ihrap,jhrap)+iarray(j)
   80 continue
   70 continue

      close(10)
c
c** Average
c
      num=0
      do 120 j=1,ny
      do 130 i=1,nx
	if(knt(i,j).eq.0) then
	  precip(i,j)=-0.1
	  num=num+1
	else
	  precip(i,j)=precip(i,j)/knt(i,j)
	endif
  130 continue
  120 continue
c
c** Fill holes
c
      do 140 j=1,ny
	jbeg=j-1
	if(jbeg.lt.1) jbeg=1
	  jend=j+1
	if(jend.gt.ny) jend=ny

      do 150 i=1,nx
	if(knt(i,j).gt.0) go to 180
          ibeg=i-1
        if(ibeg.lt.1) ibeg=1
          iend=i+1
        if(iend.gt.nx) iend=nx

	  num=0
	  sum=0.
      do 160 ii=ibeg,iend
      do 170 jj=jbeg,jend
	if(knt(ii,jj).eq.0) go to 170
	  num=num+1
	  sum=sum+precip(ii,jj)
  170 continue
  160 continue

      if(num.eq.0) then
	precip(i,j)=-0.1
      else
	precip(i,j)=sum/num
      endif

  180 continue

  150 continue
  140 continue
C
C** WRITE PRISM DATA OUT IN XMRG FORMAT
C
C     These precip estimates are scaled by 100.  There for we will
C     divide by 100 so that the estimates will now be in whole millimeters
C
      do 220 i=1,nx
      do 210 j=1,ny
        precip(i,j)=precip(i,j)/100.
  210 continue
  220 continue
C

      lenmn=71
      user='champ'
      lenus=7
      datetimes='xxxxx'
      proc_flag='prism'
      lenpf=5
      datetimev='xxxxx'
      maxval=9998
      vernum=1
      cm=cmon(k)
C
C        Determine the operating system.
C
      call get_oper_sys ( opersys )
C
C        Check for an error finding the operating system.
      if ( opersys .eq. 0 ) then
C
         operuser='HP'//user
C
      else if ( opersys .eq. 1 ) then
C
         operuser='LX'//user
C
      else if ( opersys .eq. 2 ) then
C
         write(6,*) 'The call to the getopersys routine failed.'
         write(6,*) 'Could not determine the operating system.'
         close ( 10 )
         stop
C
      endif
C
      mosaicname='PRISM_'//cm
      CALL WRITEARRAY(llx,lly,nx,ny,mosaicname,lenmn,
     *   operuser,lenus,datetimes,proc_flag,lenpf,precip,datetimev,
     *   maxval,vernum,irc)

  190 continue

      return
      end

c**********************************************************************
      subroutine latlon_to_hrap(lon,lat,x,y)
c**********************************************************************
c
c subroutine converts lat-lon to HRAP

c version Aug 12, 1998 by D.-J. Seo
c
c input variables
c
c lon - longitude in fractional degrees
c lat - lattitude in fractional degrees
c
c output variables
c
c x   - global HRAP x-coordinate
c y   - global HRAP y-coordinate
c
      real lon,lat
      pi=3.141592654
      d2rad=pi/180.
      earthr=6371.2
      ref_lat=60.
      ref_lon=105.
      rmesh=4.7625
      tlat=ref_lat*d2rad
      re=(earthr*(1.+sin(tlat)))/rmesh
      flat=lat*d2rad
      flon=(lon+180.-ref_lon)*d2rad
      r=re*cos(flat)/(1.+sin(flat))
      x=r*sin(flon)
      y=r*cos(flon)
      x=x+401.
      y=y+1601.
      return
      end

