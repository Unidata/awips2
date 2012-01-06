c  =====================================================================
c  pgm:  griddef (iutw,iupr,ibug,ngrid,inat,
c                 mwcol,msrow,ncol,nrow,
c                 fld,wfld,igds,mxbmap,ibmap,nwarn,ierr)
c
c   in: iutw   .... print unit
c   in: iupr   .... print unit
c   in: ibug   .... debug output control
c   in: ngrid  .... GRIB grid number
c   in: inat   .... national grid control
c  i/o: mwcol  .... most west column
c  i/o: msrow  .... most south row
c  i/o: ncol   .... number of columns (x-axis)
c  i/o: nrow   .... number of rows (y-axis)
c  i/o: fld    .... gridded values array
c   in: wfld   .... dimension for work array used in grid translation
c  out: igds   .... grid definition parameters for GRIB
c   in: mxbmap .... maximum size of array ibmap
c  out: ibmap  .... bit map array
c  i/o: nwarn  .... number of warnings
c  out: ierr   .... error return
c  =====================================================================
c
      subroutine griddef (iutw,iupr,ibug,ngrid,inat,
     +                    mwcol,msrow,ncol,nrow,
     +                    fld,wfld,igds,mxbmap,ibmap,nwarn,ierr)
c
c.......................................................................
c
c  The routine fills igds array (grid definition) with parameters for
c  desired grid.  Origin point of the grid, number of points along the
c  x-axis and number of points along the y-axis are placed in igds.  If
c  translation to another grid (other than HRAP) is needed, grid values
c  in array fld are copied to array wfld.  Then parameters for the other
c  grid (only grid 218 available, so far) are obtained from an NCEP
c  routine.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                   Feb 2000
c.......................................................................
c
      real tg(8),fld(*),wfld(*)
c
      integer ng(8)
      integer igds(*),ibmap(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/griddef.f,v $
     . $',                                                             '
     .$Id: griddef.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('griddef',1,1,ibug2)
ccc      ibug = 1
c
      ierr = 0
      idblev = 2
      ktr = 0
      nktr = 0
c
c  get Grid Description Section information (GRIB parameters)
      call w3fi71 (ngrid,igds,ierr)
      if (ierr.gt.0) then
         write (iutw,80) ngrid
         if (iupr.ne.iutw) write (iupr,80) ngrid
80    format (' ERROR in griddef: GRIB grid number ',i4,' is invalid.')
         go to 210
         endif
      if (ibug.eq.1) write (iupr,90) ngrid,(igds(i),i=1,18)
90    format (' ngrid=',i4,' igds(1...18)=',5i5,2i8,i4,3i8,7i6)
c
c  transfer grid to work grid array wfld
      ncolh = ncol
      nrowh = nrow
      mwcolh = mwcol
      msrowh = msrow
c
      npts = ncolh*nrowh
      do 10 i=1,npts
         wfld(i)= fld(i)
10       continue
c
      if (ngrid.eq.218) go to 70
      if (ngrid.eq.240) go to 15
      write (iutw,13) ngrid
      if (iupr.ne.iutw) write (iupr,13) ngrid
13    format (' ERROR in griddef: GRIB grid number ',i4,
     +   ' cannot be processed.')
      nerr = nerr + 1
      ierr = 1
      go to 210
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  GRIB grid number 240 - 4 km HRAP grid over contiguous United States
c
c  set to local grid for hrap
15    inat = 0
c
      if (inat.eq.1) then
c     setup for national scale
         ncol = igds(4)
         nrow = igds(5)
         koff = (msrowh-1)*ncol + mwcolh - 1
         else
c        overwrite national values
            koff = 0
c        number of points along x-axis
            igds(4) = ncol
c        number of points along y-axis
            igds(5) = nrow
c        convert southwest corner HRAP grid to lat/lon
            npair = 1
            illgd = 0
            addval = 0.01
            xhrap = mwcol + addval
            yhrap = msrow + addval
            call cvllgd (elon,alat,npair,xhrap,yhrap,illgd,ierr)
            if (ierr.gt.0) then
               write (iutw,30)
               if (iupr.ne.iutw) write (iupr,30)
30    format (' ERROR in griddef: converting HRAP ',
     +   'southwest corner to latitude and longitude.')
               go to 210
               endif
            if (ibug.eq.1) write (iutw,*) 'in griddef - addval=',addval,
     +         ' xhrap=',xhrap,' yhrap=',yhrap,
     +         ' elon=',elon,' alat=',alat
            mwcol1=xhrap
            msrow1=yhrap
c        convert southwest corner lat/lon to HRAP grid
            illgd = 1
            call cvllgd (elon,alat,npair,xhrap,yhrap,illgd,ierr)
            if (ierr.gt.0) then
               write (iupr,35)
35    format (' ERROR in griddef: converting latitude and longitude ',
     +   'southwest corner to HRAP.')
               go to 210
               endif
            if (ibug.eq.1) write (iutw,*) 'in griddef - addval=',addval,
     +         ' elon=',elon,' alat=',alat,
     +         ' xhrap=',xhrap,' yhrap=',yhrap
            mwcol2=xhrap
            msrow2=yhrap
            if (mwcol1.ne.mwcol2) then
               write (iutw,37) 'column',mwcol2,mwcol1
               if (iupr.ne.iutw) write (iupr,37) 'column',mwcol2,mwcol1
37    format (' WARNING in griddef: HRAP ',a,
     +   ' number computed from lat/lon (',
     +   i4,') is not the same as from xmrg file (',i4,').')
               endif
            if (msrow1.ne.msrow2) then
               write (iutw,37) 'row',msrow2,msrow1
               if (iupr.ne.iutw) write (iupr,37) 'row',msrow2,msrow1 
               endif
c        latitude of origin
            igds(6) = alat*1000.0
c        longitude of origin
            igds(7) = -elon*1000.0
         endif
c
c  initialize bit map array
      mbmap = ncol*nrow
      do 40 i=1,mbmap
         fld(i) = -50.0
         ibmap(i) = 0
40       continue
c
      if (ibug.eq.1) write (iutw,*) 'in griddef - nrowh=',nrowh,
     +   ' ncolh=',ncolh
      do 60 j=1,nrowh
         do 55 i=1,ncolh
            kh = (j-1)*ncolh + i
            kg = (j-1)*ncol + i + koff
            if (wfld(kh).ge.0.0) then
               fld(kg) = wfld(kh)
               ibmap(kg) = 1
               ktr = ktr + 1
               if (ibug.gt.2) then
                  write (iupr,50) ngrid,ktr,kg,fld(kg),i,j
50        format (' ngrid=',i3,' ktr=',i6,' kg=',i6,' fld(kg)=',f6.2,
     +            ' i=',i4,' j=',i4)
                  else if (ibug.gt.1.and.fld(kg).gt.0.0) then
                     write (iupr,50) ngrid,ktr,kg,fld(kg),i,j
                  endif
               else
                  fld(kg) = 0.0
                  nktr = nktr + 1
               endif
            if (ibug.eq.1)  write (iutw,*) 'in griddef -',
     +         ' kh=',kh,' wfld(kh)=',wfld(kh),
     +         ' kg=',kg,' fld(kg)=',fld(kg)
55          continue
60       continue
c
      go to 180
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
c  GRIB grid number 218 - 12 km AWIPS grid over contiguous United States
c
c  must transform from 4 km HRAP grid to 12 km AWPIS grid
c
c  set parameters to transform to grid 218
70    alat1 = igds(6)/1000.
      elon1 = igds(7)/1000.
      dx = igds(10)
      elonv = igds(9)/1000.
      alatan = igds(15)/1000.
c
c  put HRAP coordinates of corners into an array
      ng(1) = mwcol
      ng(2) = msrow
      ng(3) = mwcol + ncolh - 1
      ng(4) = msrow
      ng(5) = mwcol + ncolh - 1
      ng(6) = msrow + nrowh - 1
      ng(7) = mwcol
      ng(8) = msrow + nrowh - 1
      illgd = 0
      npair = 1
c  use center of HRAP grid
      do 120 i = 1,8,2
         xhrap = ng(i) + 0.5
         yhrap = ng(i+1) + 0.5
c     convert HRAP corners to lat lon
         call cvllgd (elon,alat,npair,xhrap,yhrap,illgd,ierr)
         if (ibug.ge.idblev) write (iupr,100) xhrap,yhrap,elon,alat,ierr
100   format (' xhrap=',f6.0,' yhrap=',f6.0,' elon=',f7.3,
     +   ' alat=',f6.3,' ierr=',i2)
c     convert corners in lat lon to Lambert(i,j) grid 218 coordinates
         elon = -elon
         call w3fb11 (alat,elon,alat1,elon1,dx,elonv,alatan,tg(i),
     +      tg(i+1))
         if (ibug.ge.idblev) write (iupr,110) elon,alat,tg(i),tg(i+1)
110   format (' elon=',f8.3,' alat=',f6.3,' tg(i)=',f8.3,
     +   ' tg(i+1)=',f8.3)
120      continue
c
      lwcol = min(tg(1),tg(7)) + 0.5
      lsrow = min(tg(2),tg(4)) + 0.5
      xl = min(tg(3),tg(5))
      yl = min(tg(6),tg(8))
      lncol = xl - lwcol + 1
      lnrow = yl - lsrow + 1
      if (ibug.ge.idblev) write (iupr,130) ngrid,lwcol,lsrow,lncol,lnrow
130   format (' ngrid=',i4,' lwcol=',i4,' lsrow=',i4,
     +    ' lncol=',i4,' lnrow=',i4)
c
      if (inat.eq.1) then
c     reset with national grid parameters
         ncol = igds(4)
         nrow = igds(5)
         msrow = 1
         mwcol = 1
         koff = (lsrow-1)*ncol + lwcol - 1
         else
c        reset with local grid parameters
            mwcol = lwcol
            msrow = lsrow
            ncol = lncol
            nrow = lnrow
            koff = 0
c        number of points along x-axis
            igds(4) = ncol
c        number of points along y-axis
            igds(5) = nrow
c        determine lat lon of local point (1,1)
            xl = lwcol
            yl = lsrow
            call w3fb12 (xl,yl,alat1,elon1,dx,elonv,alatan,alat,elon,
     +         ierr)
c        convert to east longitude
            if (elon.gt.180.) elon = 360. - elon
c        latitude of origin
            igds(6) = alat*1000.
c        longitude of origin
            igds(7) = -elon*1000.
         endif
c
c  initialize bitmap array
      mbmap = ncol*nrow
      if (mbmap.gt.mxbmap) then
         write (iutw,140) mbmap,mxbmap
         if (iupr.ne.iutw) write (iupr,140) mbmap,mxbmap
140   format (' ERROR: number of words needed for bitmap array (',i6,
     *   ') exceeds the maximum (',i6,').')
         ierr = 3
         go to 210
         endif
      do 150 i=1,mbmap
         fld(i) = 0.0
         ibmap(i) = 0
150      continue
c
      mecolh = mwcolh + ncolh - 1
      mnrowh = msrowh + nrowh - 1
      illgd = 1
      do 170 j=1,lnrow
         do 165 i=1,lncol
            xl = i + lwcol - 1
            yl = j + lsrow - 1
c        convert Lambert (xl,yl) to lon lat
            call w3fb12 (xl,yl,alat1,elon1,dx,elonv,alatan,alat,elon,
     +         ierr)
c        convert to east longitude
            if (elon.gt.180.0) elon = 360.0 - elon
            if (ibug.ge.idblev.and.i.le.3.and.j.le.3)
     +         write (iupr,160) i,j,xl,yl,elon,alat,ierr
160   format (' i=',i4,' j=',i4,' xl=',i4,' yl=',i4,
     +        ' elon=',f7.3,' alat=',f6.3,' ierr=',i4)
c        convert lon lat to national HRAP (ihrap,jhrap)
            call cvllgd (elon,alat,npair,xhrap,yhrap,illgd,ierr)
c        check if HRAP coordinates in local HRAP box
            if (xhrap.lt.mwcolh.or.xhrap.gt.mecolh) go to 170
            if (yhrap.lt.msrowh.or.yhrap.gt.mnrowh) go to 170
c        convert national HRAP coordinate to local
            ihrap = xhrap - mwcolh + 1
            jhrap = yhrap - msrowh + 1
            kh = (jhrap-1)*ncolh + ihrap
            kg = (j - 1)*ncol + i + koff
            if (wfld(kh).ge.0.0) then
               fld(kg) = wfld(kh)
               ibmap(kg) = 1
               ktr = ktr + 1
               if (ibug.gt.2) write (iupr,50) ngrid,ktr,kg,fld(kg),i,j
               else
                  fld(kg) = 0.0
                  nktr = nktr + 1
               endif
165         continue
170      continue
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
180   if (ibug.gt.0) write (iupr,190) ngrid,ktr,nktr
190   format (' ngrid ',i3,' ktr=',i6,' nktr=',i6)
      if (ktr.eq.0) then
         write (iutw,200)
         if (iupr.ne.iutw) write (iupr,200)
200   format (' WARNING: all data is missing.')
         nwarn = nwarn + 1
         ierr = ierr + 2
         endif
c
210   return
c
      end
