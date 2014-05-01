c  =====================================================================
c  pgm:  duuinf (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine duuinf (po,iout)
c
c.......................................................................
c
c  write user control parameters to file

c.......................................................................
c
      integer labl(25)
     +         / 1, 1, 2,11,12,
     +           3, 3, 4,13,14,
     +           6, 5, 6,15,16,
     +          12, 7, 8,17,18,
     +          24, 9,10,19,20/
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/uinfo'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/duuinf.f,v $
     . $',                                                             '
     .$Id: duuinf.f,v 1.3 2004/01/30 17:47:13 scv Exp $
     . $' /
C    ===================================================================
C
c
      write (iout,'(a)') 'uinf_guid'
c
c  maximum and minimum extremum for Grids and Headwaters for each 
c  duration
      do 20 i=1,5
         ii = (i-1)*5 + 1
         ij = ii + 1
         ik = labl(ij)
         il = ii + 2
         im = labl(il)
         in = ii + 3
         ji = labl(in)
         jj = ii + 4
         jk = labl(jj)
         write (iout,10) ext(ik),ext(im),ext(ji),ext(jk)
10    format (f5.1,1x,f5.1,1x,f5.1,1x,f5.1)
20       continue
c
c  bankfull factor, 
c  runoff adjust for grids, runoff adjust for headwaters, 
c  high flow adjust for grids, high flow adjust for headwaters and
c  area method
      write (iout,30) gbank,irctlg,irctlh,iqctlg,iqctlh,iameth
30    format (f5.2,1x,
     +   i2,1x,i2,1x,
     +   i2,1x,i2,1x,
     +   i2)
c
c  computer system time zone, user name, hour offset to local time,
c  time zone number of local standard time
      write (iout,40) cpzone,usrnam,loclo,nlstzo
40    format (a,1x,a,1x,i2,1x,i2)
c
c  HRAP grid subset
      write (iout,50) mwcol,ncol,msrow,nrow
50    format (4(i5,1x))
c
c  grid fill control
      write (iout,60) ngfil
60    format (i2)
c
c  check decreasing FFG
      write (iout,70) ickval
70    format (i2)
c
ccc      if (nfeat.gt.0) then
c  water supply runoff
         write (iout,80) iwats
80    format (i2)
         if (iwats.gt.0) then
c        rainfall
            write (iout,90) rnfl
90    format (6(f4.1,1x))
            endif
ccc      endif
c
c- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
c
      write (iout,'(a)') 'uinf_prod'
c
c  FFG product files, comms system, FFG physical elements, year format,
c  append name of duty forecaster
      write (iout,100) ising,icom,iffpe,icent,igduty
100   format (i2 / i2 / i2 / i2 / i2)
c
      if (igduty.gt.0) then
c     duty forecasters
         if (mdf.gt.0) then
            write (iout,110) mdf
110   format (i2)
            nperg=16
            ix1=1
            ix2=nperg
            do 130 i=1,mdf
               write (iout,120) (fcstr(j),j=ix1,ix2)
120   format (16a4)
               ix1=ix1+nperg
               ix2=ix2+nperg
130            continue
            endif
         endif
c
      return
c
      end
