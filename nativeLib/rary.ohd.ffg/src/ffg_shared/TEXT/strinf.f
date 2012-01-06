c  =====================================================================
c  pgm:  strinf (iunit,po)
c
c   in: iunit  .... unit number
c   in: po     .... parameter array
c  =====================================================================
c
      subroutine strinf (iunit,po)
c
c.......................................................................
c
c  routine to write user controls to file
c
c.......................................................................
c
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iuws'
      include 'ffg_inc/uinfo'
c
      character*2 bname
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/strinf.f,v $
     . $',                                                             '
     .$Id: strinf.f,v 1.4 2003/08/20 13:14:54 scv Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
      bname = ' '
c
c  version number
      uvers = 2.43
      po(1) = uvers
c
c  identifier
      call umemov (usrid,po(2),2)
c
c  data type
      call umemov (usrtyp,po(4),1)
c
c  location of extrema data
      loed = 20
      po(6) = float(loed)
c
c  location of options
      loop = 40
      po(7) = float(loop)
c
c  location of HRAP data
      loch = 56
      po(8) = float(loch)
c
c  location of water supply information
      lows = 62
      po(9) = float(lows)
c
c  location of land slide guidance information
      lols = 72
      po(10) = float(lols)
c
c  unused
      po(11) = -999.0
      po(12) = -999.0
c
c  location of product generation options
      lopg = 81
      po(13) = float(lopg)
c
c  warning and error messages output flag
      po(14) = float(iweout)
c
c  alternate OFS files
      po(15) = float(iofs)
c
c  new feature control
      po(16) = float(nfeat)
c
c  unused
      do 10 i=17,loed
         po(i) = -999.0
10       continue
c
c  store extrema values
      k = loed - 1
      do 20 i=1,20
         po(i+k) = ext(i)
20       continue
c
c  store options
      po(loop)   = irctlg*1.0
      po(loop+1) = irctlh*1.0
      po(loop+2) = iqctlg*1.0
      po(loop+3) = iqctlh*1.0
      po(loop+4) = iameth
c
c  bankfull factor
      po(loop+5) = gbank
c
c  computer sys time zone
      call umemov (cpzone,po(loop+6),1)
c
c  from ofs, the hour offset to local time, ofs variable local
      po(loop+7) = float(loclo)
c
c  from ofs, time zone number of local standard time
      po(loop+8) = float(nlstzo)
c
c  from ofs, user name
      call umemov (usrnam,po(loop+9),2)
c
c  extend number of columns and rows
      po(loop+11) = ngfil
c
c  check decreasing ffg values
      po(loop+12) = float(ickval)
c
c  unused
      k = loop + 12
      do 30 i=1,3
          po(k+i) = -999.
30        continue
c
c  store HRAP grid sw corner
      po(loch)   = float(mwcol)
      po(loch+1) = float(ncol)
      po(loch+2) = float(msrow)
      po(loch+3) = float(nrow)
c
c  unused
      j = loch + 4
      k = lows - 1
      do 40 i=j,k
         po(i) = -999.0
40       continue
c
c  water supply control
      po(lows) = iwats
c
c  water supply rainfall amounts
      do 50 i=1,6
         po(lows+i) = rnfl(i)
50       continue
c
c  unused
      j = lows + 7
      k = lols - 1
      do 60 i=j,k
         po(i) = -999.0
60       continue
c
c  land slide control
      po(lols) = lslide
c
c  unused
      j = lols + 1
      k = lopg -1
      do 70 i=j,k
         po(i) = -999.0
70       continue
c
c  SHEF file(s)
      po(lopg) = ising*1.0
c
c  control for comms system
      po(lopg+1) = float(icom)
c
c  control for SHEF FFG physical element code
      po(lopg+2) = float(iffpe)
c
c  control for including century with year
      po(lopg+3) = float(icent)
c
c  unused
      j = lopg + 3
      do 80 i=1,3
         po(j+i) = -999.0
80       continue
c
c  control to append duty forecaster
      po(lopg+7) = float(igduty)
c
c  number of forecasters
      po(lopg+8) = mdf
      iuseu = lopg + 8
c
c  forecasters
      if (igduty.gt.0.and.mdf.gt.0) then
         n = mdf*16
         call umemov (fcstr,po(lopg+9),n)
         iuseu = iuseu + n
         endif
c
c  set number of words used for parameters
      po(5) = float(iuseu)
c
      if (ibug.eq.1) write (iud,*) 'in strinf - iunit=',iunit
c
c  write to file
      call wppfil (iunit,iuseu,po,istat)
      call pstcod (istat,usrid,usrtyp,iunit)
      call upclos (iunit,bname,istat)
      write (iutw,90)
      if (iupr.ne.iutw) write (iupr,90)
90    format (' NOTE: user controls written to file.')
c
      return
c
      end
