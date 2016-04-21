c  =====================================================================
c  pgm:  cparea
c
c  variables are passed in common blocks
c
c  =====================================================================
c
      subroutine cparea
c
c.......................................................................
c
c  compute areal ffg values based on gridded ffg values within boundary
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                  March 1992
c
c  Added option to use threshold runoff for guidance value.
c        Tim Sweeney, HRL                                  Mar 1999
c
c  Added option to check for decreasing FFG values as
c  duration increas and, if found, set value to that of
c  previous duration.
c        Tim Sweeney, HRL                                  Sep 1999
c
c  Added capability to run one identifier for testing purposes.
c        Tim Sweeney, HL                                   May 2001
c.......................................................................
c
      character*2 bname
      character*4 bbtyp,cnone,code
      character*4 gfftyp
      character*4 ffgtyp
      character*8 aproc(5),ident
      character*8 ffgid,areaidz,basnid,bbidz
      character*20 sdatim,vdatim
      character*50 strng
      parameter (mffgar=500)
      dimension ffgar(mffgar)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/arparm'
      include 'ffg_inc/count'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/ffgrid'
      include 'ffg_inc/gpo'
      include 'ffg_inc/gidx'
      include 'ffg_inc/gbx'
      include 'ffg_inc/linseg'
      include 'ffg_inc/ghfld'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/cparea.f,v $
     . $',                                                             '
     .$Id: cparea.f,v 1.6 2004/09/13 14:23:00 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('cparea',1,1,ibug)
c
      ibug2=0
      ibug2u = iud
      if (iupr.ne.iutw) ibug2u = iupr
c
      artyp = 'affg'
      bname = ' '
      cnone = 'none'
c
      strng='area FFG guidance'
c
      nearea = nerr
      nwarea = nwarn
c
c  get user info
      kod = 0
      call getinf (kod,mpo,po,iunit,istat)
      if (istat.eq.0) then
c     determine number of durations from extrema
         ndur = mxdurg
         else
            write (iutw,10) strng(1:lenstr(strng))
            if (iupr.ne.iutw) write (iupr,10) strng(1:lenstr(strng))
10    format (' WARNING: computations for ',a,' will not be done ',
     +   'because user controls are not defined.')
            go to 350
         endif
c
c  check if area (grid based) calculations are desired
      if (ndur.lt.0) then
         write (iutw,20) strng(1:lenstr(strng)),ndur
         if (iupr.ne.iutw) write (iupr,20) strng(1:lenstr(strng)),ndur
20    format (' WARNING: computations for ',a,' will not be done ',
     +   'because the value of the user control option is ',i2,' .')
         go to 330
         endif
c
      if (ibug.gt.0) write (iud,*) 'in cparea : mwcol=',mwcol,
     +   ' ncol=',ncol,' msrow=',msrow,' nrow=',nrow
c
      if (kgridf.eq.0) then
c     read gridded guidance
         gfftyp='grff'
         ibline = 1
         call getgrd (gfftyp,idurt,ndur,nrow,ncol,mxd,mxr,mxc,
     +                msrow,mwcol,xver,usrnam,sdatim,aproc,vdatim,
     +                mxval,ihfld,gg,ibline,istat)
c     get ffg computation date from vdatim
         imo = -1
         call infxdt (imo,ida,iyr,ihr,imn,isc,vdatim)
         call fctzc (0,0,code)
         call julda (jda,jhr,imo,ida,iyr,ihr,0,0,code)
         lffcpd = (jda - 1)*24 + jhr
         else
            write (iutw,30)
            if (iupr.ne.iutw) write (iupr,30)
30    format (/ ' Gridded guidance already read from database.')
         endif
c
c  get index to affg files
      call getidx (artyp,idxdv,mcidx,po,ng,cidx,istat)
      call upclos (idxdv,bname,ic)
c
c  compute for a single identifier for testing
      if (singid.ne.'none') then
         ng = 1
         if (singid.eq.'ibug2') then
            iset = 1
            if (iset.eq.1) then
               areaidz='NYZ075'
               call umemov (areaidz,cidx(1,1),2)
               ibug2=1
               write (iutw,40) ibug2,areaidz
               if (iupr.ne.iutw) write (iupr,40) ibug2,areaidz
40    format (/ ' WARNING: in cparea - variable ibug2 set to ',i1,
     +   ' for identifier ',a,'.')
               endif
            else
               call umemov (singid,cidx(1,1),2)
               areaidz=singid
            endif
         write (iutw,50) areaidz
         if (iupr.ne.iutw) write (iupr,50) areaidz
50    format (/ ' NOTE: computations will be done for identifier ',
     +   a,'.')
         endif
c
      write (iutw,60) strng(1:lenstr(strng)),ng
      if (iupr.ne.iutw) write (iupr,60) strng(1:lenstr(strng)),ng
60    format (/ ' Computing ',a,' for ',i4,' areas.')
c
c  process each area
      do 320 la=1,ng
         if (cidx(1,la).ne.cnone) go to 70
         if (cidx(2,la).eq.cnone) go to 320
70       areaid(1) = cidx(1,la)
         areaid(2) = cidx(2,la)
         if (areaid(1).eq.' ') go to 320
         call umemov (areaid,areaidz,2)       
c     open affg file
         call umemov (cidx(1,la),ident,2)
         kod = 1
         call rppfil (ident,artyp,kod,ipdv,mpo,po,npo,istat)
         if (istat.eq.0) then
c     get affg parameters from file
            call getar (po)
            if (ibug.gt.0) write (iupr,80) areaid
80    format (' in cparea : areaid=',2a4)
            else
               write (iutw,90) artyp,areaid
               if (iupr.ne.iutw) write (iupr,90) artyp,areaid
90    format (' ERROR: ',a,' parameters not found for identifier ',
     +   2a4,'.')
               nerr = nerr + 1
               go to 310
            endif
c     determine number of durations, ndur, to compute based on
c     global value, mxdurg, and duration flag for this area, kadurf
         ndur = mxdurg
         if (nadur.lt.ndur) ndur = nadur
         if (iropta.ne.0) go to 210
c     get boundary for an area (county, zone area)
         bbtyp = 'BASN'
         call getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,nlrow,nlbeg,
     +               nlend,istat)
         if (istat.gt.0) then
            ierr = ierr + 1
c        initialize area ffg
            do 100 idr=1,ndur
               po(laffg+idr-1) = -9.7
100            continue
            ncptd = 0
            nocpt = 0
            noro = 0
            go to 250
            endif
         ickffg = 0
         if (ickffg.eq.1) then
c        check if used in FFG parameter record
            ffgtyp = 'FFG '
            irec = 0
            ifound = 0
75          ffgid = ' '
            call rpprec (ffgid,ffgtyp,irec,mffgar,ffgar,nfill,irecnx,
     +         istat)
            if (istat.eq.0) then
               call umemov (bbid,bbidz,2)
               call umemov (ffgar(9),basnid,2)
               if (bbidz.eq.basnid) then
                  ifound = 1
                  go to 74
                  endif
               if (irecnx.gt.0) then
                  irec = irecnx
                  go to 75
                  endif
               endif
74          if (ifound.eq.0) then
               write (iud,76) bbidz
76    format (' in cparea - basin id ',a,' not found in any FFG ',
     +   'parameter records')
               else
                  write (iud,77) bbidz,ffgid
77    format (' in cparea - basin id ',a,' found in FFG ',
     +   'parameter record ',a)
               endif
            endif
c     calculations for each duration
         do 190 idr=1,ndur
            ncptd = 0
            nocpt = 0
            noro = 0
            sum = 0.
            pmn = 99.9
            idur = idurt(idr)
c        calculations for each line segment within area
            lbcol=999
            lecol=0
            do 170 lseg=1,nlseg
               irow  = nlrow(lseg)
               ncbeg = nlbeg(lseg)
               ncend = nlend(lseg)
               lrow  = irow - msrow + 1
               if (ibug.gt.1) write (iud,110) lseg,irow,lrow,ncbeg,ncend
110   format (' in cparea : lseg=',i2,' irow=',i4,' lrow=',i3,
     +   ' ncbeg=',i4,' ncend=',i4)
c           set control direction in line segment
               idel = 1
               if (ncend.lt.ncbeg) idel = -1
c           calculations for each grid in line segment
               do 160 icol=ncbeg,ncend,idel
                  lcol = icol - mwcol + 1
                  if (lcol.lt.lbcol) lbcol=lcol
                  if (lcol.gt.lecol) lecol=lcol
                  ff = gg(idr,lrow,lcol)
                  if (ibug.eq.1) write (iud,*) ' idr=',idr,
     +   ' irow=',irow,' icol=',icol,' ff=',ff
                  if (ff.gt.0.004.and.ff.le.20.0) then
                     sum = sum + ff
                     ncptd = ncptd + 1
                     if (ff.lt.pmn) pmn = ff
                     else if (ff.le.-9.8) then
c                    no ffg computed runoff defined
                        if (ibug.gt.0) write (iud,120) irow,icol,ff
120   format (' in cparea : no FFG computed for for irow=',i4,
     +   ' icol=',i4,' (ff=',f6.3,')')
                        nocpt = nocpt + 1
                     else
c                    no runoff defined
                        if (ibug.gt.0) write (iud,130) irow,icol,ff
130   format (' in cparea : no runoff defined for irow=',i4,
     +   ' icol=',i4,' (ff=',f6.3,')')
                        noro = noro + 1
                     endif
                  if (ibug.gt.0) write (iud,140) idur,
     +               lseg,irow,icol,lrow,lcol,ff,pmn,sum,ncptd
140   format (' in cparea : idur=',i2,' lseg=',i3,
     +   ' irow=',i4,' icol=',i4,' lrow=',i3,' lcol=',i3,
     +   ' ff=',f6.3,' pmn=',f6.3,' sum=',f6.2,' ncptd=',i3)
                  if (ibug2.eq.1) then
                     write (ibug2u,150) idur,
     +                  lseg,
     +                  irow,icol,
     +                  lrow,lcol,
     +                  lbcol,lecol,
     +                  ff,pmn,sum,ncptd
150   format (' in cparea - idur=',i2,
     +   ' lseg=',i3,
     +   ' irow=',i4,' icol=',i4,
     +   ' lrow=',i3,' lcol=',i3,
     +   ' lbcol=',i3,' lecol=',i3,
     +   ' ff=',f6.3,' pmn=',f6.3,' sum=',f6.2,' ncptd=',i3)
                     endif
160               continue
170            continue
c        check number of bins computed
            if (ncptd.eq.0) then
c           no grids computed
               iprint = 1
               if (nocpt.eq.0) then
c              no grids not computed
                  p = -9.4
                  if (iprint.eq.1) then
                     write (iupr,175) 'computed',idurt(idr),areaid
175   format (' ERROR: no grids ',a,' for ',
     +   i2,' hour duration for area ',2a4,'.')
                     nerr = nerr + 1
                     endif
               else if (noro.eq.0) then
c              no grids with runoff
                  p = -9.2
                  if (iprint.eq.1) then
                     write (iupr,175) 'have runoff',idurt(idr),areaid
                     nerr = nerr + 1
                     endif
                  else
                     p = -9.6
                  endif
               go to 180
               endif
            if (iameth.eq.1) then
c           use minimum grid value in area
               p = pmn
               else
c              compute average of grid values in area
                  p = sum/ncptd
               endif
180         if (ibug.gt.0) write (iud,*) 'in cparea : idr=',idr,
     +         ' iameth=',iameth,' p=',p
c        store area value in po array
            po(laffg+idr-1) = p
            mtot = ncptd + nocpt + noro
            call umemov (bbid,bbidz,2)
            if (ncptd.eq.0) then
               write (iutw,185) 'ERROR:',areaidz,idurt(idr),
     +            bbidz,ncptd,nocpt,noro,mtot
               if (iupr.ne.iutw)
     +            write (iupr,185) 'ERROR:',areaidz,idurt(idr),
     +               bbidz,ncptd,nocpt,noro,mtot
185   format (' ',a,t11,'Area_ID=',a,t28,'dur=',i2,' BASN_id=',a,
     +   '  grids: ncptd=',i4,' nocpt=',i4,' noro=',i4,' total=',i4)
               nerr = nerr + 1
               else if (noro.ne.0) then
                  iprint=1
                  if (iprint.eq.1) then
                     write (iutw,185) 'WARNING:',areaidz,idurt(idr),
     +                  bbidz,ncptd,nocpt,noro,mtot
                     if (iupr.ne.iutw)
     +                  write (iupr,185) 'WARNING:',areaidz,idurt(idr),
     +                     bbidz,ncptd,nocpt,noro,mtot
                     nwarn = nwarn + 1
                     endif
               else
                  write (iupr,185) ' ',areaidz,idurt(idr),
     +               bbidz,ncptd,nocpt,noro,mtot
               endif
190         continue
c     recompute 1-hr ffg as percent of 3-hr ffg
         if ((aro(1).lt.0.0).and.(aro(1).gt.-10.0))
     +      po(laffg) = -aro(1)*po(laffg+1)
c     check for decreasing FFG
         if (ickval.gt.0) then
            do 200 idr=2,ndur
               a = po(laffg+idr-2)
               b = po(laffg+idr-1)
               if (a.le.0.0.or.b.le.0.0) go to 250
               if (b.lt.a) po(laffg+idr-1) = po(laffg+idr-2)
200            continue
            endif
         go to 250
c     use runoff as guidance
210      if (iropta.ne.3) go to 230
            do 220 idr=1,ndur
               p = aro(idr)
               if (p.gt.0.0) then
                  ncptd = 1
                  nocpt = 0
                  noro = 0
                  else
                     p = -9.1
                     ncptd = 0
                     nocpt = 0
                     noro = 1
                  endif
               po(laffg+idr-1) = p
220            continue
            go to 250
c      control error
230      do 240 idr=1,ndur
            po(laffg+idr-1) = -9.8
240         continue
         nocpt = 0
         noro = 0
c     store ffg computational date in po array
250      po(laffg-1) = lffcpd
c     check FFG values
         do 280 idr=1,ndur
            val1=po(laffg+idr-1)
            if (val1.lt.0) then
               if (val1.eq.-9.2) go to 280
               if (val1.eq.-9.4) go to 280
               write (iutw,260) idurt(idr),val1,areaid
               if (iupr.ne.iutw) write (iupr,260) idurt(idr),val1,areaid
260   format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f5.1,
     +   ') is less than zero for area ',2a4,'.')
               go to 280
               endif
            if (idr.eq.ndur) go to 280
            val2=po(laffg+idr)
            if (val2.gt.0.and.val1.gt.val2) then
               write (iutw,270) idurt(idr+1),val2,
     +            idurt(idr),val1,areaid
               if (iupr.ne.iutw) write (iupr,270) idurt(idr+1),val2,
     +            idurt(idr),val1,areaid
270   format (' WARNING: FFG value for ',
     +   i2,' hour duration (',f6.3,
     +   ') is less than value for ',
     +   i2,' hour duration (',f6.3,
     +   ') for area ',2a4,'.')
               endif
280         continue
         write (iupr,290) areaidz,(po(laffg+idr-1),idr=1,ndur)
290   format (' ',t11,'Area_ID=',a,' FFG_val=',(5(f5.2,1x)))
c     write to file
         rewind (ipdv)
         call wppfil (ipdv,npo,po,istat)
         call pstcod (istat,areaid,artyp,ipdv)
         narea = narea + 1
310      call upclos (ipdv,bname,ic)
320      continue
c
330   if (narea.gt.0) then
         write (iutw,340)
         if (iupr.ne.iutw) write (iupr,340)
340   format (/ t5,'Grid computation values:' /
     +         t10,'ncptd = grids computed' /
     +         t10,'nocpt = grids not computed' /
     +         t10,'noro  = grids with no runoff' /
     +         t10,'total = total grids'
     +        )
         endif
c
350   nearea = nerr - nearea
      nwarea = nwarn - nwarea
c
      return
c
      end
