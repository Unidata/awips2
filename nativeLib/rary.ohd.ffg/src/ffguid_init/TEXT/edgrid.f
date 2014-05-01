c  =====================================================================
c  pgm:  edgrid (mdur,mrow,mcol,gtype,mbx,bx,ibx,ihfld,ichnge,istat)
c
c   in: mdur   .... maximum number of durations
c   in: mrow   .... maximum number of grid rows
c   in: mcol   .... maximum number of grid columns
c  out: gtype  .... gridded data type
c   in: mbx    .... maximum number of words in arrays bx and ibx
c   in: bx     .... array for basin boundary parameters (dimension only)
c   in: ibx    .... integer array for above
c   in: ihfld  .... parameter array for dimension only
c  i/o: ichnge .... indicator if values changed
c  out: istat  .... status code
c  =====================================================================
c
      subroutine edgrid (mdur,mrow,mcol,gtype,mbx,bx,ibx,ihfld,ichnge,
     +   istat)
c
c.......................................................................
c
c  This routine is used to display and change individual grid values.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 25, 1993
c
c  Modified to display national HRAP row and column numers
c       Tim Sweeney, HRL                                   Nov 1995
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/uinfo'
      include 'ffg_inc/rogrid'
      include 'ffg_inc/gridsz'
      include 'ffg_inc/linseg'
c
      character*1 resp,geog
      character*4 gfftyp,grotyp,gtype
      character*8 aproc(5)
      character*20 sdatim,vdatim
      character*50 strng
      character line*80,dlim*1,word*8
c
      integer*2 ihfld(*)
      dimension bbid(2)
      parameter (mval=32)
      dimension ival(mval)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/edgrid.f,v $
     . $',                                                             '
     .$Id: edgrid.f,v 1.5 2003/08/20 13:08:29 scv Exp $
     . $' /
C    ===================================================================
C
      data grotyp /4hgrro/
      data gfftyp /4hgrff/
      data bbtyp / 4hBASN/
c
c
      call prbug ('edgrid',1,1,ibug)
c
c  set initial window
      kdrow = 26
      kdcol = 30
      inrow = kdrow
      iwcol = 1
      geog = 'a'
c
10    write (iutw,20)
20    format (/ 25x,'EDIT GRIDDED FIELD' //
     +   ' Enter option (r-runoff  g-FF guidance  [m]-menu): ',$)
c
      read (iutr,30,err=10) resp
30    format (a1)
      if (resp.eq.' ') then
         go to 350
         else if (resp.eq.'r'.or.resp.eq.'R') then
            gtype = grotyp
         else if (resp.eq.'g'.or.resp.eq.'G') then
            gtype = gfftyp
         else if (resp.eq.'m'.or.resp.eq.'M') then
            go to 350
         else
            write (iutw,40) resp(1:lenstr(resp))
40    format (' ERROR: ',a,' is an invalid option.')
            go to 10
         endif
c
      if (kgridf.eq.0) then
c     read xmrg file
         ibline = 1
         call getgrd (gtype,idurt,mdur,mrow,mcol,mxd,mxr,mxc,
     +                msrow,mwcol,gvers,usrnam,sdatim,aproc,vdatim,
     +                mxval,ihfld,tro,ibline,istat)
         kgridf = 2
         endif
c
      write (iutw,*)
c
c  change display window size
50    write (iutw,60) kdrow,kdcol
60    format (' Change window size ',
     +        '([',i2,'] rows [',i2,'] columns): ',$)
      read (iutr,'(a)',err=50) line
      if (line.ne.' ') then
         dlim=' '
         nscan=1
         call uscan2 (line,dlim,nscan,word,lword,istat)
         ibeg=1
         iprerr=1
         call ufa2i (word,ibeg,lword,i,iprerr,iutw,istat)
         nscan=nscan+1
         call uscan2 (line,dlim,nscan,word,lword,istat)
         if (word.ne.' '.and.word.ne.'.') then
            call ufa2i (word,ibeg,lword,j,iprerr,iutw,istat)
            else
               j=0
            endif
         inderr=0
         if (i.gt.0) then
            maxval=50
            if (i.le.maxval) then
               kdrow = i
               else
                  write (iutw,75) 'row',i,maxval,maxval
75    format (' WARNING: ',a,' ',i4,' is greater than ',i2,
     +   ' and will be set to ',i2,'.')
                  kdrow = maxval
               endif
            inrow = kdrow
            endif
         if (j.gt.0) then
            maxval=30
            if (j.le.maxval) then
               kdcol = j
               else
                  write (iutw,75) 'column',j,maxval,maxval
                  kdcol = maxval
               endif
            endif
         if (inderr.eq.1) go to 50
         endif
c
      strng=' '
      ibeg=1
      iprerr=1
      ipunit=iutw
      do 77 i=1,mxdurg
         if (i.gt.1.and.i.eq.mxdurg) then
            call ucncat (strng,' or',ierr)
            ibeg=ibeg+3
            endif
         integr=idurt(i)
         nchar=1
         if (integr.gt.9) nchar=2
         call ufi2a (integr,strng,ibeg,nchar,iprerr,ipunit,ierr)
         ibeg=ibeg+nchar
         if (i.lt.mxdurg-1) then
            call ucncat (strng,',',ierr)
            ibeg=ibeg+1
            endif
         ibeg=ibeg+1
77       continue
c
80    write (iutw,90) strng(1:lenstr(strng))
90    format (' Enter duration (',a,') hours): ',$)
      read (iutr,'(i4)',err=80) idur
      if (idur.eq.0) go to 350
c
c  check for valid duration
      do 110 i=1,mxdurg
         if (idur.eq.idurt(i)) then
            ndur = i
            go to 130
            endif
110      continue
      write (iutw,120) idur
120   format (' ERROR: ',i2,' is an invalid duration.')
      go to 80
c
130   if (geog.eq.'a') then
         call dspgrd (ndur,inrow,iwcol,kdrow,kdcol,mrow,mcol,
     +                mxd,mxr,mxc,tro)
         else
            call dspbb (ndur,inrow,iwcol,kdrow,kdcol,nlseg,nlrow,nlbeg,
     +                  nlend,mdur,mrow,mcol,mxd,mxr,mxc,tro)
         endif
c
135   if (geog.eq.'a') then
         write (iutw,140) idur
140   format (' Enter (l-left  r-right  u-up  d-down  ',
     +                   'a-all bins  b-boundary' /
     +        '        t-time [',i2,' hr]  D-shift display  f-fill  ',
     +                   'c-change  [m]-menu): ',$)
         else
            write (iutw,150) bbid,idur
150   format (' Enter (l-left  r-right  u-up  d-down  ',
     +                   'a-all bins  b-boundary [',2a4,']' /
     +        '        t-time [',i2,' hr]  D-shift Display  f-fill  ',
     +                   'c-change  [m]-menu): ',$)
         endif
c
      read (iutr,30,err=130) resp
      if (resp.eq.' ') go to 350
c
c  move left
      if (resp.eq.'l'.or.resp.eq.'L') then
         iwcol = iwcol - kdcol
         write (iutw,155) 'left',kdcol,'columns'
155   format (' NOTE: display will be moved ',a,' ',i2,' ',a,'.')
c
c  move right
      else if (resp.eq.'r'.or.resp.eq.'R') then
         iwcol = iwcol + kdcol
         write (iutw,155) 'right',kdcol,'columns'
c
c  move up
      else if (resp.eq.'u'.or.resp.eq.'U') then
         inrow = inrow + kdrow
         write (iutw,155) 'up',kdrow,'rows'
c
c  move down
      else if (resp.eq.'d') then
         inrow = inrow - kdrow
         write (iutw,155) 'down',kdrow,'rows'
c
c  set to display all bins
      else if (resp.eq.'a') then
         geog = 'a'
c
c  set to display by boundary id
      else if (resp.eq.'b') then
         geog = 'b'
         write (iutw,160)
160   format (' Enter Boundary identifier: ',$)
         read (iutr,170) bbid
170   format (2a4)
         call getbb (bbid,bbtyp,mbx,bx,ibx,mlseg,nlseg,
     +               nlrow,nlbeg,nlend,istat)
         if (istat.gt.0) then
            write (iutw,180)
180   format (' <return>-continue:',$)
            read (iutr,170) resp
            go to 130
            endif
c     determine northern most row
         inrow = nlrow(nlseg) - msrow + kdrow - 1
c     determine west column
         iwcol = 9999
         do 190 i=1,nlseg
            if (nlbeg(i).lt.iwcol) iwcol = nlbeg(i)
190         continue
         iwcol = iwcol - mwcol
c
c  change grid value
      else if (resp.eq.'c'.or.resp.eq.'C') then
         do 200 i=1,mval
            ival(i) = -1
200         continue
210      write (iutw,220)
220   format (' Enter (row,column,values) ',
     +   '[''m'' to set value to missing] or ',
     +   '<return>: ',$)
         read (iutr,'(a)',err=210) line
         if (line.eq.' ') go to 280
         dlim=' '
         nscan=1
         call uscan2 (line,dlim,nscan,word,lword,istat)
         ibeg=1
         iprerr=1
         call ufa2i (word,ibeg,lword,inr,iprerr,iutw,istat)
         nscan=nscan+1
         call uscan2 (line,dlim,nscan,word,lword,istat)
         call ufa2i (word,ibeg,lword,inc,iprerr,iutw,istat)
         nval=0
         do 240 i=1,mval
            nscan=nscan+1
            call uscan2 (line,dlim,nscan,word,lword,istat)
            if (word.eq.' ') go to 243
            if (word.eq.'m') then
               ival(i)=-990
               else
                  call ufa2i (word,ibeg,lword,ival(i),iprerr,iutw,istat)
               endif
            nval=nval+1
240         continue
243      irow = inr - msrow + 1
         icol = inc - mwcol + 1
         inderr=0
         if (irow.lt.1.or.irow.gt.mrow) then
            write (iutw,244) 'row',inr,msrow,msrow+mrow-1
244   format (' ERROR: ',a,' number (',i4,') is less than ',i4,
     +   ' or greater than ',i4,'.')
            inderr=1
            endif
         if (icol.lt.1.or.icol.gt.mcol) then
            write (iutw,244) 'column',inc,mwcol,mwcol+mcol-1
            inderr=1
            endif
         if (inderr.eq.1) go to 210
c     replace values
         do 270 i=1,nval
            if (ival(i).eq.-1) go to 210
            misval=-990
            if (ival(i).eq.misval) go to 260
            minval=0
            maxval=1000
            if (ival(i).ge.minval.and.ival(i).le.maxval) go to 260
            if (ival(i).lt.minval) then
               write (iutw,245) i,ival(i),'less',minval
245   format (' WARNING: data value ',i2,' (',i4,
     +   ') will not be processed because it is ',a,' than ',i2,'.')
               go to 270
               endif
            if (ival(i).gt.maxval) then
               write (iutw,245) i,ival(i),'greater',maxval
               go to 270
               endif
            write (iutw,250) i,ival(i)
250   format (' WARNING: data value ',i2,' (',i4,
     +   ') will not be processed because it invalid.')
            go to 270
260         j = icol + i - 1
            tro(ndur,irow,j) = 0.01*float(ival(i))
            ichnge = 1
            if (ibug.eq.1) write (iud,*) ' i=',i,' ival(i)=',ival(i),
     +         ' ndur=',ndur,' irow=',irow,' j=',j,
     +         ' tro(ndur,irow,j)=',tro(ndur,irow,j)
270         continue
         go to 210
280      kgridf = 2
c
c  shift display
      else if (resp.eq.'D') then
         write (iutw,300)
300   format (' Enter (northern_row,western_column): ',$)
         read (iutr,'(a)',err=50) line
         if (line.ne.' ') then
            dlim=' '
            nscan=1
            call uscan2 (line,dlim,nscan,word,lword,istat)
            ibeg=1
            iprerr=1
            call ufa2i (word,ibeg,lword,inr,iprerr,iutw,istat)
            nscan=nscan+1
            call uscan2 (line,dlim,nscan,word,lword,istat)
            call ufa2i (word,ibeg,lword,inc,iprerr,iutw,istat)
            inrow = inr - msrow + 1
            iwcol = inc - mwcol + 1
            endif
c
c  time of duration
      else if (resp.eq.'t'.or.resp.eq.'T') then
         go to 80
c
c  return to menu
      else if (resp.eq.'m'.or.resp.eq.'M') then
         go to 350
c
c  fill-in missing runoffs
      else if (resp.eq.'f') then
         write (iutw,310)
310   format (' NOTE: filling in missing runoffs.')
         lcol = mcol - 1
         lrow = mrow - 1
c     process each row
         do 340 irow=2,lrow
c        check column using row above and below target column
            do 320 icol=2,lcol
               if (tro(ndur,irow,icol).lt.0.0) then
                  a = tro(ndur,irow+1,icol)
                  b = tro(ndur,irow-1,icol)
                  if (a.gt.0..and.b.gt.0.0) then
                     tro(ndur,irow,icol) = amin1(a,b)
                     else if (b.gt.0.0) then
                        tro(ndur,irow,icol) = b
                        else if (a.gt.0.0) then
                           tro(ndur,irow,icol) = a
                     endif
                  endif
320            continue
c        check column using values left and right of target
            do 330 icol=2,lcol
               if (tro(ndur,irow,icol).lt.0.0) then
                  a = tro(ndur,irow,icol-1)
                  b = tro(ndur,irow,icol+1)
                  if (a.gt.0..and.b.gt.0.0) then
                     tro(ndur,irow,icol) = amin1(a,b)
                     else if (a.gt.0.0) then
                        tro(ndur,irow,icol) = a
                        else if (b.gt.0.0) then
                           tro(ndur,irow,icol) = b
                     endif
                  endif
330            continue
340        continue
        kgridf = 2
c
      else
         write (iutw,40) resp(1:lenstr(resp))
         go to 135
      endif
c
      go to 130
c
350   return
c
      end
