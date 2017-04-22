c  =====================================================================
c  pgm:  dspgrd (ndur,inrow,iwcol,kdrow,kdcol,mrow,mcol,
c                mxd,mxr,mxc,gar)
c
c   in: ndur   .... duration number
c   in: inrow  .... north row of HRAP window
c   in: iwcol  .... west column of HRAP window
c   in: kdrow  .... number of HRAP rows in display window
c   in: kdcol  .... number of HRAP columns in display window
c   in: mrow   .... maximum number of grid rows
c   in: mcol   .... maximum number of grid columns
c   in: gar    .... array containing gridded values
c  =====================================================================
c
      subroutine dspgrd (ndur,inrow,iwcol,kdrow,kdcol,mrow,mcol,
     +                   mxd,mxr,mxc,gar)
c
c.......................................................................
c
c  This subroutine displays gridded data in hundredths of an inch.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Oct 25, 1993

c  Modified to display national HRAP row and column numbers
c       Tim Sweeney, HRL                                      Nov 1995
c.......................................................................
c
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iuws'
      include 'ffg_inc/uinfo'
c
      character*10 strng
      character*50 fmt
c
      dimension gar(mxd,mxr,mxc)
      parameter (micnum=500)
      dimension icnum(micnum)
      dimension igr(500)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/dspgrd.f,v $
     . $',                                                             '
     .$Id: dspgrd.f,v 1.2 2002/10/10 16:57:34 dws Exp $
     . $' /
C    ===================================================================
C
c
      ibug = 0
c
      if (ibug.eq.1) write (iud,*) 'ndur=',ndur,' inrow=',inrow,
     +   ' iwcol=',iwcol,' kdrow=',kdrow,' kdcol=',kdcol,
     +   ' mrow=',mrow,' mcol=',mcol
c
      istat = 0
c
      iwcolo = 0
c
      mnrow=msrow+nrow-1
      mecol=mwcol+ncol-1
      write (iutw,11) msrow,mnrow,mwcol,mecol
11    format (' NOTE: FFG HRAP subset is rows ',i4,'-',i4,' and ',
     +   'columns ',i4,'-',i4,'.')
c
c  check top row
      if (inrow.gt.mrow) then
         strng='up'
         lstrng=lenstr(strng)
         write (iutw,13) strng(1:lstrng),kdrow,'top',
     +      msrow+inrow-1,'northern',mnrow
13    format (' WARNING: cannot move ',a,' ',i4,
     +   ' rows because new ',a,' row (',i4,
     +   ') would be beyond FFG HRAP ',a,' row (',i4,').')
         ndiff = iabs(inrow - kdrow - mrow)
         inrow = mrow
         if (ndiff.eq.0) go to 80
         write (iutw,17) strng(1:lstrng),ndiff,'rows'
17    format (' NOTE: display will be moved ',a,' ',i2,' ',a,'.')
         endif
c
c  check bottom row
      if (inrow.lt.kdrow) then
         strng='down'
         lstrng=lenstr(strng)
         write (iutw,13) strng(1:lstrng),kdrow,'bottom',
     +      msrow-kdrow,'southern',msrow
         ndiff = inrow
         if (ndiff.eq.0) then
            inrow = kdrow
            go to 80
            endif
         inrow = inrow +kdrow - ndiff
         write (iutw,17) strng(1:lstrng),ndiff,'rows'
         endif
c
      kb = iwcol
c
c  check left column
      if (iwcol.le.0) then
         strng='left'
         lstrng=lenstr(strng)
         write (iutw,15) strng(1:lstrng),kdcol,strng(1:lstrng),
     +      mwcol-kdcol,'western',mwcol
15    format (' WARNING: cannot move ',a,' ',i4,
     +   ' columns because new ',a,' column (',i4,
     +   ') would be beyond FFG HRAP ',a,' column (',i4,').')
         idiff = kb + kdcol - 1
         ndiff = idiff
         if (ndiff.eq.0) then
            iwcol = 1
            go to 80
            endif
         if (ndiff.gt.0) then
            iwcol = iwcol - kdcol + ndiff
            write (iutw,17) strng(1:lstrng),kdcol-idiff,'columns'
            endif
         endif
c
      kb = iwcol
      ke = kb + kdcol - 1
c
c  check right column
      if (ke.gt.mcol) then
         strng='right'
         lstrng=lenstr(strng)
         write (iutw,15) strng(1:lstrng),kdcol,strng(1:lstrng),
     +      mwcol+mcol,'eastern',mecol
         idiff = ke - mcol
         ke = mcol
         kb = kb - idiff
         if (kb.lt.1) kb = 1
         ndiff = kdcol - idiff + 1
         if (ndiff.le.0) then
            iwcol = iwcol - kdcol
            go to 80
            endif
         if (ndiff.gt.0) then
            iwcolo = iwcol
            iwcol = iwcol - ndiff
            write (iutw,17) strng(1:lstrng),kdcol-idiff,'columns'
            endif
         endif
c
c  generate column labels
      do 10 i=1,mcol
         icnum(i) = i + mwcol - 1
10       continue
c
c  print header
      write (iutw,*) 'Gridded data in hundredths of an inch:'
      write (iutw,20) (icnum(i),i=kb,ke)
20    format (' Col ',30i4)
      ncols=ke-kb+1
      fmt='('' Row '',   (''---+''))'
      ibeg=10
      nchar=3
      iprerr=1
      call ufi2a (ncols,fmt,ibeg,nchar,iprerr,iutw,istat)
      write (iutw,fmt)
c
c  print data
      jb = inrow
      je = jb - kdrow + 1
      do 70 jrow=jb,je,-1
         do 50 k=kb,ke
            g = gar(ndur,jrow,k)
            if (g.lt.0.0) g = g - 0.005
            igr(k) = ifix(g*100.0)
50          continue
         jr = jrow + msrow - 1
         write (iutw,60) jr,(igr(k),k=kb,ke)
60    format (' ',i4,30i4)
70       continue
c
      write (iutw,fmt)
      write (iutw,20) (icnum(i),i=kb,ke)
c
80    if (iwcolo.gt.0) iwcol = iwcolo
      if (ibug.eq.1) write (iud,*) 'inrow=',inrow,' iwcol=',iwcol
c
      return
c
      end
