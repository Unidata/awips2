c  =====================================================================
c  pgm:  dspbb (ndur,inrow,iwcol,kdrow,kdcol,nlseg,nlrow,
c               nlbeg,nlend,mdur,mrow,mcol,mxd,mxr,mxc,gar)
c
c   in: ndur   .... duration number 
c   in: inrow  .... north row of HRAP window
c   in: iwcol  .... west column of HRAP window
c   in: kdrow  .... number of HRAP rows in display window
c   in: kdcol  .... number of HRAP columns in display window
c   in: nlseg  .... number of line segments in boundary
c   in: nlrow  .... array of line segment row numbers
c   in: nlbeg  .... array of line segment left column numbers
c   in: nlend  .... array of line segment right column numbers
c   in: mdur   .... maximum number of durations
c   in: mrow   .... maximum number of grid rows
c   in: mcol   .... maximum number of grid columns
c   in: gar    .... array containing gridded values 
c  =====================================================================
      subroutine dspbb (ndur,inrow,iwcol,kdrow,kdcol,nlseg,nlrow,
     +                  nlbeg,nlend,mdur,mrow,mcol,mxd,mxr,mxc,gar)
c.......................................................................
c  This subroutine displays gridded integer values in hundredths of an
c  inch as for a single boundary.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                      Feb 1998
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/uinfo'
      dimension nlrow(*),nlbeg(*),nlend(*)
      dimension knum(500),igr(500),gar(mxd,mxr,mxc)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/dspbb.f,v $
     . $',                                                             '
     .$Id: dspbb.f,v 1.1 2001/08/16 17:42:39 dws Exp $
     . $' /
C    ===================================================================
C
c
c  generate column labels
      do 1010 i=1,mcol
 1010 knum(i) = i + mwcol - 1
c
c  check boundaries of display and reset if necessary
      if(inrow.gt.mrow) inrow = mrow
      if(inrow.lt.kdrow) inrow = kdrow
c
      jb = inrow
      je = jb - kdrow + 1
      if(iwcol.le.0) iwcol = 1
      kb = iwcol
      ke = kb + kdcol - 1
c
      if(ke.gt.mcol) then
         i = ke - mcol
         ke = mcol
         kb = kb - i
         if(kb.lt.1) kb =1
      endif
c
c 
      do 1200 jrow=jb,je,-1
c  initialize row
      do 1130 jcol=kb,ke
 1130 igr(jcol) = -1
c
c  find row (line segment) in boundary
      irow = jrow + msrow - 1
      do 1170 ils=1,nlseg
      if(nlrow(ils).ne.irow) goto 1170
c
c  found a row
c  now find columns in boundary
      do 1150 icol=nlbeg(ils),nlend(ils)
      jcol = icol - mwcol + 1
      if(jcol.lt.kb.or.jcol.gt.ke) goto 1150
c  found a column
c  transfer to integer array for display
      g = gar(ndur,jrow,jcol)
      if(g.gt.0.0) then
         g = g + 0.005
      else if(g.lt.0.0) then
         g = g - 0.005
      endif
      igr(jcol) = ifix(g*100.)
 1150 continue
c
 1170 continue
c
c  output a row of values
      write(iutw,60) irow,(igr(k),k=kb,ke)
   60 format(20i4)
c
 1200 continue
c
c  show column numbers
      write(iutw,80) (knum(k),k=kb,ke)
   80 format(' R/C',30i4)
      return
      end
