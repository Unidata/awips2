c  =====================================================================
c  pgm:  uc2ir (cnum,inum,rnum,itype,istat)
c
c   in: cnum   .... character string
c  out: inum   .... integer number
c  out: rnum   .... real number
c  out: itype  .... type indicator:
c                     -1 = real number
c                      0 = integer number
c                      1 = not a number
c                      2 = null field
c  out: istat  .... status code
c  =====================================================================
c
      subroutine uc2ir (cnum,inum,rnum,itype,istat)
c
c.......................................................................
c
c  routine converts character number cnum to integer number num or to
c  real number rnum depending on presence of decimal point in cnum
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - Dec 1992
c
c  added real number feature
c       Tim Sweeney, HRL  - Jul 1995
c
c  Changed decimal places algorithm.  Added check for trailing blank
c  spaces.
c       Tim Sweeney, HRL   -  Mar 1996
c
c  Changed to ff routines
c       Tim Sweeney, HRL                                   Nov 1998
c.......................................................................
c
      include 'ffg_inc/gdebug'
c
      character cnum*(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/uc2ir.f,v $
     . $',                                                             '
     .$Id: uc2ir.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('uc2ir',1,4,ibug)
c
      istat = 0
c
      inum = 0
      rnum = 0.0
      itype = 1
c
      nchar = lenstr(cnum)
      if (ibug.gt.0) write (iud,*) 'in uc2ir - cnum=',cnum,
     +   ' nchar=',nchar
      if (nchar.le.0) then
         itype = 2
         go to 40
         endif
c
      ipos = 1
      nval = 1
c
c  check for decimal point
      do 20 idec=1,nchar
         if (cnum(idec:idec).eq.'.') go to 30
20       continue
c
c  convert to integer
      call ffa2i (cnum,ipos,nchar,nval,inum,istat2)
      if (istat2.eq.0) then
         itype = 0
         else
            istat = 1
         endif
      if (ibug.gt.0) write (iud,*) 'in uc2ir - cnum=',cnum,
     +   ' inum=',inum,' itype=',itype
      rnum = inum
      go to 40
c
c  convert to real
30    ndec = nchar - idec
      call ffa2f (cnum,ipos,nchar,ndec,nval,rnum,istat2)
      if (istat2.eq.0) then
         itype = -1
         else
            istat = 1
         endif
      if (ibug.gt.0) write (iud,*) 'in uc2ir - cnum=',cnum,
     +   ' ndec=',ndec,' rnum=',rnum,' itype=',itype,' (-1 real)'
c
40    return
c
      end
