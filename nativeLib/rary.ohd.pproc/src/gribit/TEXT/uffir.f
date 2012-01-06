c  =====================================================================
c  pgm:  uffir (chin,istart,iwidth,inum,rnum,istnxt,itype)
c
c   in: chin   .... character input array
c   in: istart .... beginning position in chin
c   in: iwidth .... number of characters in output array
c  out: inum   .... integer number
c  out: rnum   .... real number
c  out: istnxt .... starting position for next field
c  out: itype  .... type indicator:
c                     -1 = real number
c                      0 = integer number
c                      1 = not a number
c                      2 = null field
c  =====================================================================
c
      subroutine uffir (chin,istart,iwidth,inum,rnum,istnxt,itype)
c
c.......................................................................
c
c  Decode and convert field of width iwidth starting at position
c  istart in character input array chin to integer number inum or to
c  real number rnum if decimal is present.
c
c.......................................................................
c  initially written by
c      Tim Sweeney, HRL                                March 1996
c.......................................................................
c
      character chin*(*)
      character cnum*10
c
      include 'ffg_inc/iuws'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/uffir.f,v $
     . $',                                                             '
     .$Id: uffir.f,v 1.1 2006/05/03 13:43:59 gsood Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('uffir',1,1,ibug)
c
      istnxto=istnxt
c
c  get next field
      cnum = ' '
      call uffch (chin,istart,iwidth,cnum,istnxt,istat)
      if (istat.gt.0) then
         if (itype.eq.1) write (iutw,20) istnxt,chin(1:lenstr(chin))
20    format (' ERROR: in uffir - error encountered getting field ',i2,
     +   ' from the following record:' / ' ',a)
         go to 40
         endif
c
c  convert from character to integer and real
      call uc2ir (cnum,inum,rnum,itype,istat)
      if (istat.gt.0) then
         call ubegin (chin(istart:istart),len(chin),lbegin)
         write (iutw,30) lbegin,chin(1:lenstr(chin))
30    format (' ERROR: in uffir - converting field starting at column ',
     +   i2,' on the following record to a number:' / ' ',a)
         endif
c
40    return
c
      end
