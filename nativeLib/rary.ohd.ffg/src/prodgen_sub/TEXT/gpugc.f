c  =====================================================================
c  pgm:  gpugc (loc)
c
c   in: loc    .... starting location in atext array of one line of UGC
c
c  =====================================================================
c
      subroutine gpugc (loc)
c
c.......................................................................
c
c   Append expiration date and time to last line of UGC info
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL - Sept 1994
c.......................................................................
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/timez'
      include 'prodgen_inc/txtpar'
c
      character ctxt(72)
      character*4 wa
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpugc.f,v $
     . $',                                                             '
     .$Id: gpugc.f,v 1.2 2003/03/14 18:37:04 dws Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('gpugc',1,1,ibug)
c
c
      lst = 0
      m = 0
c
c  transfer 4-character words to single character words
      do 40 j=1,16
         k = loc + j - 1
         wa = atext(k)
         if (ibug.gt.0) write (iud,10) loc,j,wa
10    format (' loc=',i4,' j=',i2,' wa=',a4)
         do 30 i=1,4
            m = m + 1
            ctxt(m) = wa(i:i)
            if (ctxt(m).ne.' ') lst = m
            if (ibug.gt.0) write(iud,20) m,wa,ctxt(m)
20    format (' m=',i3,' wa=',a4,'ctxt(m)=',a)
30          continue
40       continue
c
c  check that last non-blank character is a -
      if (ctxt(lst).ne.'-') then
c     insert -
         lst = lst + 1
         ctxt(lst) = '-'
         endif
c
c  increment day
      nzda = kzda + 1
      if (nzda.gt.lastda) nzda = 1
c
c  convert nzda to individual digits
      m = lst + 1
      ipos = 1
      num = 1
      mwid = 2
      call ffi2a (ctxt(m),ipos,mwid,num,nzda)
      if (ctxt(m).eq.' ') ctxt(m) = '0'
c
      j = kzhr/6
      jhr = j*6
      jmin = 0
c
c  convert hr and minutes to individual characters
      jhrmn = jhr*100 + jmin
      mwid = 4
      m = m + 2
      call ffi2a (ctxt(m),ipos,mwid,num,jhrmn)
      if (ctxt(m).eq.' ') ctxt(m) = '0'
c
c  append '-' after expiration time
      lst = m + 4
      ctxt(lst) = '-'
c
      write (iuf,50) (ctxt(i),i=1,lst)
50    format (72a1)
c
      return
c
      end

