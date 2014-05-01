c  =====================================================================
c  pgm:  listid (nids,pdx,ipcol)
c
c   in: nids   .... number of ids in pdx array
c   in: pdx    .... array of index ids
c   in: ipcol  .... number of ids per column
c  =====================================================================
c
      subroutine listid (nids,pdx,ipcol)
c
c.......................................................................
c
c This routine lists identifiers.
c
c.......................................................................
c Initially written by
c       Tim Sweeney, HRL                                   Feb 1997
c.......................................................................
c
      character*4 pdx(2,nids)
c
      include 'ffg_inc/iuws'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/listid.f,v $
     . $',                                                             '
     .$Id: listid.f,v 1.2 2003/08/20 13:26:34 scv Exp $
     . $' /
C    ===================================================================
C
c
      if (nids.le.ipcol) then
         do 20 i=1,nids
            if (pdx(1,i).eq.'none') go to 20
            write (iutw,10) i,(pdx(j,i),j=1,2)
10    format (15x,3x,i2,' - ',2a4)
20          continue
c
      else if (nids.le.2*ipcol) then
         mf = nids/2
         ma = mod(nids,2)
         ms = mf + ma
         do 40 i=1,mf
            j = i + ms
            write (iutw,30) i,(pdx(n,i),n=1,2),
     +                      j,(pdx(n,j),n=1,2)
30    format (15x,3x,2(6x,i2,' - ',2a4))
40          continue
         if (ma.eq.1) then
            write (iutw,30) ms,(pdx(n,ms),n=1,2)
            endif
c
      else if (nids.le.54) then
         mf = nids/3
         mr = mod(nids,3)
         ma = mr
         if (mr.ne.0) ma = 1
         ms = mf + ma
         do 50 i=1,mf
            j = i + ms
            k = j + ms
            write (iutw,60) i,(pdx(n,i),n=1,2),
     +                      j,(pdx(n,j),n=1,2),
     +                      k,(pdx(n,k),n=1,2)
50          continue
60    format (15x,3x,3(6x,i2,' - ',2a4))
         if (mr.ne.0) then
            write (iutw,70) (i,(pdx(n,i),n=1,2),i=ms,nids,ms)
70    format (15x,3x,2(6x,i2,' - ',2a4))
            endif
c
      else
         mf = nids/4
         mr = mod(nids,4)
         ma = mr
         if (mr.ne.0) ma = 1
         ms = mf + ma
         do 90 i=1,mf
            j = i + ms
            k = j + ms
            l = k + ms
            write (iutw,80) i,(pdx(n,i),n=1,2),
     +                      j,(pdx(n,j),n=1,2),
     +                      k,(pdx(n,k),n=1,2),
     +                      l,(pdx(n,l),n=1,2)
80    format (15x,3x,4(5x,i2,' - ',2a4))
90          continue
         if (mr.ne.0) then
            write (iutw,100) (i,(pdx(n,i),n=1,2),i=ms,nids,ms)
100   format (15x,3x,3(5x,i2,' - ',2a4))
            endif
c
      endif
c
      return
c
      end
