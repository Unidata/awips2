c  =====================================================================
c  pgm:  edvcag (is,lin,lout,mx,car,nperg,ngrps)
c
c   in: is     .... function
c                     1 - add
c                     2 - change
c                     3 - list
c   in: lin    .... input device
c   in: lout   .... output device
c  i/o: mx     .... number of words in car
c  i/o: car    .... array of character words by group
c   in: nperg  .... number of words per group (i.e., two for
c                   8-character identifiers
c  i/o: ngrps  .... number of groups
c  =====================================================================
c
      subroutine edvcag (is,lin,lout,mx,car,nperg,ngrps)
c
c.......................................................................
c
c  Routine for editing multiple lines of characters.
c  Routine used to edit an array of character words.  The array is
c  displayed and edited by groups of nperg words. Calls routine
c  edvca.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Mar 1991
c.......................................................................
c
      character chno,resp
      character*4 car(mx)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvcag.f,v $
     . $',                                                             '
     .$Id: edvcag.f,v 1.2 2004/01/30 17:47:39 scv Exp $
     . $' /
C    ===================================================================
C
c
      kf = is
c
      if (ngrps.eq.0) then
         nxt = 1
         lno = 1
         go to 140
         endif
c
c  list defined values
10    do 20 i=1,ngrps
         j = (i-1)*nperg + 1
         write (lout,30) i,(car(j+k-1),k=1,nperg)
30    format (3x,i2,') ',18a4)
20       continue
c
      write (lout,40)
40    format (' Select (a-add c-change d-delete <return>-exit): ',$)
      read (lin,50,err=10) resp
50    format (a1)
      if (resp.eq.' ') go to 180
c
c  select line
60    write (lout,70)
70    format (' Enter line number (0-exit): ',$)
      read (lin,80,err=90) lno,chno
80    format (i3,t1,a1)
      nxt = ngrps + 1
      if (lno.le.0) go to 10
      if (lno.gt.nxt) go to 60
      go to 100
c
90    if (chno.eq.'X'.or.chno.eq.'x') go to 180
      go to 60
c
100   if (resp.eq.'A'.or.resp.eq.'a') then
c     add line
         kf = 1
c     move entries down
         do 120 i=ngrps,lno,-1
            j = (i-1)*nperg + 1
            m = i*nperg + 1
            do 110 k=1,nperg
               car(m+k-1) = car(j+k-1)
110            continue
120         continue
         ngrps = nxt
         go to 140
         endif
c
      if (resp.eq.'C'.or.resp.eq.'c') then
c     change line
         kf = 2
         go to 140
         endif
      go to 150
c
140   j = (lno-1)*nperg + 1
      call edvca (kf,lin,lout,nperg,car(j))
      kf = 3
      go to 10
c
150   if (resp.eq.'D'.and.resp.eq.'d') then
c     delete line
         ib = lno + 1
c     move entries up
         do 170 i=ib,ngrps
            j = (i-1)*nperg + 1
            m = (i-2)*nperg + 1
            do 160 k=1,nperg
               car(m+k-1) = car(j+k-1)
160            continue
170         continue
         ngrps = ngrps - 1
         kf = 3
         go to 10
         endif
c
180   return
c
      end
