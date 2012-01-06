c  =====================================================================
c  pgm:  dispid (resp,ib,nper,nwds,ncidx,cidx)
c
c  out: resp   .... function:
c                     a = add
c                     c = change
c                     d = delete
c                     l = list
c  i/o: ib     .... array position of first identifier to be displayed
c   in: nper   .... number of identifiers per display
c   in: nwds   .... number of words per identifier
c   in: ncidx    .... number of identifiers in index array
c   in: cidx   .... index array
c  =====================================================================
c
      subroutine dispid (resp,ib,nper,nwds,ncidx,cidx)
c
c.......................................................................
c
c  Routine to display identifiers in an index array.
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character resp
      character*4 cidx(nwds,ncidx)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/dispid.f,v $
     . $',                                                             '
     .$Id: dispid.f,v 1.4 2004/01/30 17:46:36 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dispid',1,1,ibug)
      ibug = 0
c
      if (ibug.eq.1) write (iud,*) 'in dispid - nper=',nper,
     +   ' ncidx=',ncidx
c
      if (nwds.eq.-1) go to 40
c
      if (nwds.eq.0) nwds = 2
ccc      if (nper.le.0) nper = 99
      if (nper.le.0) nper = ncidx
      iadd = nper + 1
c
      if (nper.lt.ncidx) write (iutw,5) nper+1
5     format (' NOTE: identifiers will be listed in groups of ',i3,'.'
     +   /)
c
c  set starting and ending positions
10    if (ib.gt.ncidx) ib = ib - nper - 1
      if (ib.le.0) ib = 1
      ie = ib + nper
      if (ie.gt.ncidx) ie = ncidx
c
c  check number of words per group
      if (nwds.eq.2) then
         write (iutw,20) (i,(cidx(j,i),j=1,nwds),i=ib,ie)
20    format (5(1x,i4,') ',2a4))
         endif
      if (nwds.eq.3) then
         write (iutw,30) (i,(cidx(j,i),j=1,nwds),i=ib,ie)
30    format (4(2x,i3,') ',2a4,1x,a4))
         endif
c
      nmore = ncidx - ib - iadd + 1
      nprev = ib - 1
      if (ibug.eq.1) write (iud,*) 'in dispid - ncidx=',ncidx,
     +   ' nper=',nper,' ib=',ib,' ie=',ie,
     +   ' iadd=',iadd,' nmore=',nmore,' nprev=',nprev
c
      if (nmore.gt.0) write (iutw,35) nmore,'more'
35    format (/ ' NOTE: ',i3,' ',a,' identifiers.')
      if (nprev.gt.0) write (iutw,35) nprev,'previous'
c
40    if (ncidx.le.nper) write (iutw,50)
50    format (/ ' Select (a-add c-change d-delete l-list ',
     +   '<return>-exit): ',$)
      if (ncidx.gt.nper.and.ib.eq.1)  write (iutw,60)
60    format (/ ' Select (a-add c-change d-delete l-list ',
     +   'm-more <return>-exit): ',$)
      if (ncidx.gt.nper.and.ie.eq.ncidx) write (iutw,70)
70    format (/ ' Select (a-add c-change d-delete l-list ',
     +   'p-previous <return>-exit)): ',$)
      if (ib.gt.1.and.ie.lt.ncidx) write (iutw,80)
80    format (/ ' Select (a-add c-change d-delete l-list ',
     +   'm-more p-previous <return>-exit): ',$)
c
      read (iutr,'(a1)') resp
c
      if (resp.eq.'M'.or.resp.eq.'m'.or.
     +    resp.eq.'N'.or.resp.eq.'n') then
c     print next group of identifiers
         if (ib+iadd.gt.ncidx) then
            write (iutw,85)
85    format (/ ' WARNING: no more identifiers to be displayed.')
            go to 40
            endif
         ib = ib + iadd
         go to 10
         endif
c
      if (resp.eq.'P'.or.resp.eq.'p') then
c     print previous group of identifiers
         isub = nper + 1
         if (ib-iadd.lt.0) then
            write (iutw,85)
            go to 40
            endif
         ib = ib - isub
         go to 10
         endif
c
      return
c
      end
