c  =====================================================================
c  pgm:  adelim (inar,ni,lw,work)
c
c   in: inar   .... input array of ni words
c   in: ni     .... number of words in inar
c  out: lw     .... length of variable work
c  out: work   .... output variable 
c  =====================================================================
      subroutine adelim (inar,ni,lw,work)
c.......................................................................
c  routine inserts delimiters around variable work if needed.  
c  Delimiters needed if embedded space found.  
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                Feb 1997
c.......................................................................
c
      character*1 aps,dlim
      character*4 inar(ni)
      character*(*) work
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/adelim.f,v $
     . $',                                                             '
     .$Id: adelim.f,v 1.1 2001/08/16 17:42:38 dws Exp $
     . $' /
C    ===================================================================
C
c
      data aps /1h'/
c
      lw = ni*4
c
c  clear last two postions in work
      do 1000 i=1,2
      j = lw + i
 1000 work(j:j) = ' '
      
c
c  transfer to work variable
      lspc = 0
      iesp = 0
      ieaps = 0
      ieslnt = 0
      k = 0
      do 1010 n=1,ni
      do 1010 i=1,4
      j = (n-1)*4 + i
      work(j:j) = inar(n)(i:i)
c
c  check for space:  leading, embedded
      if(work(j:j).eq.' ') then
        if(k.eq.0) then
c  leading space
          lspc = lspc + 1
          goto 1010
        else
c  embedded spaces
          iesp = iesp + 1
        endif
c
c  check for embedded apostrophe
      else if(work(j:j).eq.aps) then
        ieaps = ieaps + 1
c
c  check for embedded slant
      else if(work(j:j).eq.'/') then
        ieslnt = ieslnt + 1
      else
        if(k.eq.0) then
          ib = j
        else
          ie = j
        endif
      endif
      k = k + 1
 1010 continue
c
      iesp = ie + iesp - lw
c
c  select delimiter
      if(iesp.gt.0) then
        ie = ie + 1
        if(ieaps.eq.0) then
          dlim = aps
          work(ie:ie) = aps
        else if(ieslnt.eq.0) then
          dlim = '/'
          work(ie:ie) = '/'
        else
          dlim = '['
          work(ie:ie) = ']'
        endif
c
c  right shift one position to insert first delimiter
        do 1050 i=ie,ib,-1
        j = i + 1
 1050   work(j:j) = work(i:i)
c
c  insert first delimiter
        work(ib:ib) = dlim
        ie = ie + 1
        if(ie.gt.lw) lw = lw + 2
      endif
c      
      return
      end
