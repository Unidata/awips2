c  =====================================================================
c  pgm:  uffch (chin,iptr,iwidth,chout,nxt,istat)
c
c   in: chin   .... character input array
c   in: iptr   .... beginning position in chin
c   in: iwidth .... number of characters in output array
c  out: chout  .... character output array
c  out: nxt    .... starting position for next field
c  out: istat  .... completion status:
c                     0 = valid field
c                     1 = no field
c                     2 = null field
c  =====================================================================
c
      subroutine uffch (chin,iptr,iwidth,chout,nxt,istat)
c
c......................................................................
c.
c  Decode and move free format character field starting at position
c  iptr in character input array chin to character output array chout
c  of width iwidth.  Starting position of next field is nxt.
c
c  delimiters:
c     fields with no embedded spaces (sp) and with
c        or without slash / or  apostrophe ':  spaces, comma, or colon
c     fields with embedded spaces:   ' or : or / or []
c
c                          delimiter   allowed characters
c     no embedded spaces     sp , :    /  '
c
c     with embedded spaces:   ' apos   / , : [] sp
c                             /        '
c                             , comma  / ' : [] sp
c                             :        / ' [] sp
c                             []       / ' , : sp
c.......................................................................
c  initially written by
c      Tim Sweeney, HRL                                March 1996
c
c  added comma and colon delimiter
c      Tim Sweeney, HRL                                May 2000

c.......................................................................
c
      include 'ffg_inc/iuws'
c
      character*1 iap/''''/
      character chin*(*),chout*(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/uffch.f,v $
     . $',                                                             '
     .$Id: uffch.f,v 1.5 2004/01/06 12:39:50 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('uffch',1,4,ibug)
ccc      ibug = 1
c
      if (ibug.eq.1) write (iutw,*) 'enter uffch'
c
c  initial values
      i = iptr
      k = 0
      is = 0
      iapos = 0
      islnt = 0
      icomma = 0
      icolon = 0
      ibrac = 0
      ident = 0
      istat = 0
      non = 10
c
10    if (i.gt.60) non = iwidth
c
      if (chin(i:i).eq.' ') then
         if (k.eq.0) then
            ident = ident + 1
            i = i + 1
            if (ibug.eq.1) write (iutw,*) 'in uffch - ident=',ident
            if (ident.le.non) go to 10
            nxt = i
            istat = 1
            go to 60
            else if (k.ne.0) then
               if (is.eq.0) go to 40
               if (is.eq.1) go to 20
               endif
         else if (chin(i:i).eq.iap.and.iapos.eq.0) then
            is = is + 1
            islnt = 1
            icomma = 1
            icolon = 1
            ibrac = 1
         else if (chin(i:i).eq.'/'.and.islnt.eq.0) then
            is = is + 1
            iapos = 1
         else if (chin(i:i).eq.','.and.icomma.eq.0) then
            if (k.eq.0) then
               is = 1
               iapos = 1
               islnt = 1
               icolon = 1
               ibrac = 1
               else
                  is = 2
                  i = i - 1
               endif
         else if (chin(i:i).eq.':'.and.icolon.eq.0) then
            if (k.eq.0) then
               is = 1
               iapos = 1
               islnt = 1
               icomma = 1
               ibrac = 1
               else
                  is = 2
                  i = i - 1
               endif
         else if (chin(i:i).eq.'['.and.ibrac.eq.0) then
            is = 1
            iapos = 1
            islnt = 1
         else if (chin(i:i).eq.']'.and.ibrac.eq.0) then
            is = is + 1
         else
            go to 20
         endif
c
c  the k.eq.0 to handle embedded ' or / with no embedded spaces
      if (is.eq.1.and.k.eq.0) go to 30
      if (is.eq.2) go to 40
c
c  valid character
20    k = k + 1
      if (ibug.eq.1) write (iutw,*) 'in uffch - k=',k
      chout(k:k) = chin(i:i)
      ident = 0
c
c  increment position in input array
30    if (k.lt.iwidth) then
         i = i + 1
         go to 10
         endif
c
40    nxt = i + 1
      if (chin(i+1:i+1).eq.iap) nxt = nxt + 1
      if (ibug.eq.1) write (iutw,*) 'in uffch - k=',k,' iwidth=',iwidth
      if (k.ge.iwidth) go to 60
c
c  fill right with spaces
      if (k.gt.0) then
         ixs = 0
         k = k + 1
         do 50 j=k,iwidth
            chout(j:j) = ' '
c        search for next non-blank character for next field
            i = i + 1
            if (ixs.gt.0) go to 50
            if (chin(i:i).eq.' ') then
               nxt = i
               else
                  nxt = i
                  ixs = 1
               endif
50          continue
         else
c        null field
            istat = 2
         endif
c
60    if (ibug.eq.1) write (iutw,*) 'exit uffch - iptr=',iptr,
     +   ' iwidth=',iwidth,' nxt=',nxt,' chout=',chout(1:iwidth),
     +   ' istat=',istat
c
      return
c
      end
