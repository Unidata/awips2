c  ==================================================================
c  pgm:  tablid (iupr,ibug,line,num,icent,isubc,tbid,ic)
c
c   in: iupr   .... unit number of output
c   in: ibug   .... debug control
c   in: line   .... input array
c  i/o: num    .... sequence number of parameter
c  out: icent  .... originating center
c  out: isubc  .... sub-center
c  out: tbid   .... table identifier
c  out: ic     .... completion code
c  ==================================================================
c
      subroutine tablid_sub (iupr,ibug,line,num,icent,isubc,tbid,ic)
c
c....................................................................
c
c  Routine determines table identifier of optional table(s) being
c  used.
c
c....................................................................
c  Initially written by
c     Tim Sweeney, HL                                    Apr 2000
c....................................................................
c
      character*(*) line,tbid
      character*20 work
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/tablid.f,v $
     . $',                                                             '
     .$Id: tablid.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
      if (ibug.gt.0) write (iupr,*) 'enter tablid'
c
      ic = 0
      if (line(1:1).eq.'$'.or.line(1:4).eq.'    ') then
         ic = 9
         go to 30
         endif
c
c  indicator of table info
      iptr = 1
      nwid = 2
      call uffch (line,iptr,nwid,work,nxt,ic)
      if (work(1:2).ne.'-1') go to 30
c
c  center
      iptr = nxt
      nwid = 2
      call uffir (line,iptr,nwid,icent,r,nxt,ic)
c
c  sub-center
      iptr = nxt
      call uffir (line,iptr,nwid,isubc,r,nxt,ic)
c
c  table id
      iptr = nxt
      nwid = 6
      call uffch (line,iptr,nwid,tbid,nxt,ic)
      num = 0
      ic = 1
c
      if (ibug.gt.0) write (iupr,20) icent,isubc,tbid
20    format (' User table: icent=',i4,' isubc=',i4,' tbid=',a6)
c
30    return
c
      end

