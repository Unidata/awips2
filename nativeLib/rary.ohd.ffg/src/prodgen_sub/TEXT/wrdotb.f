c  =====================================================================
c  pgm:  wrdotb (ident,mxf,nval,val,nwid,ndec,nda,desca,ndb,descb)
c
c   in: ident  .... identifier
c   in: mxf    .... maximum number of fields
c   in: nval   .... number of values to output
c   in: val    .... array of values
c   in: nwid   .... array containing number of digits in each val
c   in: ndec   .... array containing number of decimal positions in val
c   in: nda    .... number of words in desca
c   in: desca  .... array of info (not used if nda and ndb both = 0)
c   in: ndb    .... number of words in descb
c   in: descb  .... array of info (not used if ndb = 0)
c  =====================================================================
c
      subroutine wrdotb (ident,mxf,nval,val,nwid,ndec,nda,desca,ndb,
     +   descb)
c
c.......................................................................
c
c  This routine assembles and prints a SHEF .B format for nval real
c  numbers in array val of widths in array nwid, decimal positions in
c  array ndec and the physical elements.
c  Description A (area or headwater location) is appended if nda and/or
c  ndb is not zero.  Description B (stream name) is appended if ndb
c  is not zero.
c
c.......................................................................
c  Initially written by Timothy L. Sweeney, HRL - April 1992
c
c  Added arrays for widths and decimals
c            Tim Sweeney, HRL                     March 1995
c.......................................................................
c
      character*4 desca(*),descb(*)
      character*8 ident
      character*100 line
c
      dimension nwid(*),ndec(*),val(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/count'
      include 'ffg_inc/iodno'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/wrdotb.f,v $
     . $',                                                             '
     .$Id: wrdotb.f,v 1.3 2004/01/30 17:59:08 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('wrdotb',1,1,ibug)
c
      mline=len(line)
      line = ' '
      ipos = 1
c
c  set id
      mwid = 8
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,ident)
c
c  set values
      do 20 i=1,nval
         if (i.ne.1) then
            line(ipos:ipos) = '/'
            ipos = ipos + 1
            endif
c     convert real number to single character array
         nfval = 1
         call fff2a (line,ipos,nwid(i),ndec(i),nfval,val(i))
         if (line(ipos:ipos).eq.'*') then
            write (iutw,15) val(i),nwid(i),ndec(i)
            if (iupr.ne.iutw) write (iupr,15) val(i),nwid(i),ndec(i)
15    format (' ERROR in wrdotb - real value ',f13.3,' cannot be ',
     *   'stored in ',i2,' characters using ',i2,' decimal places.')
            nerr = nerr + 1
            endif
         ldec = ipos + nwid(i) - ndec(i) - 2
         if (line(ldec:ldec).eq.' ') line(ldec:ldec) = '0'
         ipos = ipos + nwid(i)
20       continue
c
c  set additional '/' to define SHEF fields when nval is less than mxf
      kb = 0
      if (nval.lt.mxf) then
         n = nval + 1
         do 30 i=n,mxf
            line(ipos:ipos) = '/'
            ipos = ipos + 1
            kb = kb + nwid(i)
30          continue
         endif
c
c  set description A
      if (nda.gt.0.and.desca(1).ne.' ') then
         ipos = kb + ipos + 1
         line(ipos:ipos) = ':'
         ipos = ipos + 1
         mwid = 4
         loc = 1
         call ucw2c1 (line,ipos,mwid,nda,loc,desca)
c     transfer description B
         if (ndb.gt.0.and.descb(1).ne.' ') then
            ipos = ipos + 2
            if (ipos.gt.mline) then
               write (iutw,45) mline,ident
               if (iupr.ne.iutw) write (iupr,45) mline,ident
45    format (' ERROR: maximum number of characters in SHEF line (',
     +   i3,') exceeded for identifier ',a,'.')
               nerr = nerr + 1
               else
                  call ucw2c1 (line,ipos,mwid,ndb,loc,descb)
               endif
            endif
         endif
c
      lline=lenstr(line)
      write (iuf,'(a)') line(1:lline)
ccc      write (iutw,50) line(1:lline)
      if (iupr.eq.iutw) write (iutw,50) line(1:lline)
50    format (5x,a)
      if (iupr.ne.iutw) write (iupr,50) line(1:lline)
c
      return
c
      end

