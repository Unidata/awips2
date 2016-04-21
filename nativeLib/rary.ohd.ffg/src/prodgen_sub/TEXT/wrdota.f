c  =====================================================================
c  pgm:  wrdota (ident,nval,val,nwid,ndec,peunit,pcod)
c
c   in: ident    .... identifier
c   in: nval   .... number of values to output
c   in: val    .... array of values
c   in: nwid   .... array containing number of digits in each val
c   in: ndec   .... array containing number of decimal positions for val
c   in: peunit .... units of values
c   in: pcod   .... array of physical codes for array of values
c  =====================================================================
c
      subroutine wrdota (ident,nval,val,nwid,ndec,peunit,pcod)
c
c.......................................................................
c
c  This routine prints a SHEF .A format for nval real numbers in array
c  val of widths in array nwid, decimal positions in array ndec and
c  the physical elements in array pcod of units peunit.
c
c.......................................................................
c  Initially written by
c        Tim Sweeney, Hydrologic Research Lab                Mar 1995
c
c  Added year and century.  Added units code peunit.
c        Tim Sweeney, HRL                                    Mar 1998
c
c  Added time zone (default was Z).  Restructured routine.
c        Tim Sweeney, HRL                                    Mar 1999
c
c  Added century option to Date Created (DC) field.
c        Tim Sweeney, HRL                                    Dec 1999
c.......................................................................
c
      character*3 peunit
      character*5 pcod(*)
      character*8 ident
      character*100 line
c
      dimension ndec(*),nwid(*),val(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/count'
      include 'ffg_inc/timez'
      include 'ffg_inc/iodno'
      include 'ffg_inc/uinfo'
      include 'prodgen_inc/poptns'
      include 'common/fctime'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/wrdota.f,v $
     . $',                                                             '
     .$Id: wrdota.f,v 1.4 2004/01/30 17:59:01 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('wrdota',1,1,ibug)
c
      mline=len(line)
      line = ' '
      ipos = 1
c
c  first line
c
c  set '.A '
      mwid = 3
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'.A ')
c
c  set id
      mwid = 8
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,ident)
c
c  set century
      ipos = ipos + 1
      if (icent.eq.4) then
         mwid = 1
         num = 2
         loc = 1
         call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
         endif
c
c  set yymmdd
      mwid = 1
      num = 8
      loc = 3
      call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
c
c  set time zone
      ipos = ipos + 1
      mwid = 1
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'Z')
c
c  set 'DH'
      ipos = ipos + 1
      mwid = 2
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'DH')
c
c  set DH hour
      mwid = 1
      num = 10
      loc = 9
      call ucw2c1 (line,ipos,mwid,num,loc,lcptim)
c
c  set '/DC'
      mwid = 3
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'/DC')
c
c  set current Z time
      mwid = 1
      num = 12
      loc = 1
      if (icent.eq.2) loc = 3
      call ucw2c1 (line,ipos,mwid,num,loc,nowz)
c
c  set units code
      ipos = ipos + 1
      mwid = 1
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,'/')
      mwid = 3
      num = 1
      loc = 1
      call ucw2c1 (line,ipos,mwid,num,loc,peunit)
c
c  set physical element codes and values to output array
      ie = 2
      if (ie.gt.nval) ie = 1
      do 20 ii=1,ie
         line(ipos:ipos) = '/'
         ipos = ipos + 1
c     set physical element code
         mwid = 5
         num = ii
         loc = ii
         call ucw2c1 (line,ipos,mwid,num,loc,pcod)
c     set real number to single character array
         num = 1
         call fff2a (line,ipos,nwid(ii),ndec(ii),num,val(ii))
         if (line(ipos:ipos).eq.'*') then
            write (iutw,15) val(i),nwid(i),ndec(i)
            if (iupr.ne.iutw) write (iupr,15) val(i),nwid(i),ndec(i)
15    format (' ERROR in wrdotb - real value ',f13.3,' cannot be ',
     *   'stored in ',i2,' characters using ',i2,' decimal places.')
            nerr = nerr + 1
            endif
         if (ndec(ii).gt.0) then
            ldec = ipos + nwid(ii) - ndec(ii) - 2
            if (line(ldec:ldec).eq.' ') line(ldec:ldec) = '0'
            endif
         ipos = ipos + nwid(ii)
20       continue
c
      lline=lenstr(line)
      write (iuf,'(a)') line(1:lline)
ccc      write (iutw,40) line(1:lline)
         if (iupr.eq.iutw) write (iutw,40) line(1:lline)
40    format (5x,a)
      if (iupr.ne.iutw) write (iupr,40) line(1:lline)
c
      if (nval.le.2) go to 90
c
c  continuation lines
c
c    ipos = position in output array line
c    kvl  = counter for variables per line
c    ncon = number of continuation line
c
      kvl = 0
      ncon = 0
c
      do 80 ii=3,nval
         if (kvl.eq.0) then
            line = ' '
            ipos = 1
c        set '.A'
            mwid = 2
            num = 1
            loc = 1
            call ucw2c1 (line,ipos,mwid,num,loc,'.A')
c        set continuation number
            ncon = ncon + 1
            mwid = 1
            num = 1
            loc = 1
            call ffi2a (line,ipos,mwid,num,ncon)
            if (line(ipos:ipos).eq.'*') then
               write (iutw,15) val(i),nwid(i),ndec(i)
               if (iupr.ne.iutw) write (iupr,15) val(i),nwid(i),ndec(i)
               nerr = nerr + 1
               endif
            ipos = ipos + 2
            else
c           insert slant if not first variable on line
               line(ipos:ipos) = '/'
               ipos = ipos + 1
            endif
c     set physical element code
         mwid = 5
         num = ii
         loc = ii
         call ucw2c1 (line,ipos,mwid,num,loc,pcod)
c     set value to character array
         ipos = ipos + 1
         num = 1
         call fff2a (line,ipos,nwid(ii),ndec(ii),num,val(ii))
         if (ndec(ii).gt.0) then
            ldec = ipos + nwid(ii) - ndec(ii) - 2
            if (line(ldec:ldec).eq.' ') line(ldec:ldec) = '0'
            endif
         ipos = ipos + nwid(ii)
         if (ipos.gt.mline) then
            write (iutw,45) mline,ident
45    format (' ERROR: maximum number of characters in SHEF line (',
     +   i3,') exceeded for identifier ',a,'.')
            if (iupr.ne.iutw) write (iupr,45) mline,ident
            go to 90
            endif
c     increment variable counter for line
         kvl = kvl + 1
         if (kvl.lt.5.and.ii.lt.nval) go to 80
            ipos = ipos - 1
            if (ipos.gt.mline) ipos = mline
            lline=lenstr(line)
            write (iuf,'(a)') line(1:lline)
ccc            write (iutw,40) line(1:lline)
            if (iupr.eq.iutw) write (iutw,40) line(1:lline)
ccc            if (iupr.ne.iutw) write (iupr,40) line(1:lline)
            kvl = 0
80       continue
c
90    return
c
      end
