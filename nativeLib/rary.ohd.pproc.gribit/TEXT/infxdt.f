c  =====================================================================
c  pgm:  infxdt (imo,ida,iyr,ihr,imn,isc,dtf)
c
c  i/o: imo    .... month
c  i/o: ida    .... day
c  i/o: iyr    .... century and year
c  i/o: ihr    .... 24 hour clock
c  i/o: imn    .... minutes
c  i/o: isc    .... seconds (if imo=>0)
c                   valid hour (imo<0 and 'f' coded in date)
c  o/i: dtf    .... date & time (Informix format)
c  =====================================================================
c
      subroutine infxdt (imo,ida,iyr,ihr,imn,isc,dtf)
c
c.......................................................................
c  Routine converts between date/time variables (imo, ida, iyr, ihr,
c
c  imn, isc) and date format (ccyy-mm-dd hh:mm:ss) when imo => 0.
c
c  imo=> 0:  imn=>0
c            convert variables imo, ida, iyr, ihr, imn, isc to
c            format ccyy-mm-dd hh:mm:ss
c
c            imn<0
c            convert variables imo, ida, iyr, ihr to format
c                   ccyy-mm-dd hhfvvv
c            where f indicates forecast at valid hours vvv
c            from reference time ihr
c
c  imo < 0: convert date format ccyy-mm-dd hhfvvv or
c                               ccyymmdd/hhfvvv  or
c                               ccyy-mm-dd hh:mm:ss   or
c                               ccyy-mm-dd hh
c
c          to reference time imo, ida, iyr, ihr and valid hr vvv after
c          reference time.   vvv is set to 0 for the 3rd & 4th formats.
c          isc is set to vvv.
c.......................................................................
c  Initially written by
c        Tim Sweeney, HRL                                  Feb 2000
c.......................................................................
c
      character*20 dtf
      character*1 work(20)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/wfo_rfc/precip_proc/source/gribit/src/RCS/infxdt.f,v $
     . $',                                                             '
     .$Id: infxdt.f,v 1.1 2006/05/03 13:43:58 gsood Exp $
     . $' /
C    ===================================================================
C
c
      istat = 0
c
      if (imo.lt.0) go to 40
c
c.......................................................................
c
c  convert to xmrg date format
c
      ipos = 1
      iwidth = 4
      num = 1
      call ffi2a (work,ipos,iwidth,num,iyr)
      work(5) = '-'
      iwidth = 2
      ipos = 6
      call ffi2a (work,ipos,iwidth,num,imo)
      work(8) = '-'
      ipos = 9
      call ffi2a (work,ipos,iwidth,num,ida)
      work(11) = ' '
      ipos = 12
      call ffi2a (work,ipos,iwidth,num,ihr)
c
c  replace blanks with zeroes
      do 10 i=6,12,3
         if (work(i).eq.' ') work(i) = '0'
10       continue

      if (imn.ge.0) then
         work(14) = ':'
         ipos = 15
         call ffi2a (work,ipos,iwidth,num,imn)
         if (work(15).eq.' ') work(15) = '0'
         work(17) = ':'
         ipos = 18
         call ffi2a (work,ipos,iwidth,num,isc)
         if (work(18).eq.' ') work(18) = '0'
         work(20) = ' '
         is = 20
         else
            work(14) = 'f'
            ipos = 15
            iwidth = 3
            call ffi2a (work,ipos,iwidth,num,isc)
            if (work(15).eq.' ') work(15) = '0'
            if (work(16).eq.' ') work(16) = '0'
            is = 18
         endif
c
c  right fill work with blanks
      do 20 i=is,20
         work(i) = ' '
20       continue
c
      do 30 i=1,20
         dtf(i:i) = work(i)
30       continue
      go to 60
c
c.......................................................................
c
c  convert to component date and time

40    do 50 i=1,20
         work(i) = dtf(i:i)
50       continue
      if (work(5).eq.'-') then
         ipos = 1
         num = 1
         iwidth = 4
         call ffa2i (work,ipos,iwidth,num,iyr,ic1)
         iwidth = 2
         ipos = 6
         call ffa2i (work,ipos,iwidth,num,imo,ic2)
         ipos = 9
         call ffa2i (work,ipos,iwidth,num,ida,ic3)
         ipos = 12
         call ffa2i (work,ipos,iwidth,num,ihr,ic4)
         else
            ipos = 1
            num = 1
            iwidth = 4
            call ffa2i (work,ipos,iwidth,num,iyr,ic1)
            iwidth = 2
            ipos = 5
            call ffa2i (work,ipos,iwidth,num,imo,ic2)
            ipos = ipos + 2
            call ffa2i (work,ipos,iwidth,num,ida,ic3)
            ipos = ipos + 3
            call ffa2i (work,ipos,iwidth,num,ihr,ic4)
         endif
c
      iwidth = 3
      if (work(12).eq.'f') then
         ipos = 13
         mf = 1
         else if (work(14).eq.'f') then
            ipos = 15
            mf = 1
         else if (work(14).eq.':') then
            ipos = 15
            mf = 2
            iwidth = 2
         else
            ipos = 0
         endif
c
c  valid hour if 'f' present. set to isc.  Otherwise, valid hr = 0
      if (ipos.gt.0) then
         call ffa2i (work,ipos,iwidth,num,ivalu,ic5)
         if (mf.eq.1) then
            isc = ivalu
            imn = 0
            else
c           no 'f', valid hr set to 0 (isc)
              imn = ivalu
              isc = 0
            endif
         else
            imn = 0
            isc = 0
        endif
c
      istat = ic1 + ic2 + ic3 + ic4 + ic5
c
c.......................................................................
c
60    return
c
      end

