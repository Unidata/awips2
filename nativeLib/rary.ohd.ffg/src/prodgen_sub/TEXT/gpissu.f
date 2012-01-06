c  =====================================================================
c  pgm:  gpissu
c
c  all variables passed in common blocks.
c  =====================================================================
c
      subroutine gpissu
c
c.......................................................................
c
c  Generate issue date and time info for a product.
c
c.......................................................................
c        Initially written by
c            Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*1 sep
      character*2 ampm,stdt(2),chr,cmin,cday
      character*3 day(7),mon(12)
      character*4 cyear
      character*10 string
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/iodno'
      include 'ffg_inc/iodev'
      include 'ffg_inc/timez'
      include 'prodgen_inc/poptns'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/prodgen_sub/RCS/gpissu.f,v $
     . $',                                                             '
     .$Id: gpissu.f,v 1.2 2003/03/14 18:36:48 dws Exp $
     . $' /
C    ===================================================================
C
      data stdt/'ST  ','DT  '/
      data day/'SUN ','MON ','TUE ','WED ','THU ','FRI ','SAT '/
      data mon/'JAN ','FEB ','MAR ','APR ','MAY ','JUN ','JUL ',
     +         'AUG ','SEP ','OCT ','NOV ','DEC '/
c
c
      call prbug ('gpissu',1,1,ibug)
c
c  determine if AM or PM
      ihr12 = khr
      ampm = 'AM'
      if (ihr12.ge.12.and.ihr12.lt.24) ampm = 'PM'
      if (ihr12.ge.13) ihr12 = ihr12 - 12
      if (ihr12.le.0)  ihr12 = 12
c
c  convert daylight time switch (ldayl) to 1 for standard or 2 daylight
      lstdt = ldayl + 1
c
c  convert date from integer to character
      ibeg = 1
      nval = 1
      nchar = 2
      call ffi2a (chr,ibeg,nchar,nval,ihr12)
      call ffi2a (cmin,ibeg,nchar,nval,kmn)
      call ffi2a (cday,ibeg,nchar,nval,kda)
      nchar = 4
      call ffi2a (cyear,ibeg,nchar,nval,kyr)
      
ccc      if (chr(1:1).eq.' ') chr(1:1) = '0'
      if (cmin(1:1).eq.' ') cmin(1:1) = '0'
ccc      if (cday(1:1).eq.' ') cday(1:1) = '0'
      call uleftc (chr,len(chr),lchr)
      call uleftc (cday,len(cday),lcday)
c
      if (ibug.eq.1) write (iud,*) 'chr=',chr,' cmin=',cmin,
     +   ' cday=',cday,' cyear=',cyear
c
c  output issuance date and time
ccc      string='ISSUED'
      string=' '
      sep=' '
      write (iuf,10) string(1:lenstr(string)),
     +               chr(1:lchr),cmin,sep,
     +               ampm,sep,
     +               tzout,
     +               stdt(lstdt),sep,
     +               day(ndawk),sep,
     +               mon(kmo),sep,
     +               cday(1:lcday),sep,
     +               cyear
10    format (       a,
     +               a,a,a,
     +               a,a,
     +               a1,
     +               a,a,
     +               a,a,
     +               a,a,
     +               a,a,
     +               a)
c
      return
c
      end
