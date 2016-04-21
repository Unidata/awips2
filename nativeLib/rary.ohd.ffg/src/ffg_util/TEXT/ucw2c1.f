c  ================================================================
c  pgm ucw2c1 (outar,ipos,mwid,num,loc,inar)
c
c  out: outar  .... output array
c   in: ipos   .... position to start in array inar
c   in: mwid   .... useable width of word in inar
c   in: num    .... number of words in inar
c   in: loc    .... location to start in array inar
c   in: inar   .... input array
c  ================================================================
      subroutine ucw2c1 (outar,ipos,mwid,num,loc,inar)
c..................................................................
c  Routine converts character word(s) of mwid characters starting 
c  at word loc to word num in inar to character*1 array beginning
c  at ipos in outar.
c
c..................................................................
c  Initially written by
c      Tim Sweeney, HRL                                  Mar 1999
c..................................................................
c
      include 'ffg_inc/iuws'
      character*(*) inar(*)
      character*1 outar(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/ucw2c1.f,v $
     . $',                                                             '
     .$Id: ucw2c1.f,v 1.1 2001/08/16 17:42:50 dws Exp $
     . $' /
C    ===================================================================
C
c
      if(num.eq.1) loc = 1
c
      do 1100 i=loc,num
      if(mwid.gt.1) then
        do 1050 k=1,mwid
        outar(ipos) = inar(i)(k:k)
 1050   ipos = ipos + 1
c
      else if(mwid.eq.1) then
        outar(ipos) = inar(i)
        ipos = ipos + 1
      endif
 1100 continue
c
c      write(iupr,30) ipos,(outar(i),i=1,ipos)
c   30 format(' ipos=',i3,':', 100a1 )
      return
      end
