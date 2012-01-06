c  =====================================================================
c  pgm:  drname (idur,lrow,ident)
c
c   in: idur   .... duration in hours
c   in: lrow   .... local HRAP row number
c  out: ident  .... file identifier
c  =====================================================================
c
      subroutine drname (idur,lrow,ident)
c
c.......................................................................
c
c  create file name for specified duration
c
c.......................................................................
c  initially written by
c      Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character*4 idurc
      character*8 ident
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/drname.f,v $
     . $',                                                             '
     .$Id: drname.f,v 1.3 2003/03/14 18:25:16 dws Exp $
     . $' /
C    ===================================================================
C
c
c  convert idur to characters
      call ui2c4 (idur,idurc)
      lidurc = lenstr(idurc)
c
      if (lrow.lt.0) then
         ident = 'hr'//idurc(1:lidurc)
         endif
c
      if (lrow.gt.0) then
         ident = 'xhr'//idurc(1:lidurc)
         endif
c
      if (lrow.eq.0) then
         ident = ident(1:lenstr(ident))//idurc(1:lidurc)
         endif
c
      return
c
      end
