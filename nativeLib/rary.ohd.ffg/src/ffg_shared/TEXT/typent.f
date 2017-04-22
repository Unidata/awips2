c  =====================================================================
c  pgm:  typent (resp)
c
c  out: resp   .... source of parametric information
c  =====================================================================
      subroutine typent (resp)
c.......................................................................
c
c this routine determines type of entry of parameters
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL - Apr 1992
c.......................................................................
c
      character resp
c
      include 'ffg_inc/iuws'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/typent.f,v $
     . $',                                                             '
     .$Id: typent.f,v 1.2 2002/05/15 18:26:33 dws Exp $
     . $' /
C    ===================================================================
C
c
10    write (iutw,20)
20    format (' Enter parameters (f-file [t]-terminal m-menu): ',$)
      read (iutr,'(a)') resp
      if (resp.eq.' ') then
         resp = 't'
         else if (resp.eq.'T'.or.resp.eq.'t') then
            resp = 't'
         else if (resp.eq.'F'.or.resp.eq.'f') then
            resp = 'f'
         else if (resp.eq.'M'.or.resp.eq.'m') then
            resp = 'm'
         else
            write(iutw,30)
30    format (' ERROR: invalid option.')
            go to 10
         endif
c
      return
c
      end
