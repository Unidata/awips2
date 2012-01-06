c  =====================================================================
c  pgm:  getsro (iqctl,ilocrr,otyprr,po,sro,istat)
c
c   in: iqctl  .... option control
c   in: ilocrr .... location of rainfall-runoff parameters in po array
c   in: otyprr .... operation type for rainfall-runoff model
c   in: po     .... array containing ffg parameters
c  out: sro    .... storm runoff
c  out: istat  .... completion code
c  =====================================================================
c
      subroutine getsro (iqctl,ilocrr,otyprr,po,sro,istat)
c
c.......................................................................
c
c  get storm runoff from selected rainfall-runoff model carryovers
c
c.....................................................................
c     Program initially written by
c           Tim Sweeney, HRL - Feb 1999
c.....................................................................
c
      character*8 otyprr
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_sub/RCS/getsro.f,v $
     . $',                                                             '
     .$Id: getsro.f,v 1.2 2003/08/20 13:11:06 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getsro',1,1,ibug)
c
      istat = 0
c
      if (iqctl.ne.2) go to 10
c
      if (otyprr(1:4).eq.'SAC-') then
         continue
         else if (otyprr(1:4).eq.'API-'.and.otyprr(5:8).eq.'CONT') then
         continue
         else
         sro = po(ilocrr+7)
         if (ibug.gt.0) write (iud,*) 'in getsro - otyprr=',otyprr,
     +      ' ilocrr=',ilocrr,' sro=',sro
         endif
c
10    return
c
      end
