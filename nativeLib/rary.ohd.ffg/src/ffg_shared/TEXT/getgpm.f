c  =====================================================================
c  pgm:  getgpm (po)
c
c   in: po     .... runoff adjust parameters
c  =====================================================================
c
      subroutine getgpm (po)
c
c.......................................................................
c
c  This routine fills common block igparm with runoff adjust parameters
c
c.......................................................................
c      Initially written by
c         Tim Sweeney, HRL                                 Sept 1992
c
c      Expanded capabilities
c         Tim Sweeney, HRL                                 Nov 1997
c
c  added percent impervious area
c         Tim Sweeney, HL                                  May 2001
c.......................................................................
c
      dimension po(*)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/igparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/getgpm.f,v $
     . $',                                                             '
     .$Id: getgpm.f,v 1.2 2004/01/30 17:48:53 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('getgpm',1,1,ibug)
c
c  version number
      fvers = po(1)
c
c  identifier
      call umemov (po(2),iffgid,2)

c  type code
      call umemov (po(4),igptyp,1)
c
c  location of high flow adjust parameters
      lqag = po(8)
c
c  location of runoff/ffg adjust parameters
      lrag = po(9)
c
c  location of overbank parameters
      lob = po(10)
c
c  high flow adjust option
      if (lqag.eq.0) then
         iqoptg = 0
         else
            iqoptg = po(lqag) + 0.01
            do 10 i=1,5
               taqg(i) = po(lqag+i)
10             continue
         call umemov (po(lqag+6),qtsidg,2)
         call umemov (po(lqag+8),dtcqg,1)
         intqg = po(lqag+9)
         endif
c
c  runoff/ffg adjust parameters
      if (lrag.eq.0) then
         iroptg = 0
         else
            iroptg = po(lrag) + 0.01
            do 20 i=1,5
               rinten(i) = po(lrag+i)
20             continue
         endif
c
c  overbank factor
      bank = po(lob)
c
c  percent impervious area
      if (fvers.gt.1.0) then
         pcimpg = po(lob+1)
         else
            pcimpg = 0.0
         endif
c
c  number of words
      iusei = po(5) + 0.01
c
      return
c
      end
