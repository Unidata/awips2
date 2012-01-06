c  =====================================================================
c  pgm:  dugdpm (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine dugdpm (po,iout)
c
c.......................................................................
c
c  This routine dumps parameters that define grid parameters (runoff
c  adjust factors) to a file in the same format that can be used to
c  define grid parameters.
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL                                    May 1992
c
c  expanded intensity capabilities
c       Tim Sweeney, HRL                                    Nov 1997
c
c  added percent impervious area
c       Tim Sweeney, HL                                     Jun 2001
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
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/dugdpm.f,v $
     . $',                                                             '
     .$Id: dugdpm.f,v 1.2 2004/01/30 17:46:57 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dugdpm',1,1,ibug)
c
c  get parameters
      call getgpm (po)
c
      write (iout,10) igptyp,iffgid,iqoptg,iroptg,bank,pcimpg
10    format (a4,1x,2a4,3x,i2,4x,i2,2f6.2)
c
      if (iqoptg.gt.0) then
c     high flow adjustment parameters
         write (iout,20) taqg,qtsidg,dtcqg,intqg
20    format (6x,5(2x,f4.0),1x,2a4,1x,a4,i4)
         endif
c
      if (iroptg.eq.1.or.iroptg.eq.2.or.iroptg.eq.5) then
c     runoff adjustment parameters
         write (iout,30) rinten
30    format (6x,5f6.2)
         endif
c
      return
c
      end
