c  =====================================================================
c  pgm:  dusup (po,iout)
c
c   in: po     .... parameter array
c   in: iout   .... output device
c  =====================================================================
c
      subroutine dusup (po,iout)
c
c.......................................................................
c
c  This routine outputs parameters that define water supply runoff
c  to a file in the same format that can be used to define water 
c  supply runoff.
c
c.......................................................................
c       Initially written by
c           Tim Sweeney, HRL                            Sept 1995
c.......................................................................
c
      character*4 cnone/'none'/
      character*8 strng
      character*22 wdesc
c
      dimension po(*)
      dimension jwt(16)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/wsparm'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/dusup.f,v $
     . $',                                                             '
     .$Id: dusup.f,v 1.2 2004/01/30 17:47:04 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('dusup',1,1,ibug)
c
c  get parameters
      call getsup (po)
c
c  check for blank values
      if (sdesc(1).eq.' ') sdesc(1) = cnone
      if (wsbid(1).eq.' ') wsbid(1) = cnone
c
c  add delimiters around description if needed
      call adelim (sdesc,5,nd,wdesc)
c
      write (iout,10) wstyp,wsid,wdesc(1:nd),sarea,wsbid
10    format (a4,1x,2a4,1x,a,f6.0,1x,2a4)
c
c  convert weights
      do 20 i=1,nsars
         jwt(i) = swt(i)*100
20       continue
c
c  add 'endid' as last id
      nsars = nsars + 1
      jwt(nsars) = 0
      strng='endid'
      call umemov (strng,sarid(1,nsars),len(strng)/4)
c
      write (iout,30) (jwt(i),(sarid(j,i),j=1,2),i=1,nsars)
30    format ((4x,5(i3,1x,2a4)))
c
      return
c
      end
