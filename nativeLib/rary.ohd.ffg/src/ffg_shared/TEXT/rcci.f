      subroutine rcci
c=======================================================================
c  Subroutine reads control info for rating curves 
c
c=======================================================================
c  Initially written by
c         Lifted from routine INSTRT by Joe Ostrowski & Tim Sweeney
c         - HRL, March 1993
c
c  Replaced routines readit and closal with ureadt and uclosl.
c        Tim Sweeney, HRL                                 Jan 1999
c=======================================================================
c
      include 'common/fcunit'
      include 'common/frcptr'
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
c
      character*8 sname
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/rcci.f,v $
     . $',                                                             '
     .$Id: rcci.f,v 1.1 2001/08/16 17:42:44 dws Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'rcci    ' /
c
      call prbug (sname,1,2,ibug)
      if(ibug.ge.2) write(iud,7020)
c
c  frcptr
      call ureadt(kfrcpt,1,nrc,istat)
c
c  can only read minimum of mrc or mrcf rating curves from file
c  but mrc is zero and not used
      mmr = nrc
      mrc = nrc
      if (nrc.gt.mrcf) mmr = mrcf
      do 150 j=1,mmr
      i = j+1
      call ureadt(kfrcpt,i,rczz(1,j),istat)
  150 continue
c
c  close all files
c
      call uclosl
c
      if(ibug.ge.2) write(iud,7050) kfrcpt,nrc,mrc,mrcf
      return
c
c  debug formats
 7020 format(5x,'About to fill common/frcptr/')
 7050 format(5x,'kfrcpt=',i3,'   nrc=',i5,'   mrc=',i5,
     +       '   mrcf=',i5)
c
      end
