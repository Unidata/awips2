c  =====================================================================
c  pgm:  putare (iuse,po,ic)
c
c  out: iuse   .... number of words used to store area parameters in po
c  out: po     .... parameter array
c  i/o: ic     .... completion code
c  =====================================================================
      subroutine putare (iuse,po,ic)
c.......................................................................
c  put data type 'affg' parameters in po array
c
c.......................................................................
c  Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character*4 blnk,cnone,type
      character*8 sname
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/arparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/putare.f,v $
     . $',                                                             '
     .$Id: putare.f,v 1.2 2004/09/13 14:23:57 scv Exp $
     . $' /
C    ===================================================================
C
c
      data sname/ 'putare  '/
      data blnk/ '    '/
      data cnone/ 'none'/
c
      artyp = 'affg'
c
      call prbug (sname,1,1,ibug)
      if(ic.gt.0) goto 9999
c
c  file version number
      avers = 1.1
      po(1) = avers
c
c  transfer to parameter array po
c  identifier
      call umemov(areaid,po(2),2)
c
c  type code
      call umemov(artyp,po(4),1)
c
c  areal description
      call umemov(adesc,po(6),5)
c
c  boundary identifier
      call umemov(bbid,po(11),2)
c
      if(bbid(1).eq.blnk.or.bbid(1).eq.cnone) then
c
c  latitude, longitude of centroid of box
        po(13) = alat
        po(14) = alon
c
c  1/2 widths for box
        po(15) = alath
        po(16) = alonh
      end if
c
c  location of output flash flood guidance values
      laffg = 26
      po(17) = laffg
c
c  duration flag
      po(18) = kadurf
c
c  area runoffs
      do 1030 i=1,5
 1030 po(i+18) = aro(i)
c
c  computation control
      po(24) = iropta
c
c  area computation time
      po(laffg-1) = lacpd
c
c  ffg array
      do 1050 i=1,5
 1050 po(laffg+i-1) = affg(i)
      iusea = laffg + 4
c
c  unused
      do 1090 i=1,6
 1090 po(iusea+i) = 0.01
      iusea = iusea + 6
c
c  number of words used
      po(5) = float(iusea)
      iuse  = iusea
      if(ibug.gt.0) write(iud,30) iusea
   30 format(10x,'affg:  number of words for parameters =',i4)
c
 9999 return
      end
