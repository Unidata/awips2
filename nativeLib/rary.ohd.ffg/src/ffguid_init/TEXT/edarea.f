c  =====================================================================
c  pgm:  edarea (ident,ipdv,is,mpo,po,istat)
c
c  out: ident  .... identifier
c   in: ipdv   .... device number of parameter file
c   in: is     .... edit mode for parameters
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine edarea (ident,ipdv,is,mpo,po,istat)
c
c.......................................................................
c
c  editor routine for area add and change definition
c
c.......................................................................
c   Initially written by
c       Tim Sweeney, HRL - March 1992
c.......................................................................
c
      character resp
      character*4 blnk,cnone,unone
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/arparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/edarea.f,v $
     . $',                                                             '
     .$Id: edarea.f,v 1.5 2004/09/13 14:23:23 scv Exp $
     . $' /
C    ===================================================================
C
      data blnk/'    '/
      data cnone/'none'/
      data unone/'none'/
c
c
10    write (iutw,20)
20    format (' Enter Area identifier: ',$)
      read (iutr,'(a)',err=10) ident
c
c  open an affg file
      kod = 2
      call rppfil (ident,artyp,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         call getar (po)
         call umemov (ident,areaid,2)
      else if (istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,areaid,2)
         is = 1
         do 40 i=1,5
            adesc(i) = cnone
40          continue
         iropta = 0
         kadurf = 0
         lacpd = -1
         bbid(1) = blnk
         bbid(2) = blnk
         do 50 i=1,5
            aro(i) = -99.0
50          affg(i) = -999.0
         alat = 0.
         alon = 0.
         alath = 0.
         alonh = 0.
      else
         go to 120
      endif
      icv = 2
      call degcon (icv,lat,alat)
      call degcon (icv,lon,alon)
      alath = alath*60.
      alonh = alonh*60.
      call degcon (icv,lath,alath)
      call degcon (icv,lonh,alonh)
c
60    write (iutw,70) areaid
70    format (/ 25x,'Edit Area Parameters - ',2a4 /)
      write (iutw,80) adesc,iropta,kadurf,bbid,aro
80    format (5x,'( 1) Description:',7x,5a4 /
     +        5x,'( 2) Compute Control (0-grid  3-runoff):',i2 /
     +        5x,'( 3) Duration flag:',8x,i2 /
     +        5x,'( 4) Area boundary ID:  ',2a4 /
     +        5x,'( 5) Area runoffs: ',5f6.2)

      if (bbid(1).eq.'box '.or.bbid(1).eq.'BOX ') then
         write (iutw,90) lat,lath,lon,lonh
90    format (5x,'( 6) Latitude (ddmm):   ',i7,
     +        5x,'( 7) Half-width lat (mm): ',i3 /
     +        5x,'( 8) Longitude (ddmm): ',i8,
     +        5x,'( 9) Half-width lon (mm): ',i3,/)
         endif
      write (iutw,100)
100   format (/ ' Select (number or <return>-exit): ',$)
c
      read (iutr,110,err=60) resp
110   format (a1)
c
      if (resp.eq.' ') then
         go to 120
c
      else if (resp.eq.'1') then
         call edvca (is,iutr,iutw,5,adesc)
      else if (resp.eq.'2') then
         call edvi (is,iutr,iutw,iropta)
      else if (resp.eq.'3') then
         call edvi (is,iutr,iutw,kadurf)
      else if (resp.eq.'4') then
         call edvca (is,iutr,iutw,2,bbid)
         if (bbid(1).eq.cnone.or.bbid(1).eq.unone) then
            bbid(1) = blnk
            bbid(2) = blnk
         endif
      else if (resp.eq.'5') then
         call edvrag (is,iutr,iutw,aro,5,1,ng)
      else if (resp.eq.'6') then
         call edvi (is,iutr,iutw,lat)
         icv = 1
         call degcon (icv,lat,alat)
      else if (resp.eq.'7') then
         call edvi (is,iutr,iutw,lath)
         alath = lath/60.
      else if (resp.eq.'8') then
         call edvi (is,iutr,iutw,lon)
         icv = 1
         call degcon (icv,lon,alon)
      else if (resp.eq.'9') then
         call edvi (is,iutr,iutw,lonh)
         alonh = lonh/60.
      else
         go to 120
      endif
      go to 60
c
120   return
c
      end
