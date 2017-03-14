c  =====================================================================
c  pgm:  putsup (jed,iuse,po,ic)
c
c   in: jed    .... editor control
c                     = 0, editor not used
c                     = 1, editor used
c  out: iuse   .... number of words to store parameters in po
c  out: po     .... array containing headwater parameters
c  i/o: ic     .... completion code
c  =====================================================================
c
      subroutine putsup (jed,iuse,po,ic)
c
c.......................................................................
c
c  put wsup water supply parameters in po array
c
c.......................................................................
c      Initially written by
c           Tim Sweeney, HRL                         Sept 1995
c.......................................................................
c
      character*4 blnk,cend(2)
      character*4 lcend(2)
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/wsparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/putsup.f,v $
     . $',                                                             '
     .$Id: putsup.f,v 1.3 2004/09/13 14:24:03 scv Exp $
     . $' /
C    ===================================================================
C
      data cend / 'ENDI','D   '/
      data lcend/ 'endi','d   '/
      data blnk/ '    '/
c
c
      call prbug ('putsup',1,1,ibug)
c
      if (ic.gt.0) go to 80
c
      wstyp = 'wsup'
c
c  file version number
      wvers = 1.0
      po(1) = wvers
c
c  transfer to parameter array po
c  identifier
      call umemov (wsid,po(2),2)
c
c  type code
      call umemov (wstyp,po(4),1)
c
c  location name
      call umemov (sdesc,po(6),5)
c
c  area
      po(11) = sarea
c
c  water supply boundary identifier
      call umemov (wsbid,po(12),2)
c
c  unused
      do 10 i=1,4
10       po(i+13) = -999.0
c
c  location in po array of output water supply values
      lro = 21
      po(18) = lro + 0.01
c
c  location of number of component areas
      loar = 32
      po(19) = loar
c
c  computation date for rainfall-runoff curves
      po(lro-1) = lwcpd
c
c  water supply runoff
      do 20 i=1,6
         po(lro+i-1) = wsup(i)
20       continue
c
c........................................................
c  unused
      do 30 i=1,5
         po(lro+5+i) = -999.0
30       continue
c
c..............................................
c  areas
c..............................................
      lpe = loar
c
c  input area weights, type code and identifiers
      lpe = lpe + 1
c
c  check for 'endid' when parameters are entered from a file (jed=0)
c  or limit loop 1600 to snars when parameters are entered from the
c  editor (jed=1)
      if (jed.eq.0) then
         k = 16
         else
           k = nsars
         endif
      nsars = 0
c
      do 50 i=1,k
         if (sarid(1,i).eq.blnk) go to 50
         if (jed.eq.1) go to 40
         if (sarid(1,i).ne.cend(1).and.sarid(1,i).ne.lcend(1)) go to 40
         if (sarid(2,i).eq.cend(2).or.sarid(2,i).eq.lcend(2)) go to 60
40       nsars = nsars + 1
         j = lpe + (nsars-1)*3
c     transfer variables to po array
         po(j) = swt(i)
         call umemov (sarid(1,i),po(j+1),2)
50       continue
c
c  number of weighted areas
60    po(loar) = float(nsars)
c
c....................................................
c    maximum 75 po values if 15 areas needed
c....................................................
c  number of words used
      iuses = 3*nsars + lpe
      po(5) = float(iuses)
      iuse  = iuses
      if (ibug.gt.0) write (iud,*) ' iuses=',iuses
c
80    return
c
      end
