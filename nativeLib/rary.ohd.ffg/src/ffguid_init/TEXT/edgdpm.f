c  =====================================================================
c  pgm:  edgdpm (ident,ipdv,is,mpo,po,istat)
c
c  out: ident  .... identifier
c   in: ipdv   .... device number of parameter file
c   in: is     .... edit mode
c   in: mpo    .... maximum words in array po
c   in: po     .... parameter array
c  out: istat  .... status code
c  =====================================================================
c
      subroutine edgdpm (ident,ipdv,is,mpo,po,istat)
c
c.......................................................................
c
c  routine for grid parameter definition
c
c.......................................................................
c  initially written by
c        Tim Sweeney, HRL                                    Apr 1997
c
c  expanded capabilities
c        Tim Sweeney, HRL                                    Nov 1997
c
c  added percent impervious area
c        Tim Sweeney, HL                                     May 2001
c.......................................................................
c
      character*1 resp
      character*8 ident
c
      include 'ffg_inc/iuws'
      include 'ffg_inc/gdebug'
      include 'ffg_inc/igparm'
c
      dimension po(*)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffguid_init/RCS/edgdpm.f,v $
     . $',                                                             '
     .$Id: edgdpm.f,v 1.5 2004/09/13 14:23:26 scv Exp $
     . $' /
C    ===================================================================
C
c
      call prbug ('edgdpm',1,1,ibug)
c
      igptyp = 'gdpm'
c
10    write (iutw,20)
20    format (' Enter grid parameter id (ffgid): ',$)
      read (iutr,'(a)',err=10) ident
      if (ident.eq.' ') go to 240
c
c  open an gdpm file
      kod = 2
      call rppfil (ident,igptyp,kod,ipdv,mpo,po,npo,istat)
      if (istat.eq.0) then
         rewind (ipdv)
         call getgpm (po)
         call umemov (ident,iffgid,2)
      else if (istat.eq.1) then
c     set default values for new file
         istat = 0
         call umemov (ident,iffgid,2)
         iqoptg = 0
         iroptg = 0
         bank = 1.1
         else
            go to 240
         endif
c
40    write (iutw,50) iffgid
50    format (22x,'Edit Grid Adjust Parameters - ',2a4 /)
      write (iutw,60) iqoptg
60    format (5x,'(1) High Flow Adjust:    ',i2)
      if (iqoptg.gt.0) write (iutw,70) taqg,qtsidg,dtcqg,intqg
70    format (10x,'Hours: ',5f4.0 /
     +        10x,'Forecast Flow time series ID: ',2a4 /
     +        10x,'Type: ',a4,4x,'Time Interval: ',i4,' hours')
c
      write (iutw,80) iroptg
80    format (5x,'(2) Runoff Option:       ',i2)
      if (iroptg.gt.0.and.iroptg.lt.6) write (iutw,90) rinten
90    format (9x,'Value (x.xx for 1, 3, 6, 12, 24-hr durations:' /
     +        10x,5f8.2)
c
      write (iutw,100) bank
100   format (5x,'(3) Overbank Factor (x.xx):  ',f6.2)
c
      write (iutw,110) pcimpg
110   format (5x,'(4) Percent impervious (x.xx): ',f4.2)
c
      write (iutw,120)
120   format (/' Select (number or <return>-exit): ',$)
c
      read (iutr,'(a)',err=40) resp
c
      if (resp.eq.' ') then
         go to 240
      else if (resp.eq.'1') then
         write (iutw,150)
150   format (10x,'0 - Off' /
     +        10x,'1 - Forecast flow at hours entered' /
     +        10x,'2 - Highest fcst flow over next hours' /
     +        10x,'3 - Highest fcst flow in time series' /
     +         9x,'High Flow Adjust: ',$)
         call edvi (is,iutr,iutw,iqoptg)
ccc         if (iqoptg.gt.0) then
            write (iutw,160)
160   format (9x,'Hours for each duration: ')
            call edvrag (is,iutr,iutw,taqg,5,1,ng)
            write (iutw,170)
170   format (9x,'Forecast time series ID: ', $)
            call edvca  (is,iutr,iutw,2,qtsidg)
            write (iutw,180)
180   format (9x,'Forecast time series type code: ',$)
            call edvca (is,iutr,iutw,1,dtcqg)
            write (iutw,190)
190   format (9x,'Forecast time series time interval:', $)
            call edvi  (is,iutr,iutw,intqg)
ccc         endif
      else if (resp.eq.'2') then
         write (iutw,200)
200   format (10x,'0 - Off' /
     +        10x,'1 - Adjust runoff using factors' /
     +        10x,'2 - Use values as FFG' /
     +        10x,'3 - Use runoff as FFG' /
     +        10x,'5 - Adjust FFG using factors' /
     +        10x,'9 - Exclude from grid computations' /
     +         9x,'Runoff/FFG adjust options: ',$)
         call edvi (is,iutr,iutw,iroptg)
         if (iroptg.gt.0.and.iroptg.lt.6.and.iroptg.ne.3) then
            write (iutw,210)
210   format (9x,'Values for each duration:')
            call edvrag (is,iutr,iutw,rinten,5,1,ng)
            endif
      else if (resp.eq.'3') then
         write (iutw,220)
220   format (9x,'Overbank factor: ',$)
         call edvra (is,iutr,iutw,bank,1)
      else if (resp.eq.'4') then
         write (iutw,230)
230   format (9x,'Percent impervious area: ',$)
         call edvra (is,iutr,iutw,pcimpg,1)
      else
         go to 240
      endif
c
      go to 40
c
240   return
c
      end
