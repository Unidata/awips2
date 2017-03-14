C$PRAGMA C (GBF_WRIT)
c***********************************************************************
c
c  pgm:  gpcomm (iupr,iud,ibug2,idev,ihdr,ffm,pid,cct,
c                wmo,senof,kzyr,kzmo,kzda,kzhr,kzmn)
c
c   in: iupr   .... unit number of terminal or logfile
c   in: iud    .... unit number for debug
c   in: ibug2  .... debug control
c   in: idev   .... output device
c  i/o: ihdr   .... product header control
c   in: ffm    .... file format
c   in: pid    .... product identifier
c   in: cct    .... circuit
c   in: wmo    .... wmo identifier
c   in: senof  .... sending office
c   in: kzyr   .... year (4 digits)
c   in: kzmo   .... month
c   in: kzda   .... day
c   in: kzhr   .... hour
c   in: kzmn   .... minutes
c
c***********************************************************************
c
      subroutine gpcomm (iupr,iud,ibug2,idev,ihdr,ffm,pid,cct,
     +                   wmo,senof,kzyr,kzmo,kzda,kzhr,kzmn)
c
c***********************************************************************
c
c  This routine outputs the comms header according to ihdr:
c     ihdr = 0:  skip header
c     ihdr = 1:  AWIPS
c                        ZCZC CCCNNNXXX
C                        TTAA00 CCCC DDHHMM
c                        NNNXXX
c                   Must use actual WMO id. ZCZC used only for multiple
c                   message parsing from a single file.  CCCNNNXXX
c                   supports distributeProduct command line argument.
c                   The ZCZC and NNNXXX lines are not included for
c                   binary files.
c     ihdr = 2:  AFOS
c                        ZCZC CCCNNNXXX ADR
c                        TTAA00 CCCC DDHHMM
c                   Current DDHHMM is substituted if actual WMO id used.
c                   Generic DDHHMM is used if generic TTAA00 is used.
c     ihdr = 3:  AWIPS   ZCZC
C                        TTAA00 CCCC DDHHMM
c                        NNNXXX
c                   Must use actual WMO id.
c
c  Output file is formatted or unformatted:
c         ffm:  f = formatted    (all SHEF products)
c               u = unformatted  (GRIB products)
c
c***********************************************************************
c  Initially written by
c     Tim Sweeney, HRL                                    Aug 1992
c
c  Added AWIPS comms option
c     Tim Sweeney, HRL                                    Sept 1997
c
c  Added option to output to unformatted or formatted file.
c  Removed common blocks, moved variables to argument list.
c     Tim Sweeney, HRL                                    Jun 1998
c
c  Changed algorithm to output unformatted AWIPS header
c     Tim Sweeney, HRL                                    Nov 1998
c
c  Added a third function to variable ihdr:
c               0 = skip header
c     Tim Sweeney, HRL                                    Feb 2000
c***********************************************************************
c
      character*1 blnk,ffm,nowz(12)
      character*3 crcrlf
      character*4 smesg,pid(3),cct,wmo(2),senof
      character*6 cdate,wmoid
      character*21 temp
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_shared/RCS/gpcomm.f,v $
     . $',                                                             '
     .$Id: gpcomm.f,v 1.5 2004/09/13 14:59:04 scv Exp $
     . $' /
C    ===================================================================
C
ccc      data crcrlf /Z'0D0D0A'/
c
c
      call prbug ('gpcomm',1,1,ibug)
ccc      ibug = 1
c
      crcrlf(1:1)=char(13)
      crcrlf(2:2)=char(13)
      crcrlf(3:3)=char(10)
      blnk = ' '
      smesg = 'ZCZC'
c
      if (ihdr.eq.0) go to 100
      if (cct.eq.'ftp '.or.cct.eq.'FTP ') go to 100
c
      if (ibug.eq.1) write (iud,*) 'in gpcomm - senof=',senof,
     +   ' wmo=',wmo
c
c  convert date and time to single character digits
      if (kzmo.le.0) kzmo = 1
      if (kzda.le.0) kzda = 1
      ipos = 1
      num = 1
      nwid = 4
      call ffi2a (nowz(1),ipos,nwid,num,kzyr)
      nwid = 2
      call ffi2a (nowz(5),ipos,nwid,num,kzmo)
      call ffi2a (nowz(7),ipos,nwid,num,kzda)
      call ffi2a (nowz(9),ipos,nwid,num,kzhr)
      call ffi2a (nowz(11),ipos,nwid,num,kzmn)
      if (nowz(5).eq.' ') nowz(5) = '0'
      if (nowz(7).eq.' ') nowz(7) = '0'
      if (nowz(9).eq.' ') nowz(9) = '0'
      if (nowz(11).eq.' ') nowz(11) = '0'
c
      if (ffm.eq.'u') go to 80
c
c  formatted file ......................................................
c
      if (ihdr.eq.1) then
c     AWIPS multi-product separator & product id
         write (idev,10) pid,cct
10    format ('ZCZC ',2a4,a1,1x,a3)
         else if (ihdr.eq.2) then
c        AFOS: first line
            write (idev,20) pid,cct
20    format ('ZCZC ',2a4,a1,1x,a3)
         else if (ihdr.eq.3) then
            write (idev,30)
30    format ('ZCZC')
         endif
c
c   AWIPS:  first line,    AFOS: 2nd line      (WMO line)
      if (wmo(1).eq.'TTAA') then
         write (idev,40) wmo,senof
40    format (a4,a3,a4,1x,'DDHHMM')
         else
            write (idev,50) wmo,senof,(nowz(i),i=7,12)
50    format (a4,a3,a4,1x,6a1)
         endif
c
c   AWIPS:  2nd line
      if (ihdr.ne.2) then
         if (pid(2)(4:4).eq.' ') then
            write (idev,60) (pid(i),i=1,2)
60    format (a4,a2)
            else
               write (idev,70) pid(1)(4:4),pid(2),pid(3)
70    format (a1,a4,a1)
            endif
         endif
c
      go to 100
c
c  unformatted file ....................................................
c
c  AWIPS:  first line             (WMO line)
80    do 90 i=1,6
         cdate(i:i) = nowz(i+6)
90       continue
      wmoid(1:4) = wmo(1)
      wmoid(5:6) = wmo(2)(1:2)
      temp = wmoid(1:6)//' '//senof//' '//cdate//crcrlf
      call gbf_writ (21,temp,ierr)
c
100   ihdr = 0
c
      return
c
      end
