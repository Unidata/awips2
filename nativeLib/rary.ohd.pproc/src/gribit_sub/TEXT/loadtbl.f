c  =====================================================================
c  pgm:  loadtbl_sub (iupr,ibug,ic)
c
c   in: iupr   .... unit number of output device
c   in: ibug   .... debug control
c  out: ic     .... completion code
c  =====================================================================
      subroutine loadtbl_sub (iupr,ibug,ic)
c.......................................................................
c  Routine defines tables from default parameters
c.......................................................................
c  Initially written by
c     Tim Sweeney, HL                               May 2000
c.......................................................................
      include 'xmrg_pm'
      include 'xmrg_tbl'
      include 'grib_pm'
      include 'grib_tbl'
c
      character*1 dum
      character*4 id
      character*5 abv
      character*6 wmo
      character*50 des
      character*56 bdes
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob81/ohd/pproc/src/gribit_sub/RCS/loadtbl.f,v $
     . $',                                                             '
     .$Id: loadtbl.f,v 1.1 2006/10/19 16:06:04 dsa Exp $
     . $' /
C    ===================================================================
C
c
c  load xmrg to grib reference parameters
      do 10 i=1,numx
         xmproc(i) = 'undefine'
10       continue
      do 20 num=1,numx
         call xmparm (iupr,ibug,xline(num),num,ic)
20       continue
c
c  load wmo ids for qpf
      nw1 = 3
      nw2 = 6
      nw3 = 0
      do 30 i=1,12
         call gbparm (iupr,ibug,xqpfwmo(i),nw1,nw2,nw3,num,
     +                wmo,dum,ic)
         if (num.ge.1.and.num.le.12) qpfwmo(num) = wmo
30       continue
c
      mproc = numx
c
c...........................................................
c  load GRIB parameters for Table 0 - originating centers
      nw1 = 3
      nw2 = 56
      nw3 = 0
      do 50 i=1,num0
         call gbparm (iupr,ibug,gtab0(i),nw1,nw2,nw3,num,bdes,dum,ic)
         if (num.le.0.or.num.gt.255) then
            write (iupr,40) num
40    format (' ERROR: center number (',i4,') outside range.')
            go to 50
            endif
         tab_0(num) = bdes
50       continue
c
c  load GRIB parameters for Table A - Processes
      do 60 i=1,255
         tab_a(i) = 'undefined'
60       continue
      do 80 i=1,numa
         call gbparm (iupr,ibug,gtaba(i),nw1,nw2,nw3,num,bdes,dum,ic)
         if (num.le.0.or.num.gt.255) then
            write (iupr,70) num
70    format (' ERROR: process number (',i4,') outside range.')
            go to 80
            endif
         tab_a(num) = bdes
80       continue
c
c  load GRIB parameters for Table C for center 9
      nw2 = 4
      nw3 = 50
      do 90 i=1,255
         scid9c(i) = 'none'
90       continue
      do 110 i=1,num9c
         call gbparm (iupr,ibug,gtab9c(i),nw1,nw2,nw3,num,id,des,ic)
         if (num.le.0.or.num.gt.255) then
            write (iupr,100) num,i
100   format(' ERROR: sub-center number (',i4,') outside range ',
     +   'on input record ',i4,'.')
            go to 110
            endif
         scid9c(num) = id
         scnam9c(num) = des
110      continue
c
c  load GRIB parameters for Table 128
      nw2 = 5
      nw3 = 50
      do 120 i=128,255
         abv128(i) = 'none '
         des128(i) = ' '
120      continue
      do 140 i=128,255
         call gbparm (iupr,ibug,gtab128(i),nw1,nw2,nw3,num,abv,des,ic)
         if (num.lt.128.or.num.gt.255) then
            write (iupr,130) num
130   format (' ERROR: Table 128 number (',i4,') outside range.')
           go to 140
           endif
         abv128(num) = abv
         des128(num) = des
140      continue
c
c  load GRIB parameters for Table 2
      do 150 i=1,255
         abv2(i) = 'none '
         des2(i) = ' '
150      continue
      do 170 i=1,num2
         call gbparm (iupr,ibug,gtab2(i),nw1,nw2,nw3,num,abv,des,ic)
         if (num.le.0.or.num.gt.255) then
            write (iupr,160) num
160   format(' ERROR: Table 2 number (',i4,') outside range.')
            go to 170
            endif
         abv2(num) = abv
         des2(num) = des
170      continue
c
      return
c
      end

