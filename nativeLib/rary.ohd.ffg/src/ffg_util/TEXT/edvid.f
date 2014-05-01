c  =====================================================================
c  pgm:  edvid (iutr,iutw,resp,ib,npgrp,ncid,cid)
c
c   in: iutr   .... input unit
c   in: iutw   .... output unit
c   in: resp   .... response
c  out: ib     .... first number for list of identifiers
c   in: npgrp .... number of words per group
c   i/o ncid   .... number of identifiers in cid
c   i/o cid    .... array of identifiers
c  =====================================================================
c
         subroutine edvid (iutr,iutw,resp,ib,npgrp,ncid,cid)
c
c.......................................................................
c
c  routine to edit list of (1) location ids and types and (2)
c  product ids in a group of products
c
c.......................................................................
c  initially written by
c       Tim Sweeney, HRL                                     Mar 1991
c.......................................................................
c
         character resp,cresp
         character*4 bid(2),cid(npgrp,1),type
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ffg/src/ffg_util/RCS/edvid.f,v $
     . $',                                                             '
     .$Id: edvid.f,v 1.2 2003/08/20 13:25:22 scv Exp $
     . $' /
C    ===================================================================
C
c
10    write (iutw,20)
20    format (' Enter number: ',$)
      read (iutr,30,err=10) j
30    format (i4)
      if (resp.eq.'L'.or.resp.eq.'l') then
c     list IDs
         ib = j
         go to 150
         endif
c
      write (iutw,40)
40    format (' Enter identifier: ',$)
      read (iutr,50) bid
50    format (2a4)
      if (resp.eq.'D'.or.resp.eq.'d') then
c     delete ID
          write (iutw,60)
60    format (' Are you sure (y or n)? ',$)
          read (iutr,70) cresp
70    format (a)
          if (cresp.eq.'Y'.or.cresp.eq.'y') then
c         compress file
             do 90 i=j,ncid
                do 80 k=1,npgrp
                   cid(k,i) = cid(k,i+1)
80                 continue
90              continue
             do 100 k=1,npgrp
                cid(k,ncid) = ' '
100             continue
             ncid = ncid - 1
             else
             endif
         go to 150
         endif
c
      if (npgrp.gt.2) then
         write (iutw,110)
110   format (' Enter type: ',$)
         read (iutr,50) type
         endif
c
      if (resp.eq.'A'.or.resp.eq.'a') then
c     move IDs down 1 position
         do 130 i=ncid,j,-1
            do 120 k=1,npgrp
               cid(k,i+1) = cid(k,i)
120            continue
130         continue
c     insert new ID and type
         ncid = ncid + 1
         endif
c
c  transfer new or change old ID and type
      if (bid(1).ne.'    ') then
         cid(1,j) = bid(1)
         cid(2,j) = bid(2)
         if (npgrp.eq.3) cid(3,j) = type
         endif
c
150   return
c
      end
