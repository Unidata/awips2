c  =====================================================================
c  pgm:  ai43 (nrel,isea,api,atif,aicomp)
c
c   in: nrel   .... number of relationship
c   in: isea   .... season (1 = summer  2 = winter)
c   in: api    .... antecedent precipitation index
c   in: atif   .... final antecedent temperature index
c  out: aicomp .... antecedent index (quad I)
c  =====================================================================
      subroutine AI43 (nrel,isea,api,atif,aicomp)
c.......................................................................
c  This routine is the antecedent index relationship for the Northeast
c  RFC (NERFC).
c  The three relationships for the RFC are contained in separate data
c  initialization routines.  There are three ai relationships for each
c  of the two seasons as follows:
c
c        isea   nrel   routine     Area
c        ====   ====   ========    ==========================
c  summer  1      1    ains43      New England, Eastern NY
c          1      2    aibs43      Buffalo area
c          1      3    airs43      Rochester area
c  winter
c          2      1    ainw43      New England, Eastern NY
c          2      2    aibw43      Buffalo area
c          2      3    airw43      Rochester area
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack    NERFC                            9/22/95
c     Tim Sweeney  HRL                             9/29/95
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
      common /tblbug/ ikh,nrelh
c
      dimension sname(2),ai(2)
      dimension line(5),aindx(15,5),aiati(15,5)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/ai43.f,v $
     . $',                                                             '
     .$Id: ai43.f,v 1.1 1996/03/21 15:50:58 page Exp $
     . $' /
C    ===================================================================
C
c
      data sname/4hAI43,4h    /,nop/43/
c
      call fprbug(sname,2,nop,ibug)
c
      aicomp = 0.00
      iat    = atif
c
c  get ai relationship for specified season and area
      ik = nrel + (isea-1)*3
c
c  for summer
      if(ik.eq.1) then
         call ains43 (len,line,aindx,aiati)
      else if(ik.eq.2) then
         call aibs43 (len,line,aindx,aiati)
      else if(ik.eq.3) then
         call airs43 (len,line,aindx,aiati)
c
c  for winter
      else if(ik.eq.4) then
         call ainw43 (len,line,aindx,aiati)
      else if(ik.eq.5) then
         call aibw43 (len,line,aindx,aiati)
      else if(ik.eq.6) then
         call airw43 (len,line,aindx,aiati)
      else
         write(ipr,5000) ik
 5000    format(10x,'** ERROR **   INVALID ik',i4)
         call error
         goto 900
      endif
c
      if(ik.eq.ikh) then
        goto 400
      else
        ikh = ik
      endif
c
      if(itrace.gt.2) then
        write(iodbug,3100) nrel,isea
        write(iodbug,3200) (line(j),(aindx(i,j),i=1,15),j=1,len)
        write(iodbug,3220)
        write(iodbug,3300) (line(j),(aiati(i,j),i=1,15),j=1,len)
      endif
c   
  400 do 470 i=1,len
      IF(IAT-LINE(I)) 500,690,470
  470 continue
      i = len
      goto 690
  500 x = iat - line(i-1)
      y = line(i) - line(i-1)
      factor = x/y
      n = i

C  DO LOOP 660 DETERMINES THE 'ai' FOR A GIVEN API VALUE
C  FOR THE SURROUNDING 'ati' LINES
      do 660 i=1,2
      do 580 j=1,15
      IF(api-aindx(J,N)) 610,640,580
  580 continue
      ai(i) = 0.
      goto 650
  610 ai(i)=aiati(j,n)+(aiati(j-1,n)-aiati(j,n))*(aindx(j,n)-api)
     1/(aindx(j,n)-aindx(j-1,n))
      ai(i)=ai(i)*0.01
      goto 650
  640 ai(i) = aiati(j,n)*0.01
  650 n = n - 1
  660 continue
      aicomp = ai(2) + (ai(1) - ai(2))*factor
      goto 900
  690 n = i
      do 720 j=1,15
      IF(api-aindx(J,N)) 730,780,720
  720 continue
  730 aicomp=aiati(j,n)+(aiati(j-1,n)-aiati(j,n))*(aindx(j,n)-api)
     1/(aindx(j,n)-aindx(j-1,n))
      aicomp=aicomp*0.01
  750 if(iat.le.line(len) ) goto 900
      aicomp = aicomp + (iat - line(len))*0.01
      goto 900
  780 aicomp = aiati(j,n)*0.01
      goto 750
c
  900 if(itrace.gt.1) write(iodbug,3400) api,atif,aicomp
      return
c
c  debug formats
 3100 format(5x,'nrel=',i2,4x,'isea=',i2)
 3200 format(5x,i5,15f5.2)
 3220 format()
 3300 format(5x,i5,15f6.0)
 3400 format(5x,'api=',f5.2,4x,'atif=',f5.1,4x,'aicomp=',f6.2)
c
      end
