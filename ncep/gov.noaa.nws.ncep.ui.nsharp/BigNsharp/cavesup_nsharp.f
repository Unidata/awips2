c       program sup_nsharp
 
       subroutine cavespnsharp(mlcape, mllcl, temp, lr, shr,
     +                 srh, tier1, matches, p1, sndglist, supl2,
     +                  fname,shr3k,shr9k,srh3, totalSnd)

      
c SARS For Supercells

       parameter (maob=15000) ! Max number of raobs allowed.
       parameter (saob=15)    ! Number of raobs to return to NSHARP.
       
       real mlcape,mlcin,mllcl,mucape,sblcl,shr,srh,stp,
     &mustp,temp,ddd,lr,h500u,h500v,matmlmr(maob),matmlcape(maob),
     &matmlcin(maob),matmllcl(maob),matmucape(maob),matsblcl(maob),
     &matshr(maob),matsrh(maob),matstp(maob),matmustp(maob),
     &mattemp(maob),matddd(maob),matlr(maob),math500u(maob),
     &math500v(maob),ranmlmr,ranmlcape,ranmlcin,ranmllcl,ranmucape,
     &ransblcl,ranshr,ransrh,rantemp,ranlr,ranh500u,
     &ranh500v,ranstp,torcnt,noncnt,matches,p1,p2,supl2(saob),
     &matshr3k(maob),matshr9k(maob),shr3kmat(maob),shr9kmat(maob),
     &shr3k,shr9k,ranshr3kt1,ranshr9kt1,srh3,matsrh3(maob),
     &ransrh3t1
       
       character datestn(maob)*16,dummy*25,matdatestn(maob)*16
       character sndglist(saob)*15, fname*(256), fname1*(256)
       character tortype(maob)*8,suplist(saob)*8       

       integer totalSnd, cnt, maob,i,j,mlmrmat(maob),mlcapemat(maob),
     & mlcinmat(maob),mllclmat(maob),mucapemat(maob),sblclmat(maob),
     &shrmat(maob),srhmat(maob),tempmat(maob),lrmat(maob),stpmat(maob),
     &h500umat(maob),h500vmat(maob),shrcat(maob),matshrcat(maob),tier1,
     & tier1cnt,matcat(maob),srh3mat(maob)
       
c        print *, "****************************************************"
c	print *, "        Entering SUP_NSHARP fortran subroutine"
c        print *, "****************************************************"


1     format(a)
        fname1 = fname(1:len_trim(fname))
c        print *, "Opening input file:  ", fname1(1:len_trim(fname1))
        open(unit=10,status='old',file=fname1,err=999,iostat=IERR)

       

*************  Read file list.txt into second array ********************
c      Note...first line of input file ignored.     
       read(10,1) dummy

       j = 1 
 70    read(10,*,err=70,end=80) matdatestn(j),matcat(j),matmlmr(j),
     &matmlcape(j),matmlcin(j),matmllcl(j),matsrh(j),matshr(j),
     &matstp(j),mattemp(j),matddd(j),matlr(j),matshr3k(j),matshr9k(j),
     &matsrh3(j)

c         matsrh(j) = abs(matsrh(j))
	  if(matcat(j).eq.2) tortype(j)=' SIG TOR'
	  if(matcat(j).eq.1) tortype(j)='WEAK TOR'
	  if(matcat(j).eq.0) tortype(j)=' NON TOR'

	  if(matshr(j).lt.20) matshrcat(j)=1
	  if(matshr(j).ge.20.and.matshr(j).lt.35) matshrcat(j)=2
	  if(matshr(j).ge.35.and.matshr(j).lt.50) matshrcat(j)=3
	  if(matshr(j).ge.50) matshrcat(j)=4	  	

       j=j+1
       if(j.gt.maob)stop'Array size too small to read in data...99999'
       goto 70    
            
 80    close(10)

c count number of soundings      
      cnt = j - 1

c mlcape ranges - k2
        ranmlcape= 1300
        ranmlcapet1 = mlcape*0.25

c mllcl ranges - k4
        ranmllcl = 50
        ranmllclt1= 200

c 0-6 km shear ranges (kt)  - k7
       ranshr = 14
       ranshrt1 = 10
	
c 0-1 km srh ranges (m2/s2) - k8
       if(abs(srh).lt.50) then
       ransrh = 100
       else
       ransrh = srh
       endif
       
       if(abs(srh).lt.100) then
        ransrht1 = 50
       else
        ransrht1 = (abs(srh))*0.30
       endif

c 0-3 srh tier 1 ranges
       if(abs(srh3).lt.100) then
        ransrh3t1 = 50
       else
        ransrh3t1 = (abs(srh3))*0.50
       endif
              
c 500 mb temperature ranges (c) - k9
       rantemp= 7
       rantempt1 = 5

c 700-500 mb lapse rate ranges (c/km)- k10
        ranlr = 1.0
	ranlrt1= 0.8

c 3km and 9km shear matching
        ranshr3kt1 = 15
	ranshr9kt1 = 25
	
c 500 U and V components (kt) - k11 and k12
c       ranh500u(1)= 5
c       ranh500u(2)= 15
c       ranh500u(3)= 20
c       ranh500u(4)= 25
c       ranh500v(1)= 5
c       ranh500v(2)= 15
c       ranh500v(3)= 20
c       ranh500v(4)= 25
            
*************************************************************	

c using sounding i , check against all soundings j . 
     
       tier1 = 0
       tier1cnt = 0
       matches = 0
       torcnt = 0
       noncnt = 0
       p1=0
       p2=0
       totalSnd = cnt
      DO 99 j=1,cnt
         
	mlcapemat(j) = 0
	mlcinmat(j) = 0
	mllclmat(j) = 0
	shrmat(j) = 0
	srhmat(j) = 0
	tempmat(j) = 0
	lrmat(j) = 0
c        h500umat(j) = 0
c  	 h500vmat(j) = 0
	
        if(mlcape.ge.(matmlcape(j)-ranmlcape).and.
     &      mlcape.le.(matmlcape(j)+ranmlcape)) mlcapemat(j)=1
        if(mllcl.ge.(matmllcl(j)-ranmllcl).and.
     &      mllcl.le.(matmllcl(j)+ranmllcl)) mllclmat(j)=1
        if(shr.ge.(matshr(j)-ranshr).and.
     &      shr.le.(matshr(j)+ranshr)) shrmat(j)=1
        if(srh.ge.(matsrh(j)-ransrh).and.
     &      srh.le.(matsrh(j)+ransrh)) srhmat(j)=1
        if(temp.ge.(mattemp(j)-rantemp).and.
     &      temp.le.(mattemp(j)+rantemp)) tempmat(j)=1
        if(lr.ge.(matlr(j)-ranlr).and.
     &      lr.le.(matlr(j)+ranlr)) lrmat(j)=1
 
    
****** Check if all 6 parameters are met, exclude datestn (i) *********
      if(mlcapemat(j).eq.1.and.mllclmat(j).eq.1.
     & and.shrmat(j).eq.1.and.srhmat(j).eq.1.and.
     & tempmat(j).eq.1.and.lrmat(j).eq.1) then	

************* Determine if majority of matches are correct category ***	
      if(matcat(j).eq.1.or.matcat(j).eq.2) torcnt = torcnt + 1.
      if(matcat(j).eq.0) noncnt = noncnt + 1.       
      
      endif

********************  Reset Variable for Tier 1 matches **************	

        mlmrmat(j) = 0
	mlcapemat(j) = 0
	mllclmat(j) = 0
	shrmat(j) = 0
	srhmat(j) = 0
	tempmat(j) = 0
	lrmat(j) = 0
	shr3kmat(j) = 0
	shr9kmat(j) = 0
	srh3mat(j) = 0
c       h500umat(j) = 0
c	h500vmat(j) = 0
	      
********************************* TIER 1 *******************************
        if(mlcape.ge.(matmlcape(j)-ranmlcapet1).and.
     &      mlcape.le.(matmlcape(j)+ranmlcapet1)) mlcapemat(j)=1
        if(mllcl.ge.(matmllcl(j)-ranmllclt1).and.
     &      mllcl.le.(matmllcl(j)+ranmllclt1)) mllclmat(j)=1
        if(shr.ge.(matshr(j)-ranshrt1).and.
     &      shr.le.(matshr(j)+ranshrt1)) shrmat(j)=1
        if(srh.ge.(matsrh(j)-ransrht1).and.
     &      srh.le.(matsrh(j)+ransrht1)) srhmat(j)=1
        if(temp.ge.(mattemp(j)-rantempt1).and.
     &      temp.le.(mattemp(j)+rantempt1)) tempmat(j)=1
        if(lr.ge.(matlr(j)-ranlrt1).and.
     &      lr.le.(matlr(j)+ranlrt1)) lrmat(j)=1
        if(shr3k.ge.(matshr3k(j)-ranshr3kt1).and.
     &      shr3k.le.(matshr3k(j)+ranshr3kt1)) shr3kmat(j)=1
        if(shr9k.ge.(matshr9k(j)-ranshr9kt1).and.
     &      shr9k.le.(matshr9k(j)+ranshr9kt1)) shr9kmat(j)=1
        if(srh3.ge.(matsrh3(j)-ransrh3t1).and.
     &      srh3.le.(matsrh3(j)+ransrh3t1)) srh3mat(j)=1

****** Check if all 6 parameters are met ******************************
 
      if(mlcapemat(j).eq.1.and.mllclmat(j).eq.1.
     & and.shrmat(j).eq.1.and.srhmat(j).eq.1.and.
     & tempmat(j).eq.1.and.lrmat(j).eq.1.and.shr3kmat(j).eq.1.and.
     &shr9kmat(j).eq.1.and.srh3mat(j).eq.1) then	
c
cr         if(tier1.gt.0) GOTO 5
cr	 write(*,*)'--------------------------------------------------'
c5        write(*,8) matdatestn(j), matcat(j), matshr(j)
6         format('** TOP MATCHES FOR  ',a,'***')	 
8         format(1X,a16,1X,I1,2X,'6km= ',f4.1,' kt')	 
          tier1=tier1 + 1
*** John, lets just list them all, at worst they go off the screen ***
         if (tier1 .lt. 15) then
             sndglist(tier1) = matdatestn(j)
             suplist(tier1) = tortype(j)
	     supl2(tier1) = matcat(j)
         endif
        endif 

 	
  99    continue
		
c       if(tier1.ne.1) then
c        print *, '-----------------------------------------------------'
c        write(*,*) tier1,' High Quality Matches Listed Above '
c        print *, '-----------------------------------------------------'
c         else
c        print *, '-----------------------------------------------------'
c        write(*,*) tier1, ' High Quality Match Listed Above '
c        print *, '-----------------------------------------------------'  
c       endif
       
        matches = torcnt + noncnt

c	print *, 'SARS Searching',cnt,' soundings, found',matches
              
       if(matches.ne.0) then
        p1 = torcnt/matches*100
	p2 = noncnt/matches*100
	  
        if(torcnt.gt.noncnt.or.torcnt.eq.noncnt) then
c	  write(*,10) p1
10        format(' SARS Says:',f5.1,'% TOR')	  
	 elseif(torcnt.lt.noncnt) then 
c          write(*,11) p2
11        format(' SARS Says:',f5.1,'% NON-TOR')	  
c	 elseif(torcnt.eq.noncnt) then
c	   write(*,*)' SARS Says: Crap shoot'	 
	endif        
c       else
c        write(*,*) ' No Matches.'
c        write(*,*) ' '
       endif
       
c        write(*,*)'----------------------------------------------------'
c	write(*,*)'To overlay matching soundings, browse to'
c	write (*,*)' /users/mead/pfcdir/'    
c	write(*,*) ''     
       
        return(0)



999     matches = 0     
        p1 = 0
        tier1 = 0
        print *, "ERROR - SARS input file not found.  Aborting..."
       end 
      
