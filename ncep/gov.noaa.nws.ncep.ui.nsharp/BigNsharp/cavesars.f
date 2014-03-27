       subroutine cavesars(mumr, mucape, temp, lr, shr, 
     +                 km9, shr3, ship, srh, tier1, 
     +                 matches, p1, avsize, matches2,
     +                 sndglist, haillist, fname, totalSnd)
 
c Jan 27, 2010, REJ.

       parameter (maob=15000) ! Max number of raobs allowed.
       parameter (saob=15)    ! Number of raobs to return to NSHARP.
       
       real mumr,mucape,temp,lr,shr,km9,shr3,ship,srh,avsize

       real matsize(maob),matmr(maob),matcape(maob),mattemp(maob),
     & matlr(maob),matshr(maob),matkm9(maob),matshr3(maob),p1,p2,
     & matsrh(maob),matship(maob),matmodel(maob),ranmr,rancape,rantemp,
     & ranlr,ranshr,rankm9,ranshr3,ranmrt1,rancapet1,rantempt1,ranshrt1,
     & rankm9t1,ransrh2t1,ransrh,ransrht1,sigcnt,nulcnt,matches,
     & dif(maob),haillist(saob),matelev(maob),mattemp3(maob),
     & matlr2(maob),matches2

       character datestn*15,dummy*25,matdatestn(maob)*15
       character sndglist(saob)*15, fname*(256), fname1*(256)
       
       integer  cnt, maob,j,mrmat(maob),capemat(maob),tempmat(maob),
     &lrmat(maob),shrmat(maob),km9mat(maob),shr3mat(maob),golf,
     & sigm(maob),tier1,tier1cnt,srhmat(maob),totalSnd

c        print *, "****************************************************"
c	print *, "        Entering SARS fortran subroutine"
c        print *, "****************************************************"

1     format(a)

	fname1 = fname(1:len_trim(fname))
c	print *, "Opening input file:  ", fname1(1:len_trim(fname1))
	open(unit=10,status='old',file=fname1,err=999,iostat=IERR)
       
*************  Read file list.txt into array ********************
c       Note...first line of input file ignored.     
        read(10,1) dummy

       j = 1 
       p1=0

 70    read(10,*,err=70,end=80) matdatestn(j),matelev(j),matsize(j),
     & matcape(j),matmr(j),mattemp(j),mattemp3(j),matlr(j),matlr2(j),
     & matshr3(j),matshr(j),matkm9(j),matsrh(j),matship(j),matmodel(j) 
     

c 70    read(10,*,err=70,end=80) matdatestn(j),matsize(j),matmr(j),
c     & matcape(j),mattemp(j),matlr(j),matshr(j),matkm9(j),matshr3(j),
c     & matship(j),matmodel(j),matsrh(j)

	if(matsize(j).ge.2.0) then
	 sigm(j)=1
	  else
	 sigm(j)=0
	endif
	
c Calculate difference in shear magnitude between sounding and match 
	dif(j) = 1.94*abs(shr-matshr(j))
	  	
       j=j+1
       if(j.gt.maob)stop'Array size too small to read in data...99999'
       goto 70    
       
       
 80    close(10)

c count number of soundings      
      cnt = j - 1
c mixing ratio ranges (g/kg) - k1
c        ranmr= 2.0
c	ranmrt1= 2.0
c
c determine cape ranges based on cape magnitude (j/kg) - k2
c    
c       rancape = mucape*.30
c	
c        if(mucape.lt.500.) then
c         rancapet1= mucape*.50
c        elseif(mucape.ge.500.0.and.mucape.lt.2000.) then
c        rancapet1= mucape*.25
c        else
c        rancapet1= mucape*.20
c       endif	
c
c 700-500 mb lapse rate ranges (c/km)- k3
c        ranlr= 2.0
c        ranlrt1= 0.4
c       
c 500 mb temperature ranges (c) - k4
c        rantemp= 9
c        rantempt1= 1.5
c     
c 0-6 km shear ranges (m/s) - k6
c        ranshr= 12 
c        ranshrt1= 6 
c
c 0-9 shear ranges - k7
c        rankm9= 22	
c        rankm9t1= 15	
c
c 0-3 km shear ranges (m/s) - k8
c        ranshr3= 10    
c        ranshr3t1= 8    
c
c SRH shear ranges (m/s) - k9
c        ransrh = 100    
c        if(srh.lt.50) then
c	ransrht1 = 25
c	else
c        ransrht1= srh*0.5    
c	endif
*************************************************************	
c using passed sounding, check against all soundings j .
      
       sigcnt = 0
       nulcnt = 0
       matches = 0
       matches2 = 0
       avsize = 0	
       tier1cnt=0
       golf = 0
       tier1 = 0
       jh = 0
		totalSnd = cnt
      DO 99 j=1,cnt
      
c mixing ratio ranges (g/kg) - k1
        ranmr= 2.0
        ranmrt1= 2.0

c determine cape ranges based on cape magnitude (j/kg) - k2

        rancape = mucape*.30
        
        if(mucape.lt.500.) then
         rancapet1= mucape*.50
        elseif(mucape.ge.500.0.and.mucape.lt.2000.) then
        rancapet1= mucape*.25
        else
        rancapet1= mucape*.20
       endif    

c 700-500 mb lapse rate ranges (c/km)- k3
        ranlr= 2.0
        ranlrt1= 0.4

c 500 mb temperature ranges (c) - k4
        rantemp= 9
        rantempt1= 1.5

c 0-6 km shear ranges (m/s) - k6
        ranshr= 12
        ranshrt1= 6

c 0-9 shear ranges - k7
        rankm9= 22      
        rankm9t1= 15    

c 0-3 km shear ranges (m/s) - k8
        ranshr3= 10
        ranshr3t1= 8

c SRH shear ranges (m/s) - k9
c        ransrh = 100
        if(srh.lt.50) then
        ransrht1 = 25
        else
        ransrht1= srh*0.5
        endif

          
        mrmat(j) = 0
	capemat(j) = 0
	lrmat(j) = 0
	tempmat(j) = 0
	shrmat(j)= 0
	km9mat(j) = 0
	shr3mat(j) = 0
	srhmat(j) = 0
	
        if(mumr.ge.(matmr(j)-ranmr).and.
     &      mumr.le.(matmr(j)+ranmr)) mrmat(j)=1
        if(mucape.ge.(matcape(j)-rancape).and.
     &      mucape.le.(matcape(j)+rancape)) capemat(j)=1
        if(lr.ge.(matlr(j)-ranlr).and.
     &      lr.le.(matlr(j)+ranlr)) lrmat(j)=1
        if(temp.ge.(mattemp(j)-rantemp).and.
     &      temp.le.(mattemp(j)+rantemp)) tempmat(j)=1
        if(shr.ge.(matshr(j)-ranshr).and.
     &      shr.le.(matshr(j)+ranshr)) shrmat(j)=1
        if(km9.ge.(matkm9(j)-rankm9).and.
     &      km9.le.(matkm9(j)+rankm9)) km9mat(j)=1
        if(shr3.ge.(matshr3(j)-ranshr3).and.
     &      shr3.le.(matshr3(j)+ranshr3)) shr3mat(j)=1
     
****** Check if all 7 parameters are met, exclude datestn  *********
        if(mrmat(j).eq.1.and.capemat(j).eq.1.and.tempmat(j).eq.1.and.
     &  lrmat(j).eq.1.and.shrmat(j).eq.1.and.km9mat(j).eq.1.and.
     &  shr3mat(j).eq.1) then	
     
*** It's a match.....................     
         matches = matches + 1
************* Determine if majority of matches are correct category ***	
	 if(sigm(j).eq.1) sigcnt = sigcnt + 1.
         if(sigm(j).eq.0) nulcnt = nulcnt + 1.

********* Count Golfball Matches for each  sounding ********	
	 if(matsize(j).eq.1.75) golf = golf + 1
	         
        endif
	
********************  Reset Variable for Tier 1 matches **************	
        mrmat(j) = 0
	capemat(j) = 0
	lrmat(j) = 0
	tempmat(j) = 0
	shrmat(j)= 0
	km9mat(j) = 0
	shr3mat(j) = 0
        srhmat(j) = 0
********************************* TIER 1 *******************************
        if(mumr.ge.(matmr(j)-ranmrt1).and.
     &      mumr.le.(matmr(j)+ranmrt1)) mrmat(j)=1
        if(mucape.ge.(matcape(j)-rancapet1).and.
     &      mucape.le.(matcape(j)+rancapet1)) capemat(j)=1
        if(lr.ge.(matlr(j)-ranlrt1).and.
     &      lr.le.(matlr(j)+ranlrt1)) lrmat(j)=1
        if(temp.ge.(mattemp(j)-rantempt1).and.
     &      temp.le.(mattemp(j)+rantempt1)) tempmat(j)=1
        if(shr.ge.(matshr(j)-ranshrt1).and.
     &      shr.le.(matshr(j)+ranshrt1)) shrmat(j)=1
        if(km9.ge.(matkm9(j)-rankm9t1).and.
     &      km9.le.(matkm9(j)+rankm9t1)) km9mat(j)=1
        if(shr3.ge.(matshr3(j)-ranshr3t1).and.
     &      shr3.le.(matshr3(j)+ranshr3t1)) shr3mat(j)=1
        if(srh.ge.(matsrh(j)-ransrht1).and.
     &      srh.le.(matsrh(j)+ransrht1)) srhmat(j)=1

************** See if sounding matches ********************************     
        if(mrmat(j).eq.1.and.capemat(j).eq.1.and.tempmat(j).eq.1.and.
     &  lrmat(j).eq.1.and.shrmat(j).eq.1.and.km9mat(j).eq.1.and.
     &  shr3mat(j).eq.1.and.srhmat(j).eq.1) then	
cr         if(tier1.gt.0) GOTO 5
cr	 write(*,*)'--------------------------------------------------'
c5        write(*,8) matdatestn(j), matsize(j), 1.94*matshr(j)
6         format('** TOP MATCHES FOR  ',a,'***')	 
8         format(1X,a13,1X,f5.2,2X,'6km= ',f4.1,' kt')	 
         tier1=tier1 + 1

*** John, lets just list them all, at worst they go off the screen ***
         if (tier1 .lt. 15) then
             sndglist(tier1) = matdatestn(j)
             haillist(tier1) = matsize(j)
         endif
        endif

************************************************************************
************************************************************************
***  Run again, using new ranges, to find the average match size *******
************************************************************************
************************************************************************
c mixing ratio ranges (g/kg) - k1
        ranmr= 2.0
	
c determine cape ranges based on cape magnitude (j/kg) - k2
        rancape = mucape*.40     
	
c 700-500 mb lapse rate ranges (c/km)- k3
        ranlr= 1.5
        
c 500 mb temperature ranges (c) - k4
        rantemp= 7
        
c 0-6 km shear ranges (m/s) - k6
        ranshr= 9 
        
c 0-9 shear ranges - k7
        rankm9= 22	
        
c 0-3 km shear ranges (m/s) - k8
        ranshr3= 10            
	
	
	mrmat(j) = 0
	capemat(j) = 0
	lrmat(j) = 0
	tempmat(j) = 0
	shrmat(j)= 0
	km9mat(j) = 0
	shr3mat(j) = 0
	srhmat(j) = 0
	
        if(mumr.ge.(matmr(j)-ranmr).and.
     &      mumr.le.(matmr(j)+ranmr)) mrmat(j)=1
        if(mucape.ge.(matcape(j)-rancape).and.
     &      mucape.le.(matcape(j)+rancape)) capemat(j)=1
        if(lr.ge.(matlr(j)-ranlr).and.
     &      lr.le.(matlr(j)+ranlr)) lrmat(j)=1
        if(temp.ge.(mattemp(j)-rantemp).and.
     &      temp.le.(mattemp(j)+rantemp)) tempmat(j)=1
        if(shr.ge.(matshr(j)-ranshr).and.
     &      shr.le.(matshr(j)+ranshr)) shrmat(j)=1
        if(km9.ge.(matkm9(j)-rankm9).and.
     &      km9.le.(matkm9(j)+rankm9)) km9mat(j)=1
        if(shr3.ge.(matshr3(j)-ranshr3).and.
     &      shr3.le.(matshr3(j)+ranshr3)) shr3mat(j)=1
     
****** Check if all 7 parameters are met, exclude datestn  *********
        if(mrmat(j).eq.1.and.capemat(j).eq.1.and.tempmat(j).eq.1.and.
     &  lrmat(j).eq.1.and.shrmat(j).eq.1.and.km9mat(j).eq.1.and.
     &  shr3mat(j).eq.1) then	

*** It's a match.....................     
          matches2 = matches2 + 1
	          
***** For sndg (i), sum matching sizes through list j calc avg below ***
         avsize = avsize + matsize(j)

        endif
************************************************************************
************************************************************************

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
       
c        matches = sigcnt + nulcnt

C	print *, 'SARS Searching',cnt,' soundings, found',matches
      
        if(matches2.gt.0) then
c Calculate average hail size from matches...
         avsize = avsize/matches2
c         write(*,*) 'Averaging ',matches2, ' matches.'
c 	 write(*,*) 'SARS Best Guess Size:  ',avsize
        endif

       if(matches.gt.0) then
        p1 = sigcnt/matches*100
	p2 = nulcnt/matches*100	
	 	 
        if(sigcnt.gt.nulcnt) then
	
c	  write(*,10) p1
10        format(' SARS Says:',f5.1,'% SIG')	  
	 elseif(sigcnt.lt.nulcnt) then 
c          write(*,11) p2
11        format(' SARS Says:',f5.1,'% SMALL')	  
	 elseif(sigcnt.eq.nulcnt) then
	  if(golf.gt.nulcnt/2) then
c  	   write(*,10) p1
	   else
c	   write(*,*)' SARS Says: Inconclusive'	 
          endif       
	endif        
       
       else
        
c        write(*,*) ' No Matches.'
c        write(*,*) ' '
       
       endif
       
c        write(*,*)'----------------------------------------------------'
c	write(*,*)'To overlay matching soundings, browse to'
c	write(*,*)' ~jewell/hail/snd/*'    
c	write(*,*) ''     

    	return(0)



999	matches = 0	
	p1 = 0
	tier1 = 0
	print *, "ERROR - SARS input file not found.  Aborting..."
       end 
