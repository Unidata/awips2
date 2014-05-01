       program sig_nsharp
       
c SARS For Supercells

       parameter (maob=15000) ! Max number of raobs allowed.
       
       real mlcape,mlcin,mllcl,shr,srh,stp,temp,ddd,lr,h500u,h500v,
     &matmlmr(maob),matmlcape(maob),matmlcin(maob),matmllcl(maob),
     &matshr(maob),matsrh(maob),matstp(maob),mattemp(maob),matddd(maob),
     &matlr(maob),math500u(maob),math500v(maob),ranmlmr,ranmlcape,
     &ranmlcin,ranmllcl,ranshr,ransrh,rantemp,ranlr,ranh500u,
     &ranh500v,ranstp,sigcnt,noncnt,matches,p1,p2,matmustp(maob),
     &matmucape(maob),matsblcl(maob)
       
       character datestn(maob)*16,dummy*25,matdatestn(maob)*16
       
       integer maob,i,j,cnt,mlmrmat(maob),mlcapemat(maob),
     & mlcinmat(maob),mllclmat(maob),mucapemat(maob),sblclmat(maob),
     & shrmat(maob),srhmat(maob),tempmat(maob),lrmat(maob),stpmat(maob),
     & h500umat(maob),h500vmat(maob),shrcat(maob),matshrcat(maob),
     & matcat(maob),sig,num,siggy
       
        print *, '****************************************************'
	print *, '     Entering SARS -"SIGTOR" fortran subroutine '
        print *, '****************************************************'


1     format(a)

       open(unit=10,status='old', file='siglist.txt')
c       open(unit=11,status='unknown', file='sup_output.txt')      
c       open(unit=12,status='unknown', file='sup_wrong.txt')      
       

*************  Read file list.txt into second array ********************
c      Note...first line of input file ignored.     
       read(10,1) dummy

       mlcape = 1500
       mllcl = 900
       temp = -11.2  #500mb temp
       lr = 7.0      #7-5 LR
       shr = 40.1    #0-6km shear kts
       srh = 100     #1km SRH
       h500u = 20.0  #knots
       h500v = 10.0
       stp = 3.5     #sigtor ML no cinh     

       j = 1 
 70    read(10,*,err=70,end=80) matdatestn(j),matcat(j),matmlmr(j),
     &matmlcape(j),matmlcin(j),matmllcl(j),matmucape(j),matsblcl(j),
     &matshr(j),matsrh(j),matstp(j),matmustp(j),mattemp(j),matddd(j),
     &matlr(j),math500u(j),math500v(j)

c         matsrh(j) = abs(matsrh(j))
	 
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
c mlcape ranges 
        ranmlcape= 1300

c mllcl ranges 
        ranmllcl = 50

c 0-6 km SHEAR
       ranshr = 14
	
c 0-1 km srh ranges (m2/s2)  
       if(abs(srh).lt.50) then
       ransrh = 100
       else
       ransrh = srh
       endif
       
c 500 mb temperature ranges (c) 
       rantemp= 6

c 700-500 mb lapse rate ranges (c/km)
        ranlr = 1.0

c 500 U and V components (kt) 
       ranh500u= 24
       ranh500v= 22

c SIG TOR PARAMETER

       ranstp = 2.5
            
*************************************************************	

c using sounding i , check against all soundings j . 
     
       tier1 = 0
       tier1cnt = 0
       matches = 0
       sigcnt = 0
       noncnt = 0
       sig = 0
       num=0
       p1=0
       p2=0
       siggy = 0
      
      DO 99 j=1,cnt
         
	mlcapemat(j) = 0
	mlcinmat(j) = 0
	mllclmat(j) = 0
	shrmat(j) = 0
	srhmat(j) = 0
	tempmat(j) = 0
	lrmat(j) = 0
        h500umat(j) = 0
        h500vmat(j) = 0
	stpmat(j) = 0
	
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
        if(h500u.ge.(math500u(j)-ranh500u).and.
     &      h500u.le.(math500u(j)+ranh500u)) h500umat(j)=1
        if(h500v.ge.(math500v(j)-ranh500v).and.
     &      h500v.le.(math500v(j)+ranh500v)) h500vmat(j)=1
        if(stp.ge.(matstp(j)-ranstp).and.
     &      stp.le.(matstp(j)+ranstp)) stpmat(j)=1
 
    
****** Check if all 6 parameters are met, exclude datestn (i) *********
      if(mlcapemat(j).eq.1.and.mllclmat(j).eq.1.and.shrmat(j).eq.1.and.
     & srhmat(j).eq.1.and.tempmat(j).eq.1.and.lrmat(j).eq.1.and.
     & h500umat(j).eq.1.and.h500vmat(j).eq.1.and.stpmat(j).eq.1) then	

************* Determine if majority of matches are correct category ***	
      if(matcat(j).eq.2) sigcnt = sigcnt + 1.
      if(matcat(j).eq.0) noncnt = noncnt + 1.       
      endif

 	
  99    continue
		
        matches = sigcnt + noncnt
        sig = sigcnt
	num = matches

        if(sig.eq.1) then
	print *, 'SARS Searching',cnt,' soundings, found',num,
     & ',',sig,' is SIGTOR.'
	elseif (sig.eq.0) then
	print *, 'SARS Searching',cnt,' soundings, found',num,
     & ', no SIGTOR.'
	else
	print *, 'SARS Searching',cnt,' soundings, found',num,
     & ',',sig,' are SIGTOR.'
        endif
	
       if(matches.ne.0) then
        p1 = sigcnt/matches*100
	p2 = noncnt/matches*100
	  
        if(sigcnt.gt.noncnt) then
	  write(*,10) p1
	  siggy = 2
10        format(' SARS Says: ',f5.1,'% SIG TORNADO !!!')	  
	 elseif(sigcnt.lt.noncnt) then 
          write(*,11) p2
          siggy = 1
11        format(' SARS Says: ',f5.1,'% NON TORNADIC SUPERCELLS')	  
	 elseif(sigcnt.eq.noncnt) then
	   write(*,*)' SARS Says: COULD BE SIG TORNADIC'	 
	endif        
       else
        write(*,*) ' No Matches.'
       endif
        write(*,*) 'SWITCH= ',siggy
       
        write(*,*)'----------------------------------------------------'
	write(*,*)'To overlay matching soundings, browse to'
	write (*,*)' /users/mead/pfcdir/'    
	write(*,*) ''     

       
       stop 'Calibration program complete'
       end      
