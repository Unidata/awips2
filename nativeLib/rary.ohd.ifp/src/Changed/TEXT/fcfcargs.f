C$PRAGMA C (getnonuniversaltechs)
C$PRAGMA C (gethomedirectory)
C$PRAGMA C (getfname)
C$PRAGMA C (chkdot)
C$PRAGMA C (chkdsh)
c
      Subroutine fcargs(MODCRD,NCARDS,MAXCRD,IER)
c
      INCLUDE 'common/ionum'     
      INCLUDE 'common/fcrunc'     
      INCLUDE 'common/fsnw'     
      INCLUDE 'common/fsacpr'     
      common /fsnwup/ iupwe, iupsc
      COMMON/OUTCTL/IOUTYP
      INCLUDE 'common/fdisps'
      INCLUDE 'common/fpltab'
      INCLUDE 'common/fengmt'
c
cew  updated to include the IOUTYP variable for the snow/sac states display

      Dimension MODCRD(20,1000), ModTemp(20)
      Dimension ibuf(20)
      Character*150 SEGCHR
      Character*100 homdir
      Character*20 FRfName
      Integer    istatus  
      Integer    ivalid_dot, ivalid_dash, ivalid, irflg,ivalcmnl
      Integer    ifirst_dash, ilstknt, idot_found
      Integer    jstatus  
      Integer      length,nread,nlast
      external integer  getfName
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Changed/RCS/fcfcargs.f,v $
     . $',                                                             '
     .$Id: fcfcargs.f,v 1.11 2004/12/02 19:35:41 dsa Exp $
     . $' /
C  =====================================================================
C
c
      ibug = IFBUG(4hFCAR)
c
      ier = 0
c

      Call GetNonuniversalTechs(NOSNOW, NOFRZE, iupsc, iupwe,
     1                          IPRSAC, IPRSNW, ICRTRO, IPRHY,
     2                          IOUTYP) 
     
cav       Call GetNonuniversalTechs(NOSNOW, NOFRZE, iupsc, iupwe,
cav     1                          IPRSAC, IPRSNW, ICRTRO, IPRHY)


c
c  fsnw and fsacpr
ccc      ibug = 1
      if(ibug.eq.1)Write(ipr,10)NOSNOW,NOFRZE
  10  Format("NOSNOW, NOFRZE = ",2i10)
c
c  don't know where upsc and upwe belong yet
      if(ibug.eq.1)Write(ipr,20)iupsc,iupwe
  20  Format("IUPSC, IUPWE = ",2i10)
c
c  write IPRSNW and IPRSAC just to see what there values are
      if(ibug.eq.1)Write(ipr,30)IPRSNW, IPRSAC
  30  Format("IPRSNW, IPRSAC = ",2i10)
C
C  modscb
C      MAXCRD
      NCARDS = 0
     
C      MODCRD(20,50)
c
c  see if there are any mod cards,
c   first look for FG and range mods brought over from the OFS
c
      CALL gethomedirectory(homdir, length)
c  get the file name for the FG/Range mods, if can't find goto 130
      jstatus = getfName(istatus, FRfName)
      if(istatus .eq. 0) goto 130
c
      WRITE(SEGCHR(:), 100)homdir
 100  FORMAT(a)
      WRITE(SEGCHR(length+1:), '(''/.ifp_files/mods_from_ofs/ '')')
      WRITE(SEGCHR(INDEX(SEGCHR,' '):),100)FRfName
 110  FORMAT(2a4)
C
      if(ibug.eq.1)WRITE(IPR,'(A)') SEGCHR
C
      IREAD = 17
      OPEN(UNIT=IREAD,FILE=SEGCHR,FORM='FORMATTED',
     1     STATUS='OLD',ERR=130)
C
      go to 150
 130  if(ibug.eq.1)WRITE(IPR,140) RUNID
 140  Format('No FG/Range mods from OFS for segment ',2a4)
      mods_ofs = 0

      go to 230
C
 150  if(ibug.eq.1)Write(ipr, 160)MAXCRD
 160  Format('MAXCRD = ',i5)
      if(ibug.eq.1)Write(ipr,'(''FG mods from OFS'')')

      ikount = 1
      nread = 1
      nlast = 0

      Do 210 i = 1, MAXCRD+1
      Read(IREAD, 170, END=220)ModTemp
      nread = nread + 1
 170  Format(20a4)
      if(i .le. MAXCRD) then
	 Do 180 j = 1,20
	    MODCRD(j,ikount) = ModTemp(j)
	    ibuf(j)          = ModTemp(j)
 180     Continue
C----    Check for commnand line CCCCC

 185     call chkdot(ivalid_dot,ibuf)

C-----   For the case that FGROUP is found  in a command line

         if ( ivalid_dot .eq. 2 ) then
	    idot_found = 1
	    Do while (idot_found .eq. 1) 
              Read(IREAD, 170, END=220)ModTemp
              ikount = ikount + 1
	      nread = nread + 1
              Do  187 j = 1,20
                  MODCRD(j,ikount) = ModTemp(j)
                  ibuf(j)          = ModTemp(j)
 187          Continue
              call chkdot(ivalid_dot,ibuf)
              if ( ivalid_dot .eq.1 ) then
                idot_found = 0
                go to 185
              end if
 	    End do 
         end if
C-----   For the case that command line does not have FGROUP

         ilstknt = ikount

         ifirst_dash = 0
         idot_found  = 1

         if ( ivalid_dot .eq. 1 ) then
            do while ( idot_found .eq. 1)
               Read(IREAD, 170, END=220)ModTemp
               nread = nread + 1

               ikount = ikount + 1

               Do 188  j = 1,20
                   MODCRD(j,ikount) = ModTemp(j)
                   ibuf(j)          = ModTemp(j)
 188           Continue
              call chkdot(ivalid_dot,ibuf)
              if ( ivalid_dot .eq. 1 ) then
                  go to 185
              end if
              call chkdsh(ivalid_dash,ibuf,RUNID)
              if ( ivalid_dash .eq.1 ) ifirst_dash = 1
                if( ivalid_dash .eq. 0 .and. ifirst_dash .eq. 0) then
                    idot_found = 0
                    ikount     = ilstknt
		    nlast = nread
                    
                end if
            end do
         end if

      else
	 Write(ipr,200)RUNID
 200     Format('*** Too many Mod cards for segment ',2a4,' ***')
	 go to 430
      endif
 210  Continue
c
 220  Rewind IREAD
      CLOSE(IREAD)

      if(nread .eq. nlast) ikount = ikount - 1
      if(ikount .eq. 1) ikount = 0
      if(ibug .eq. 1)then
        do 189 n = 1 , ikount
          Write(*, 190)n,(MODCRD(j,n), j = 1,20)
 189    continue
      end if 
 190  Format(i5,'---- ',20a4)

c
c  Now get segment mods from ofs
c

c      mods_ofs = i-1
      mods_ofs = ikount

c
 230  WRITE(SEGCHR(:), 100)homdir
      WRITE(SEGCHR(length+1:), '(''/.ifp_files/mods_from_ofs/ '')')
      WRITE(SEGCHR(INDEX(SEGCHR,' '):),110)RUNID
C
      if(ibug.eq.1)WRITE(IPR,'(A)') SEGCHR
C
      IREAD = 17
      OPEN(UNIT=IREAD,FILE=SEGCHR,FORM='FORMATTED',
     1     STATUS='OLD',ERR=240)
C
      go to 260
 240  if(ibug.eq.1)WRITE(IPR,250) RUNID
 250  Format('No mods from the OFS for segment ',2a4)
      i = 1
      nread = 1
      go to 300
C
 260  if(ibug.eq.1)Write(ipr,'(''Mods from OFS'')')
      Do 280 i = 1, MAXCRD
      Read(IREAD, 170, END=290)ModTemp
      if(mods_ofs + i .le. MAXCRD) then
	 Do 270 j = 1,20
	 MODCRD(j,mods_ofs + i) = ModTemp(j)
 270     Continue
	 if(ibug.eq.1)
     1   Write(ipr, 190)i,(MODCRD(j, mods_ofs + i), j = 1,20)
      else
	 Write(ipr,200)RUNID
	 go to 430
      endif
 280  Continue
c
 290  Rewind IREAD
      CLOSE(IREAD)
C
c  Get current FG mods
c
      mods_ofs = mods_ofs + (i-1)

c
 300  WRITE(SEGCHR(:), 100)homdir
      WRITE(SEGCHR(length+1:), '(''/.ifp_files/mods/ '')')
      WRITE(SEGCHR(INDEX(SEGCHR,' '):),100)FRfName
C
      if(ibug.eq.1)WRITE(IPR,'(A)') SEGCHR
C
      IREAD = 17
      OPEN(UNIT=IREAD,FILE=SEGCHR,FORM='FORMATTED',
     1     STATUS='OLD',ERR=310)
C
      go to 330
 310  if(ibug.eq.1)WRITE(IPR,320) RUNID
 320  Format('No current FG mods for segment ',2a4)
      i = 1
      nread = 1
      go to 370
C
 330  if(ibug.eq.1)Write(ipr,'(''Current FG mods'')')

      ikount=1
      nread = 1
      nlast = 0
      Do 350 i = 1, MAXCRD
      Read(IREAD, 170, END=360)ModTemp
      nread = nread + 1
      if(mods_ofs + i .le. MAXCRD) then
	 Do 340 j = 1,20
	    MODCRD(j,mods_ofs + ikount) = ModTemp(j)
            ibuf(j)                     = ModTemp(j)
 340     Continue
C----    Check for commnand line call C routine
 342     call chkdot(ivalcmnl,ibuf)

C-----   For the case that FGROUP is found  in a command line

         if ( ivalcmnl .eq. 2 ) then
            idot_found = 1
            do while ( idot_found .eq. 1)
              Read(IREAD, 170, END=360)ModTemp
              ikount = ikount + 1
	      nread = nread + 1
              Do  343 j = 1,20
                  MODCRD(j,mods_ofs +ikount) = ModTemp(j)
                  ibuf(j)                    = ModTemp(j)
 343          Continue
              call chkdot(ivalid_dot,ibuf)
              if ( ivalid_dot .eq.1 ) then
                idot_found = 0
                go to 342
              end if
            end do


         end if
C-----   For the case that command line does not have FGROUP

         ilstknt = ikount
         ifirst_dash = 0
         idot_found  = 1

         if ( ivalcmnl .eq. 1 ) then
            do while ( idot_found .eq. 1)

               Read(IREAD, 170, END=360)ModTemp
	       nread = nread + 1
               ikount = ikount + 1
               Do 346  j = 1,20
                   MODCRD(j,mods_ofs +ikount) = ModTemp(j)
                   ibuf(j)                    = ModTemp(j)
 346           Continue

              call chkdot(ivalcmnl,ibuf)
              if ( ivalcmnl .eq. 1 ) then
                  go to 342
              end if
C-------Call C routine
              call chkdsh(irflg,ibuf,RUNID)
              if ( irflg .eq. 1 ) ifirst_dash = 1
                if( irflg .eq. 0 .and. ifirst_dash .eq. 0) then
                    idot_found = 0
                    ikount     = ilstknt
		    nlast = nread
                end if

            end do
        end if
      else
	 Write(ipr,200)RUNID
	 go to 430
      endif
 350  Continue
c
 360  Rewind IREAD
      CLOSE(IREAD)
c--- last card is a failed card
      if (nread .eq. nlast)ikount = ikount - 1
      if( ikount .eq. 1)ikount = 0

C
c  Get current segment mods
c
c     mods_ofs = mods_ofs + (i-1)
      mods_ofs = mods_ofs + ikount

      if(ibug .eq. 1) then
        do 362 n = 1 , ikount
          Write(*, 191)n,(MODCRD(j,n), j = 1,20)
 362    continue
 191    Format(i5,'....',20a4)
      end if
c
 370  WRITE(SEGCHR(:), 100)homdir
      WRITE(SEGCHR(length+1:), '(''/.ifp_files/mods/ '')')
      WRITE(SEGCHR(INDEX(SEGCHR,' '):),110)RUNID
C
      if(ibug.eq.1)WRITE(IPR,'(A)') SEGCHR
C
      IREAD = 17
      OPEN(UNIT=IREAD,FILE=SEGCHR,FORM='FORMATTED',
     1     STATUS='OLD',ERR=380)
C
      go to 400
 380  if(ibug.eq.1)WRITE(IPR,390) RUNID
 390  Format('No current mods for segment ',2a4)
      nread = 1
      i = 1
      go to 450
C
 400  if(ibug.eq.1)Write(ipr,'(''Current mods'')')

      Do 420 i = 1, MAXCRD
      Read(IREAD, 170, END=430)ModTemp
      if(mods_ofs + i .le. MAXCRD) then
	 Do 410 j = 1,20
	 MODCRD(j,mods_ofs + i) = ModTemp(j)
 410     Continue
	 if(ibug.eq.1)
     1   Write(ipr, 190)i,(MODCRD(j, mods_ofs + i), j = 1,20)
      else
	 Write(ipr,200)RUNID
	 go to 430
      endif
 420  Continue
c
 430  Rewind IREAD
      CLOSE(IREAD)
      

cc 
 450  NCARDS = (i-1) + mods_ofs

      if(ibug.eq.1)Write(ipr,460)RUNID, NCARDS
 460  Format('Total number of mod cards for ',2a4,' is',i4)
c
      return
      end

