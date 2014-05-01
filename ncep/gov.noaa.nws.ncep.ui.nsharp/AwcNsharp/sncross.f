	SUBROUTINE SNCROSS (snfile, dattim, yaxis, cxstns )
C************************************************************************
C* PROGRAM SNCROSS							*
C*									*
C* This program draws cross sections using upper air data.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	12/85						*
C* I. Graffman/RDS	 5/86	Added observed wind drawing		*
C* I. Graffman/RDS	 5/88	GG call, DEVICE, GFLUSH to GEPLOT	*
C* G. Huffman/GSC	11/88	GEMPAK4.1; modify parameter names	*
C* M. desJardins/GSFC	 8/89	Add IP_IDNT				*
C* M. desJardins/GSFC	 1/90	Change thtinc to thtint in SNSGTH	*
C* S. Schotz/GSC	 6/90   Updates and cleanup for GEMPAK5		*
C* S. Schotz/GSC	 7/90	Added changes for IN_PTYP, IN_AXIS	*
C* S. Schotz/GSC	 8/90	Plot winds only at wind data levels	*
C* M. desJardins/GSFC	 8/90	Changed array size; plot any parameter	*
C* J. Nielsen/SUNYA	 2/91	Read data for new set of stations	*
C* J. Whistler/SSAI	 2/91	Added subroutine SNSWWE			*
C* M. desJardins/GSFC	 3/91	Reorganized x axis for time section	*
C* M. desJardins/NMC	 4/91	Change time section title		*
C* J. Nielsen/TAMU	11/91	Added filter factor			*
C* K. Brill/NMC		11/91	Changed PANEL*24 to *48			*
C* K. Brill/NMC		12/91	Call SNSWWE only if PROCES is true	*
C* K. Brill/NMC		01/92	Changes for contour fill		*
C* K. Brill/NMC         01/92   Replace GERROR with ER_WMSG             *
C* K. Brill/NMC		02/92	Check for new vertical coord; add parm	*
C*				conditions				*
C* S. Jacobs/EAI        11/92   Added call to GMESG and 'shrttl'        *
C* S. Jacobs/EAI	 9/93	Added CLRBAR, IN_CBAR and GG_CBAR	*
C* S. Jacobs/EAI	 9/93	Changed IN_CBAR and GG_CBAR to GG_CBAR	*
C* S. Jacobs/NMC         4/94   Removed unused variables        	*
C* S. Jacobs/NMC	 6/94	DEVICE*12 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNSUPD and added shrttl *
C*				to the user input variables		*
C* S. Jacobs/NMC	 9/94	Moved the title plotting to the end	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC	 	 8/96	Added ER_WMSG call after FL_MFIL call	*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* K. Brill/EMC		 4/99   Set MAXD grid dimension in PARAMETER	*
C* M. Li/GSC		 1/00	Added GCNTLN and nflag; removed GCSPLN	*
C* T. Lee/GSC		 7/00	Moved MAXD to GEMPRM.PRM & named LLMAXD	*
C* T. Lee/GSC		 8/00	Added calls to GSTANM and GENANM	*
C* T. Lee/SAIC		10/01	Added contour fill types		*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized icvtyp & isnfln	*
C* L. Hinson/AWC         3/03   Defined/init iend and ierr              *
C* L. Hinson/AWC         4/03   Numerous Declarations/Initializations   *
C*                              Added.  Logic/Miscoding Fix in call to  *
C*                              SNSWND, with undefined nlvl.  Passed    *
C*                              defined nvlvl set up by SNSVNT instead  *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER	snfile*(*), dattim*(*), yaxis*(*), cxstns*(*)
	CHARACTER	device*(LLMXLN),
     +			border*(LLMXLN), cint*(LLMXLN), line*(LLMXLN),
     +			ptype*(LLMXLN), title*(LLMXLN), wind*(LLMXLN), 
     +			taxis*(LLMXLN), filter*(LLMXLN), shrttl*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), snparm*(LLMXLN),
     +			vcoord*(LLMXLN),
     +			curve*(LLMXLN), contur*(LLMXLN),
     +			fint*(LLMXLN), fline*(LLMXLN), ctype*(LLMXLN),
     +			clrbar*(LLMXLN), filnam*(LLMXLN),
     +			vline*(LLMXLN), vint*(LLMXLN)
C*
	LOGICAL		clear
C
C*	The "old" values keep track of the current values.  NEWSTN
C*	is set when station data must be read in.  NEWPRM is set when
C*	a new parameter is selected.  NEWTHA is read when new theta
C*	data for an isentropic display must be read.
C
	CHARACTER	cxsold*72, snfold*72, datold*48, prmold*4,
     +			taxold*48, prmhld*16
	LOGICAL		newstn, newprm, newtha, newvco
C*
	CHARACTER	times (LLMXTM)*20, stns (LLTMCX)*20, parm*4
	CHARACTER	wintyp*1, winuni*1, vcord*4
	CHARACTER	ctlbl (LLMXTM)*12, ttlstr*72
	PARAMETER	( MSDSIZ = 100000 )
	REAL		stndat ( MSDSIZ ), xtlbl (LLMXTM)
	REAL		sloc (LLTMCX), xx (LLMAXD), yy (LLMAXD), 
     +			pontha (LLMAXD,200), clvl (LLCLEV),
     +			flvl (LLCLEV), pdat (LLTMCX,200),
     +			rmargn (4), grid (LLMAXD,LLMAXD),
     +			toptmp (LLTMCX), topwnd (LLTMCX),
     +			yaxval (LLAXIS),vlvl(LLCLEV)
	INTEGER		iltype (LLCLEV), linwid (LLCLEV), 
     +			ilabel (LLCLEV), icolor (LLCLEV),
     +			ifcolr (LLCLEV), iflabl (LLCLEV),
     +			ifltyp (LLCLEV), ipsdat (LLTMCX), 
     +			nlvls (LLTMCX), idtype (LLMXLV,LLTMCX), 
     +                  ivcolr(LLCLEV)
	LOGICAL		proces, respnd, done, isnflg, timflg,prmexs,
     +			wndexs, cflag, lflag, sflag, bflag, fflag, nflag,
     +			vflag
        INTEGER         iend, ier, iret, ivcord
	INTEGER         nstn, nyval, iylbfr, iyglfr, iytmfr, nxlbl, 
     +                  ixlbfr, ixglfr, ixtmfr, nvlvl, iwnclr, kgxy,
     +                  iytype, nclvl,nflvl, ithinc, icttl, linttl
	REAL            ybot, ytop, xmin, xmax
C*
	DATA		snfold, cxsold, prmold, datold, taxold
     +					  / 5 * ' ' /
	DATA		newstn, newprm, newvco  / 3 * .true. /
	DATA		icvtyp / 0 /, isnfln / 0 /, ivcold / -9999 /
C------------------------------------------------------------------------
C*	Initialize TAE and GEMPLT
C
            vflag=.false.
	    nflag=.false.
	    lflag=.false.
	    sflag=.false.
	    bflag=.false.
	    cflag=.false.
	    kgxy = 0
	    nvlvl = 0
	    nclvl = 0
	    nflvl = 0
            nstn = 0
            iend = 0
	    ier = 0
	    iret = 0
	    icttl = 0
	    linttl = 0
	    fflag = .false.
	    isnflg = .false.
	    newtha = .false.
	    timflg = .false.
	    ybot = 0.0
	    ytop = 0.0
	    xmin = 0.0
	    xmax = 0.0
	    nyval = 0
	    iylbfr = 0
	    iyglfr = 0
	    iytmfr = 0
	    nxlbl = 0
	    ixlbfr = 0
	    ixglfr = 0
	    ixtmfr = 0
	    iwnclr = 0
	    
	    snfold = '     '
            cxsold = '     '
            prmold = '     '
            datold = '     '
            taxold = '     '
            newstn = .true.
            newprm = .true.
            newvco = .true.
            ivcold = -9999
	    CALL IN_BDTA ( ier )
	    CALL GSMODE ( 2, ier )
	    snparm = ' ' 
	    vcoord = 'hght'
	    ptype  = 'lin//12;5;6;3'
	    taxis  = 'r--1;2;1;1'
	    line   = '0'
	    border = '1;1;8'
	    cint   = '2'
	    wind   = 'bk1/1/2/112'
	    title  = '1/-3'
	    panel  = '0'
	    clear  = .true.
	    filter = 'yes'
	    text   = '1/22//hw'
	    curve  = '2'
	    clrbar = '0!1/v/lr/.99;.1//-1'
	    fint   = '0;2;4;6;8;10'
	    contur = '0'
	    fline  = '0'
	    vint   = '20/0/180'
	    vline  = '0;31;11;2;17;20;23;21;24;27;29'
	    ctype  = 'l'
	    proces = .true.
	    prmexs = .true.
	    wndexs = .true.
	    
	    
C
C*		Set up the graphics device.
C
C		CALL GG_SDEV  ( device, iret )
C		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Set text attributes
C
		CALL IN_TEXT  ( text, iret )
C
C*		Open the sounding file.
C
		CALL FL_MFIL ( snfile, ' ', filnam, ier )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SNSFIL  ( filnam, snfold, isnfln, newstn, nparms,
     +			       iret )
		IF  ( iret .ne. 0 )  THEN
		    proces  = .false.
		  ELSE
		    CALL SNSDAT  ( isnfln, dattim, datold, newstn,
     +				   times, ntime, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		      ELSE IF  ( ntime .eq. 1 )  THEN
			timflg = .false.
		      ELSE IF  ( ntime .ge. 4 )  THEN
			timflg = .true.
			IF  ( ntime .gt. LLTMCX )  THEN
			    ntime = LLTMCX 
			END IF
		      ELSE
			iret  = -6
			CALL ER_WMSG  ( 'SNCROSS', iret, dattim, ier )
			proces = .false.
			datold = ' '
		    END IF
		END IF
C
C*		Check to see if the station(s) have changed.
C
		IF (proces) THEN
		CALL ST_LCUC  ( cxstns, cxstns, ier )
		IF  ( cxsold .ne. cxstns )  THEN
		    newstn = .true.
		    cxsold = ' '
		END IF
C
C*		Get wind information.
C
		CALL IN_WIND  ( wind, wintyp, winuni, iwnclr, ier )
C
C*		Get the parameter to evaluate.
C
		CALL ST_CLST  ( snparm, ';', ' ', 1, prmhld, n, ier )
		parm = prmhld (1:4)
		isnflg=.false.
		CALL ST_LCUC  ( parm, parm, ier )
		IF  ( ( parm .eq. ' ' ) .and. ( iwnclr .eq. 0 ) )  THEN
		    iret = -13
		    CALL ER_WMSG  ( 'SNCROSS', iret, snparm, ier )
		    proces = .false.
		    newprm = .true.
		  ELSE IF  ( parm .ne. prmold )  THEN
		    newprm = .true.
		    IF  ( parm .eq. 'ISEN' )  THEN
			isnflg = .true.
		      ELSE
			isnflg = .false.
		    END IF
		END IF
C
C*		Check to see if the time axis has changed for a time
C*		series.
C
		IF  ( taxis .ne. taxold )  newstn = .true.
		taxold = taxis
C
C*		Get the data for the selected stations or times.
C
		iret = 0
		END IF
		IF  ( proces .and. ( .not. timflg ) .and. newstn )
     +							    THEN
		    CALL SNSSST ( cxstns, isnfln, nparms, MSDSIZ, 
     +				  nstn, stns, ipsdat, nlvls, stndat, 
     +				  idtype, sloc, xmin, xmax, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		  ELSE IF  ( proces .and. timflg .and. newstn )  THEN
		    CALL SNSSTM ( cxstns, isnfln, nparms, MSDSIZ, times,
     +				  ntime, taxis, nstn, ipsdat, nlvls,
     +				  stndat, idtype, sloc, xmin, xmax,
     +				  xtlbl, ctlbl, nxlbl, ixlbfr, ixglfr,
     +				  ixtmfr, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
		CALL SN_CLOS( isnfln, ier)
C
C*		Check for at least four stations.
C
		IF  ( proces .and. ( nstn .ge. 4 ) )  THEN
		    cxsold = cxstns
		    prmold = parm
		  ELSE
		    cxsold = ' '
		    prmold = ' '
		    proces = .false.
		END IF
C
C*		Get information about the y axes.  Find the y points
C*		on a LLMAXD by LLMAXD grid.
C
		IF  ( proces ) THEN
		    CALL SNSYAX  ( ptype, vcoord, yaxis, iytype, ratio,
     +				   rmargn, ybot, ytop, ivcord, vcord,
     +				   yaxval, nyval, iylbfr, iyglfr, 
     +				   iytmfr, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		      ELSE IF  ( isnflg .and. ( ivcord .ne. 1 ) )  THEN
			iret = -4
			CALL ER_WMSG  ( 'SNCROSS', iret, ' ', ier )
			proces = .false.
		      ELSE
			IF ( ivcold .eq. ivcord ) newvco = .false.
		        ivcold = ivcord
			CALL SNSGRD  ( iytype, xmin, xmax, ybot, ytop, 
     +				       ratio, rmargn, yy, xx, iret )
			IF  ( iret .ne. 0 ) proces = .false.
		    END IF
		END IF
C
C*		Get the minimum pressure for temperature and winds.
C
		IF  ( proces )  THEN
		    CALL SNSRGE  ( vcord, nstn, stndat, nlvls, ipsdat, 
     +				   toptmp, topwnd, iret )
C
C*		    Get interval and curve type.
C
		    CALL SNSGTH  ( cint, curve, ithinc, icvtyp, newtha,
     +				   iret )
		END IF
C
C*		Get either grid or isentropic data.
C
		IF  ( proces )  THEN
		    IF  ( isnflg .and. 
     +			( newprm .or. newtha .or. newstn ) )  THEN
			CALL SNSTHA  ( ithinc, icvtyp, nstn, stns, 
     +				       ipsdat, nlvls, stndat, sloc, 
     +				       xx, thmin, thmax, pontha, 
     +				       pdat, iret )
			IF  ( iret .eq. 0 )  THEN
			    newprm = .false.
			  ELSE
			    prmexs = .false.
			END IF
		      ELSE IF ( ( .not. isnflg ) .and. 
     +			      ( newprm .or. newstn .or. newvco ) ) THEN
			CALL SNSPRM ( snparm, nstn, stns, ipsdat, nlvls,
     +				      stndat, sloc, xx, yy, vcord,
     +				      ivcord, icvtyp, grid, pontha,
     +				      iret )
			IF  ( iret .eq. 0 .and. parm .ne. ' ' )  THEN
			    CALL IN_CONT ( contur, ier )
			    CALL IN_CTYP ( ctype, nflag, lflag, sflag,
     +					   bflag, fflag, ier )
			    IF  ( lflag .or. sflag .or.
     +				  bflag .or. nflag )  THEN
				cflag = .true.
			    ELSE
				cflag = .false.
			    END IF
			    kgxy = LLMAXD * LLMAXD
			    CALL SNSLEV ( cflag, line, cint, fflag,
     +					  fline, fint, kgxy, grid,
     +					  nclvl, clvl, icolor, iltype,
     +					  linwid, ilabel, nflvl, flvl,
     +					  ifcolr, iflabl, ifltyp, ier )
                	    IF ( nclvl .eq. 0) cflag = .false.
                	    IF ( nflvl .eq. 0) fflag = .false.
			END IF
			IF  ( iret .ne. 0 )  THEN
			    prmexs = .false.
			  ELSE
			    CALL GSGGRF  ( 1, iytype, LLMAXD, LLMAXD,
     +					   xmin, ybot, xmax, ytop, ier )
			    IF  ( ier .ne. 0 )  THEN
				iret = -15
				CALL ER_WMSG ('SNCROSS', iret, ' ', ier)
				proces = .false.
			    END IF
			END IF
		    END IF
		END IF
C
C*		Check to see if wind data exist.		
C
	       IF ( proces ) THEN
	           CALL SNSWWE  ( winuni, vcord, ier )
	           IF  ( ier .ne. 0 )  THEN
		       wndexs = .false.
	           END IF
	       END IF
C
C
C*		Draw the cross section.
C
		IF  ( proces .and. ( prmexs .or. wndexs ) )  THEN
C
C*		    Clear the device if requested and set panel.
C
		    IF  ( clear ) CALL GCLEAR ( iret )
		    CALL GG_PANL ( panel, iret )
C
C*		    Draw the isentropes or contours.
C
		    IF ( prmexs )  THEN
		        IF  ( isnflg )  THEN
			    CALL SNSISN ( line, xx, pontha, ybot, ytop, 
     +			    	          ithinc, thmin, thmax, nstn, 
     +				          sloc, pdat, iret )
		          ELSE IF  ( parm .ne. ' ' .and.
     +				     ( cflag .or. fflag ) )  THEN
                            IF  ( fflag )  THEN
                    	      CALL GCFILL ( LLMAXD, LLMAXD, grid,
     +					    0, 0, 0, nflvl, flvl,
     +					    ifcolr, iflabl, ifltyp,
     +					    iret )
                    	      IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
                  	    END IF
                  	    IF  ( cflag )  THEN
                   	      IF  ( lflag )  THEN
                    		CALL GCLGRN ( LLMAXD, LLMAXD, grid,
     +					      0, 0, 0, nclvl, clvl,
     +					      icolor, iltype, linwid,
     +					      ilabel, iret )
                    		IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
                   	      END IF
                   	      IF  ( nflag )  THEN
                    		CALL GCNTLN ( LLMAXD, LLMAXD, grid,
     +					      0, 0, 0, nclvl, clvl,
     +					      icolor, iltype, linwid,
     +					      ilabel, iret )
                    		IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
                   	      END IF
			      IF  ( sflag )  THEN
                    		CALL GCSPLN ( LLMAXD, LLMAXD, grid, 0, 0, 0,
     +					      nclvl, clvl, icolor,
     +					      iltype, linwid, ilabel,
     +					      iret )
                     	        IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )       
                              END IF
                   	      IF  ( bflag )  THEN
                    		CALL GCBOXX ( LLMAXD, LLMAXD, grid,
     +					      0, 0, 0, nclvl, clvl,
     +					      icolor, iltype, linwid,
     +					      ilabel, iret )
                    		IF ( iret .ne. 0 ) CALL ER_WMSG
     +					    ( 'GEMPLT', iret, ' ', ier )
			      END IF
			    END IF
		        END IF
		    END IF
C
C*		    Plot the background and surface.
C
		    CALL SNSBOR  ( border, ybot, ytop, yaxval, nyval, 
     +				   iylbfr, iyglfr, iytmfr, xmin, xmax, 
     +				   nstn, stns, sloc, xx, pontha, 
     +				   toptmp, topwnd, xtlbl, ctlbl,
     +				   nxlbl, ixlbfr, ixglfr, ixtmfr, 
     +				   timflg, iret )
C
C*		    Draw the winds.
C
		    IF ( wndexs )  THEN
		    
			IF ( vint .ne. ' ' ) THEN
			    CALL SNSVNT ( vint, vline, vflag, vlvl, 
     +					  nvlvl, ivcolr, ier ) 
			ELSE
			    vflag = .false.
			END IF
C*
		        IF  ( iwnclr .gt. 0 .or. vflag)  THEN
			    CALL GSCOLR  ( iwnclr, ier )
			    CALL IN_FILT ( filter, filtfc, ier )
			    CALL SNSWND  ( wintyp, nstn, ipsdat, 
     +				           nlvls, stndat, idtype, sloc, 
     +				           ybot, ytop, filtfc, vcord, 
     +					   nvlvl, vlvl, ivcolr, vflag,
     +				           iret )
		        END IF
		    END IF
C
C*		    Plot the color bar.

		    ipoint = INDEX ( clrbar, '!' )
		    IF ( fflag )  THEN
			IF ( ipoint .gt. 0 ) THEN
			  iend = ipoint - 1
			ELSE 
			  CALL ST_LSTR ( clrbar, iend, ier )
		  	END IF
			CALL GG_CBAR ( clrbar(:iend), nflvl, flvl,
     +				       ifcolr, ier )
			IF ( vflag .and. ipoint .gt. 0 ) THEN
			    CALL GG_CBAR ( clrbar(ipoint+1:), nvlvl, 
     +					   vlvl, ivcolr, ier )

			END IF
		    ELSE IF ( vflag ) THEN
			CALL GG_CBAR ( clrbar, nvlvl, vlvl,
     +				       ivcolr, ier )
		    END IF
C
C
C*		    Write the title.
C
		    CALL SNSTTL (title, timflg, cxstns, times, ntime,
     +				 icttl, linttl, ttlstr, shrttl, iret)
		    IF  ( clear )  CALL GMESG ( shrttl, ier )
		    IF  ( icttl .gt. 0 )  THEN
			CALL GSCOLR  ( icttl, ier )
			CALL GG_WSTR ( ttlstr, linttl, ier )
		    END IF
	            title  = '1/-1/Height in Feet'
		    CALL SNSTTL (title, timflg, cxstns, times, ntime,
     +				 icttl, linttl, ttlstr, shrttl, iret)
		    IF  ( icttl .gt. 0 )  THEN
			CALL GSCOLR  ( icttl, ier )
			CALL GG_WSTR ( ttlstr, linttl, ier )
		    END IF
C
C
C*		    Flush the plotting buffers and update globals.
C
		    CALL GEPLOT ( iret )
		END IF
C
C*		Prompt for next cross section to be drawn.
C
C*
	RETURN
	END
