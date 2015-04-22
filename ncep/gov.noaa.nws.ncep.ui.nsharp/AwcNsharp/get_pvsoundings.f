        SUBROUTINE GET_PVSOUNDINGS ( snfile, dattim, cxstns, pvsndgblk, 
     +    pvnlvls, pvnstns, offset)
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE		'ERMISS.FNC'
	        
C*
	CHARACTER	snfile*(*), dattim*(*), cxstns*(*),
     +			border*(LLMXLN), cint*(LLMXLN), line*(LLMXLN),
     +			ptype*(LLMXLN), title*(LLMXLN), wind*(LLMXLN),
     +			taxis*(LLMXLN), filter*(LLMXLN),
     +			panel*(LLMXLN), text*(LLMXLN), snparm*(LLMXLN),
     +			vcoord*(LLMXLN),
     +			curve*(LLMXLN), contur*(LLMXLN),
     +			fint*(LLMXLN), fline*(LLMXLN), ctype*(LLMXLN),
     +			clrbar*(LLMXLN), vint*(LLMXLN), vline*(LLMXLN),
     +			filnam*(LLMXLN)
        REAL		pvsndgblk(*)
        INTEGER         pvnlvls(*)
        INTEGER		pvnstns
        INTEGER         offset, i, j, ivcord, nyval, iytmfr, isnfln,
     +                   nparms
	REAL		ratio, ybot, ytop
        
	LOGICAL		clear
C*	The "old" values keep track of the current values.  NEWSTN
C*	is set when station data must be read in.  NEWPRM is set when
C*	a new parameter is selected.  NEWTHA is read when new theta
C*	data for an isentropic display must be read.
C
	CHARACTER	cxsold*(LLMXLN), snfold*(LLMXLN), datold*48,
     +			prmold*4, taxold*48, prmhld*16
	LOGICAL		newstn, newprm, newvco
C*
	CHARACTER	times (LLMXTM)*20, stns (LLTMCX)*20, parm*4
	CHARACTER	wintyp*1, winuni*1, vcord*4
	PARAMETER	( MSDSIZ = 100000 )
	REAL		stndat ( MSDSIZ )
	REAL		sloc (LLTMCX), xx (40), yy (40), 
     +			pontha (40,200),
     +			rmargn (4), grid (40,40),
     +			toptmp (LLTMCX), topwnd (LLTMCX),
     +			yaxval (LLAXIS)
	INTEGER         ier, iret
	INTEGER		ipsdat (LLTMCX), nlvls (LLTMCX), 
     +			idtype (LLMXLV,LLTMCX)
	LOGICAL		proces, isnflg, timflg,prmexs,
     +			wndexs
     	CHARACTER	cdata (3)*8
	REAL		data (3), p (2000), d (2000), s (2000)
	CHARACTER       yaxis*50
	DATA		snfold, cxsold, prmold, datold, taxold / 5 * ' ' /
        DATA		newstn, newprm, newvco  / 3 * .true. /
	DATA		ivcold / -9999 /
        INTEGER         orgloc
	INTEGER		nstn, nlv, iytype, ntime

        ier=0
	iret=0
C*	    Set flag to indicate processing will be done.
C
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
	    border = '1'
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
	    yaxis  = "0/65000/1000/2;0;1"
C

C
C*		Open the sounding file.
C
		CALL FL_MFIL ( snfile, ' ', filnam, ier )
		
		CALL SNSFIL  ( snfile, snfold, isnfln, newstn, nparms,
     +			       iret )		
              	CALL SNSDAT  ( isnfln, dattim, datold, newstn,
     +				   times, ntime, iret )
                timflg = .false.

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
		isnflg = .false.
		CALL ST_LCUC  ( parm, parm, ier )
		taxold = taxis
                CALL SNSSST ( cxstns, isnfln, nparms, MSDSIZ, 
     +				  nstn, stns, ipsdat, nlvls, stndat, 
     +				  idtype, sloc, xmin, xmax, iret )
                CALL SN_CLOS ( isnfln, ier)
                cxsold = cxstns
                prmold = parm
                CALL SNSYAX  ( ptype, vcoord, yaxis, iytype, ratio,
     +				   rmargn, ybot, ytop, ivcord, vcord,
     +				   yaxval, nyval, iylbfr, iyglfr, 
     +				   iytmfr, iret )
     		ivcold = ivcord
		CALL SNSGRD  ( iytype, xmin, xmax, ybot, ytop, 
     +				       ratio, rmargn, yy, xx, iret )
     		CALL SNSRGE  ( vcord, nstn, stndat, nlvls, ipsdat, 
     +				   toptmp, topwnd, iret )
C
     
                CALL SNSPRM ( snparm, nstn, stns, ipsdat, nlvls,
     +				      stndat, sloc, xx, yy, vcord,
     +				      ivcord, icvtyp, grid, pontha,
     +				      iret )
     	        IF ( proces ) THEN
	           CALL SNSWWE  ( winuni, vcord, ier )
	           IF  ( ier .ne. 0 )  THEN
		       wndexs = .false.
	           END IF
	        END IF
                DO i = 1, nstn
                  nlv = 0
                  CALL PC_SSTN( '    ',0,0,0,0,0,IMISSD,nlvls (i),iret)
                  DO j = 1, nlvls(i)
                    CALL PC_CMLV(j,stndat (ipsdat (i)),data,cdata,iret)
                    data(1) = data(1) * 3.28
		    IF  ( ( .not. ERMISS (data (1)) ) .and.
     *		      ( .not. ERMISS (data (2)) ) .and.
     *		      ( .not. ERMISS (data (3)) ) .and.
     *                ( idtype ( j, i ) .ne. 2  ) .and. 
     *		      ( ( ( vcord .eq. 'PRES' )   .and.
     *		          ( data (1) .le. ybot )  .and.
     *			  ( data (1) .ge. ytop ) ) .or.
     *			( ( vcord .ne. 'PRES' )   .and.
     *			  ( data (1) .ge. ybot )  .and.
     *			  ( data (1) .le. ytop ) ) ) ) THEN
		    nlv = nlv + 1
		    p (nlv) = data (1)
		    d (nlv) = data (2)
		    s (nlv) = data (3)
 		    orgloc=(INDEX(cxstns,stns(i)(1:3))-1)/4+1
		    DO k = 1, 3
		      pvsndgblk((orgloc-1+offset)*300+(nlv-1)*3+k)=data(k)
		    END DO
		    
		   END IF
	          END DO
	          pvnlvls(orgloc-1+offset+1)=nlv
	        END DO
	        pvnstns=nstn
C
        RETURN
        END
     
