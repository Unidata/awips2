C$PRAGMA C (gethomedirectory)
C$PRAGMA C(getfgroupid)
C$PRAGMA C(searcht)
C$PRAGMA C(findt_next)
C  MODULE  ss_input.f
C
C  DESC  PROGRAM TO READ INFORMATION from binary files FOR
C        the forecast group selected by startifp/seed
C        IN A FORMAT WRITTEN BY fill_bin
C        AND create the input expected by the seg_sort program.
C
C  ORIGINALLY WRITTEN BY GEORGE SMITH, HRL, May 1990.
c
c  Changed by George Smith, November 1992 to read NWSRFS OFS
c   files in workstation directories.
C
C  Changed by D. Page, Aug. 93 to be a subroutine called from a 
C   C main program (main_ss_input.c) to get this to compile on
C   the HPs.
C
      Subroutine ss_input()
c
      EQUIVALENCE(RTCVID, XXRTCV)
C
      INCLUDE 'common/fccgd'
      INCLUDE 'common/fccgd1'
      INCLUDE 'common/fcfgs'
      INCLUDE 'common/fcsegn'
      INCLUDE 'common/fratng'
      INCLUDE 'common/fp'
      INCLUDE 'common/ft'
      INCLUDE 'common/fts'
      INCLUDE 'common/fc'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fcunit'
C
      CHARACTER*80 SEGCHR, HOME_directory
      CHARACTER*8  FGroup_ID
C
      DIMENSION IEMPTY(2)
      DIMENSION XXRTCV(300),seg_status(2)
C
      DATA IEMPTY/4HEMPT,4HY   /, IBLANK/4H    /
      DATA xnone/4hNone/, blank/4h    /
c
      Integer seq_num, flag, NumRCSum,RCIndex
      Integer, DIMENSION(100)::RCPosList
cc RCPosList = forecast point location in the p array, NumRCSum is length---kwz[4/29/02]
C
      LOGICAL NotFoundFlag
      integer         dir_chk_status
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/bin_to_ss_input/RCS/ss_input.f,v $
     . $',                                                             '
     .$Id: ss_input.f,v 1.4 2002/10/10 16:33:30 dws Exp $
     . $' /
C  =====================================================================
C
c
      I300 = 300
c
c ---------------------------------------------------------------------
c  Assign directory pathnames for NWSRFS OFS file assignments
c ---------------------------------------------------------------------
c
c
      call upinio()
      call upchkd('syst oper',dir_chk_status)

      if(dir_chk_status .ne. 0) then
	write(*, 775) dir_chk_status
 775    format("status returned from up_chk_dir = ", i5/
     1         "cannot continue with program")
	go to 900
      end if
c
      call rpdbci(istat)
c
      if(istat .ne. 0)write(ipr, 140)istat
 140  format("status from rpdbci = ", i4)
c
      Call faze0()
c
cckwz      ipr_outfile = 8
C
      CALL GetHomeDirectory(HOME_directory, LENGTH)
      IF(LENGTH.GT.0)
     1   THEN
      WRITE(SEGCHR(:),410)HOME_directory
 410  FORMAT(A)
cckwz      WRITE(SEGCHR(LENGTH+1:),
cckwz     1    '(''/.ifp_files/local/ss_inp.out'')')
	 ELSE
      WRITE(*,420)
 420  FORMAT('No HOME directory found.')
      GO TO 900
	 END IF
C
cckwz      OPEN (UNIT=ipr_outfile,FILE=SEGCHR,
cckwz     1      STATUS='UNKNOWN',ERR=991)
C
      IUSS_INPUT = 3
C
      WRITE(SEGCHR(LENGTH+1:),
     1    '(''/.ifp_files/local/seg_sort.in'')')
C
      OPEN (UNIT=IUSS_INPUT,FILE=SEGCHR,
     1      STATUS='UNKNOWN',ERR=991)
C
C  .........................................................
C  GET NAME OF FORECAST GROUP FROM X-WINDOW PROPERTY
C
      CALL GetFGroupID(FGroup_ID, ISTAT)
C
      IF(ISTAT.GT.0)GO TO 992
C
C  FORECAST GROUP INFO
C  .........................................................
C  READ FORECAST GROUP INFO FROM nwsrfs ofs files
C
 730  FORMAT(A)
C
cckwz      WRITE(ipr_outfile,'(A)') FGroup_ID
C
      flag = 1
      Call fgetfg(seq_num, FGroup_ID, flag)
C
cckwz      WRITE(ipr_outfile,868)seq_num, FGID, CGIDF, DESCR, NSEG, MINDTF
cckwz 868  FORMAT(1x,i5,2(1X,2A4)/1X,5A4/2(1X,I10))
C
      DO 100 ISEG = 1, NSEG
c
c  irec in cb /fcfgs/ is first record for segment info for this fg
c
      ix = irec + iseg - 1
c
      Call ureadt(KFFGL, IX, FGroup_ID, ISTAT)
c
cckwz      WRITE(ipr_outfile,867)FGroup_ID
cckwz 867  FORMAT(1X,A)
C
      IOPT = 0
      NOPARM = 0
c
      CALL FGETSG(FGroup_ID, IXXREC, MP, P, MT, T, MTS, TS,
     1           IOPT, NOPARM, ISTAT)
C  .........................................................
C
C  SEGMENT INFO
C  .........................................................
C
cckwz      WRITE(ipr_outfile,869)FGroup_ID,IDSEGN,IUPSEG,IDNSEG,IFGID,ICGID,
cckwz     1  SGDSCR,MINDT,XLAT,XLONG,NC,ND,NT,NTS,NP,NCOPS
C
cckwz 869  FORMAT(1X,A,1X,2A4/
cckwz     1  5X,5(1X,2A4)/10X,2(1X,2A4)/2(1X,2A4)/
cckwz     2  1X,5A4/1X,I10,2(1X,F8.3)/6(1X,I10))
C
C  Fill IUPSEG and IDNSEG with 'EMPTY' if no segment listed.
C  .........................................................
C
      DO 20 IUP = 1, 5
      IF(IUPSEG(1,IUP).NE.IBLANK .OR. IUPSEG(2,IUP).NE.IBLANK)
     1     GO TO 20
C  IUPSEG equals blanks if get here
      IUPSEG(1,IUP)=IEMPTY(1)
      IUPSEG(2,IUP)=IEMPTY(2)
 20   CONTINUE
C
      DO 30 IDN = 1, 2
      IF(IDNSEG(1,IDN).NE.IBLANK .OR. IDNSEG(2,IDN).NE.IBLANK)
     1     GO TO 30
C  IDNSEG equals blanks if get here
      IDNSEG(1,IDN)=IEMPTY(1)
      IDNSEG(2,IDN)=IEMPTY(2)
 30   CONTINUE
C  .........................................................
c
C  Write first three lines to seg_sort input file.
C
      WRITE(IUSS_INPUT,600)IDSEGN, FGID, CGIDC, ICRDTE,
     1     SGDSCR, IUPSEG, IDNSEG
 600  FORMAT(3(1X,2A4), 1X,5i4,1H~, 1X,5A4,1H~, / 3x, 5(1X,2A4) /
     1  3x, 2(1X,2A4))
C  .........................................................
C
C  Operations Table INFO
C  .........................................................
CC Search the T array for all forecast points and store it in the RCPosList
cc variable. eliminate duplicated forecast points---kwz[4/29/02]
      NumRCSum=0
      LOCT=1
      DO
        IF (T(LOCT).EQ.-1) EXIT
	IF (NumRCSum.EQ.100) EXIT
cc	CALL searcht(LOCT, 25, T, NT)
        if (T(LOCT).NE.23)then
	  loct=T(loct+1)
	else
	IF (T(LOCT+6).GT.0) THEN
	  NotFoundFlag=.TRUE.	  
	  DO RCIndex=1,NumRCSum,1
	    IF ((P(RCPosList(RCIndex)).EQ.P(T(LOCT+6) )).AND.
     1          (P(RCPosList(RCIndex)+1).EQ.P(T(LOCT+6)+1))) THEN
              NotFoundFlag=.FALSE.
            END IF
	  END DO
	  IF (NotFoundFlag) then
   	    NumRCSum=NumRCSum+1
	    RCPosList(NumRCSum)=T(LOCT+6)
	  END IF
	END IF
	LOCT=T(LOCT+1)
	end if
      END DO
     
      IF (NumRCSum.EQ.0) THEN
cckwz        WRITE(ipr_outfile,'(A)')'Rating curves not used in this segment'
CC Every segment must have >= 1 RC, if not use blank, xnone to substitude RC info
	NumRCSum=1
        NotFoundFlag=.TRUE.
      ELSE
        NotFoundFlag=.FALSE.
cckwz        WRITE(ipr_outfile,'(2A4)') P(RCPosList(1)),P(RCPosList(1)+1)
      END IF
C  .........................................................
C
C  Parametric INFO
C  .........................................................
C
C  Rating Curve INFO
C  .........................................................
C
c Write 4th line to seg_sort input file.
      WRITE(IUSS_INPUT,'(i3)')NumRCSum
      
      DO 101 RCIndex = 1, NumRCSum
     
      IF (NotFoundFlag) THEN
        Call set_rc_missing
	rating_limit = -999
      ELSE
C  .........................................................
C
C  Extract info needed for e19 display from rating curve file
C
        Call fgetrc(P(RCPosList(RCIndex)), ierr)
        if(loch.eq.0)
     1  then
	  rating_limit = -999.
        else
	  rating_limit = xrc(loch-1 + nrcpts)
        end if
      END IF
C  .........................................................
C
C  Write 5th through 9th lines to seg_sort input file.
C
      if(rfcomt(1) .eq. blank) then
	 rfcomt(1) = xnone
	 do 95 i = 2,5
 95      rfcomt(i) = blank
      end if
      if(fptype(1) .eq. blank .and.
     1   fptype(2) .eq. blank .and.
     2   fptype(3) .eq. blank .and.
     3   fptype(4) .eq. blank .and.
     4   fptype(5) .eq. blank)      then
	 fptype(1) = xnone
	 do 96 i = 2,5
 96      fptype(i) = blank
      end if
      if(rivern(1) .eq. blank) then
	 rivern(1) = xnone
	 do 97 i = 2,5
 97      rivern(i) = blank
      end if
      if(rivsta(1) .eq. blank) then
	 rivsta(1) = xnone
	 do 98 i = 2,5
 98      rivsta(i) = blank
      end if
c
c  for now set wrnq equal to 80% of floodq - eventually
c   will get from a value in the rating curve file
c
      wrnq = -999.
      if(floodq .gt. 0.0) wrnq = 0.8 * floodq
c
      IF (NotFoundFlag) THEN
      WRITE(IUSS_INPUT,601)'None',
     1  '    ',rivern,rivsta,rlat,rlong,
     2  (fptype(i), i=1,5), areat, areal, fldstg, floodq,
     3  scfstg, wrnstg, wrnq, gzero,
     4  rfstg, rfq, irfday, rfcomt,
     5  rating_limit
      ELSE
      WRITE(IUSS_INPUT,601)P(RCPosList(RCIndex)),
     1  P(RCPosList(RCIndex)+1),rivern,rivsta,rlat,rlong,
     2  (fptype(i), i=1,5), areat, areal, fldstg, floodq,
     3  scfstg, wrnstg, wrnq, gzero,
     4  rfstg, rfq, irfday, rfcomt,
     5  rating_limit
      END IF
 601  FORMAT(2a4, 2(1x,5a4, 1h~) / 2(1x,f7.2), 1x,5a4, 1h~ /
     1  2(1x,f8.0), 1x,f8.1, 1x,f12.0, 2(1x,f8.1), 1x,f12.0, 1x,f7.1 /
     2  1x,f10.1, 1x,f12.0, 1x,i10, 1x,5a4, 1h~ / 1x,f8.1)
C  .........................................................
C
C  Write 10th line to seg_sort input file.
C
c  Get status (Normal, Alert, Flood) of segment from
c   wrnq and floodq variables in rating file, and
c   from QINE time series for the segment.
c
      Call get_segment_status(FGroup_ID, wrnq, floodq, IREAD_TS,
     1                        seg_status)
c
      WRITE(IUSS_INPUT,602)seg_status
 602  Format(1x,2a4)
c
 101  CONTINUE
 100  CONTINUE
C
      GO TO 900
C  .........................................................
C
 990  WRITE(*,'(A)') 'Problem reading file'
cckwz      WRITE(ipr_outfile,'(A)') 'Problem reading file'
      GO TO 900
C
 991  WRITE(*,'(A)') 'Problem opening file'
cckwz      WRITE(ipr_outfile,'(A)') 'Problem opening file'
      GO TO 900
C
 992  WRITE(*,'(A)') 'Problem getting forecast group name'
cckwz      WRITE(ipr_outfile,'(A)') 'Problem getting forecast group name'
C
 900  CONTINUE
C      WRITE(*,603)
 603  FORMAT(/' FINISHED.')
c 
c  add calls to close all open files - dp 13 July 95
      Close(IUSS_INPUT)
cckwz      Close(ipr_outfile)
      Call close_all_open_files()
c       
      STOP
      END
c  ---------------------------------------------------------
      Subroutine set_rc_missing
c
      INCLUDE 'common/fratng'     
      DATA xnone/4hNone/, blank/4h    /
c
      rivern(1) = xnone
      rivsta(1) = xnone
      fptype(1) = xnone
      rfcomt(1) = xnone
      do 10 i = 2, 5
	rivern(i) = blank
	rivsta(i) = blank
	fptype(i) = blank
	rfcomt(i) = blank
 10   continue
c
      rlat = -999.
      rlong = -999.
      areat = -999.
      areal = -999.
      fldstg = -999.
      floodq = -999.
      scfstg = -999.
      wrnstg = -999.
      gzero = -999.
      rfstg = -999.
      rfq = -999.
      irfday = -999
c
      return
      end
