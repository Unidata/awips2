c  module read_forecast_and_carryover_group_names.f
c
c  desc 'return the forecast and corresponding carryover group
c         names for the rfc'
c
cc AV change routine name for linux port.  (undefine ref. error)
      Subroutine read_fcstandco_groupnames
     1            (fg_ids, cg_ids)
c...................................................................
c  argument list:
c
c    fg_ids:  forecast group names that belong to
c              carryover groups in to rfc area
c    cg_ids:  carryover group name corresponding to each
c              entry in fg_ids
c
c...................................................................
c
c  routine originally written by George Smith, HRL, November 14, 1992
c  modified to also return special fg names - G.Smith, HRL, April 1993
c  modified to skip processing of obsolete special fgs - 
c     G. Smith, HRL, Oct 10, 1996
c...................................................................
c
      INCLUDE 'common/fccgd'                                            RTi     
      INCLUDE 'common/fccgd1'                                           RTi     
      INCLUDE 'common/fcfgs'                                            RTi     
      INCLUDE 'common/fcunit'                                           RTi     
c
      Character*8 fg_ids(1), cg_ids(1)
      Real*4 obsolt(2)
c
      Double Precision special
c
      Integer icg, ifgrec, fg_counter, fgs_matched
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/read_f_and_c_group_names.f,v $
     . $',                                                             '
     .$Id: read_f_and_c_group_names.f,v 1.4 2002/02/11 15:11:55 michaelo Exp $
     . $' /
C  =====================================================================
C
c
      Data special/8hSpecial /
      Data obsolt/4hOBSO,4hLETE/
C
C  GET NUMBER OF FORECAST GROUPS FROM RECORD ONE OF FILE FCFGSTAT
C
      CALL UREADT(KFFGST,1,FGID,ISTAT)
      NFGREC=IDUMYG
      IF(NFGREC.LE.0) Return
c...................................................................
C
C  FCCGD
C      COPY 1ST RECORD OF FILE FCCOGDEF INTO CB FCCGD
C-------
      CALL UREADT(KFCGD,1,NSLOTS,ISTAT)
C
      fg_counter = 0
c
c  This loop will fill the fg list with all fgs belonging
c   to carryover groups, grouped by carryover group.
c  After this loop we will add all special fgs to the list.
c
      DO 100 ICG = 1, NCG
c
C  FCCGD1
C      Fill common FCCGD1 for each carryover group
C--------
      CALL UREADT(KFCGD,ICOREC(ICG),CGIDC,ISTAT)
c
      fgs_matched = 0
C
C  LOOP THROUGH ALL FORECAST GROUPS ON FILE - EVEN OBSOLETE ONES
C
      DO 90 IFGREC = 1, NFGREC
c
      CALL UREADT(KFFGST,IFGREC,FGID,ISTAT)
c
c  See if this forecast group read from fgstatus file
c   belongs to the carryover group currently in common FCCGD1
c
      IF(CGIDC(1) .eq. CGIDF(1) .and. CGIDC(2) .eq. CGIDF(2))
     1   then
	  fg_counter = fg_counter + 1
	  fgs_matched = fgs_matched + 1
c
c  umemov moves n words (4 characters long each) from src to dest
c   arguments with syntax -- call umemov(src, dest, n)
c
	  Call umemov(FGID,  fg_ids(fg_counter), 2)
	  Call umemov(CGIDF, cg_ids(fg_counter), 2)
	  if(fgs_matched .ge. nfg) go to 100
      end if
c
 90   Continue
c
 100  Continue
c
c  Loop through all fgs one last time to pick up any special fgs.
C
      DO 110 IFGREC = 1, NFGREC
c
      CALL UREADT(KFFGST,IFGREC,FGID,ISTAT)
c
c  Skip if OBSOLETE forecast group
c   The following one line (and associated end if) added 961010 
c     by George Smith
c
      IF(FGID(1) .ne. obsolt(1) .or. FGID(2) .ne. obsolt(2)) then
c
c  See if this is a special forecast group
c
        IF(ISPEC .eq. 1) then
c
	  fg_counter = fg_counter + 1
	  fgs_matched = fgs_matched + 1
c
c  umemov moves n words (4 characters long each) from src to dest
c   arguments with syntax -- call umemov(src, dest, n)
c
	  Call umemov(FGID,  fg_ids(fg_counter), 2)
c
c  Put "Special" into carryover group name
c
	  Call umemov(special, cg_ids(fg_counter), 2)
        end if
c
      end if
c      
 110  Continue
c
      Return
      End

