c  module number_of_fgroups.f
c
c  desc 'return the total number of forecast groups for the rfc'
c
cc AV rename the routine for linux port
      Subroutine number_of_fgroups(total_fgroups)
c...................................................................
c  argument list:
c
c    total_fgroups:  the total number of forecast groups
c                     for the rfc
c...................................................................
c
c  routine originally written by George Smith, HRL, November 14, 1992
c  modified to also count special fgs - G. Smith, HRL, April 5, 1993
c  modified to skip processing of obsolete special fgs - 
c     G. Smith, HRL, Oct 10, 1996
c...................................................................
c
      INCLUDE 'common/fccgd'                                            RTi     
      INCLUDE 'common/fccgd1'                                           RTi     
      INCLUDE 'common/fcfgs'                                            RTi     
      INCLUDE 'common/fcunit'                                           RTi     
c
      Integer total_fgroups
      Integer icg
      Real*4 obsolt(2)
      
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/number_of_fgroups.f,v $
     . $',                                                             '
     .$Id: number_of_fgroups.f,v 1.4 2002/02/20 10:18:50 michaelo Exp $
     . $' /
C  =====================================================================
C
      Data obsolt/4hOBSO,4hLETE/
c
      total_fgroups = 0
c
c  First count forecast groups which belong to carryover groups
C
C  FCCGD
C      COPY 1ST RECORD OF FILE FCCOGDEF INTO CB FCCGD
C-------
      CALL UREADT(KFCGD,1,NSLOTS,ISTAT)
C
      DO 100 ICG = 1, NCG
c
C  FCCGD1
C      Fill common FCCGD1 for each carryover group
C--------
      CALL UREADT(KFCGD,ICOREC(ICG),CGIDC,ISTAT)
c
      total_fgroups = total_fgroups + NFG
c
 100  Continue
c
c  Now add any special forecast groups
C
C  GET NUMBER OF FORECAST GROUPS FROM RECORD ONE OF FILE FCFGSTAT
C
      CALL UREADT(KFFGST,1,FGID,ISTAT)
      NFGREC=IDUMYG
      IF(NFGREC.LE.0) Return
C
C  LOOP THROUGH ALL FORECAST GROUPS ON FILE - EVEN OBSOLETE ONES
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
	  total_fgroups = total_fgroups + 1
        end if
c
      end if
c
 110  Continue
c
      Return
      End
