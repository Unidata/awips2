C$PRAGMA C (FINDINTS)
C$PRAGMA C (GETRDATES)
      Subroutine get_segment_status(segnam, wrnq, floodq, IREAD_TS,
     1                              seg_status)
c
      INCLUDE 'common/fctime'  
      INCLUDE 'common/fctim2'
      INCLUDE 'common/fp'        
      INCLUDE 'common/ft'     
      INCLUDE 'common/fts'    
c
      Dimension segnam(2), seg_status(2)
      Dimension status(2,4)
      PARAMETER (LIWORK=20000)
      DIMENSION BUF(300), IWKBUF(LIWORK)
c
      Character*8 orig_segnam, test_segnam
      Character*8 blank8
      Character*1 blank1
c
      Integer delta_t(8)
C
C  =================================== RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ifp/src/Xifp/RCS/getSegStatus.f,v $
     . $',                                                             '
     .$Id: getSegStatus.f,v 1.4 2002/02/20 10:17:56 michaelo Exp $
     . $' /
C  =====================================================================
C
c
      Data status /4hUnkn, 4hown ,
     1             4hNorm, 4hal  ,
     2             4hAler, 4ht   ,
     3             4hFloo, 4hd   /
c
      Data delta_t/1, 2, 3, 4, 6, 8, 12, 24/
c
      Data blank8/8h        /, blank1/1h /
c
      if((wrnq .lt. 0.0) .and. (floodq .lt. 0.0)) then
        seg_status(1) = status(1,1)
        seg_status(2) = status(2,1)
        go to 999
      end if
c
c  Have flood or alert flow values, read QINE time series for
c   this segment and compare flows.
c
c  First see if any QINE time series exist for this segment.
c
c  Read ts array.
C
      IOPT = 0
      NOPARM = 0
c
      CALL FGETSG(SEGNAM, IXXREC, MP, P, MT, T, MTS, TS,
     1           IOPT, NOPARM, ISTAT)
C  .........................................................
c
c  Loop through valid delta_t values and try to find the
c   time series SEGNAM, "QINE", delta_t in the ts array.
c
c  Modified 4/20/93 by gfs to search for the QINE t.s. with
c   truncated versions of the segment name as the segment id.
c  This change requested by ABRFC because they append characters
c   (i.e., X) to the end of the usual segment names when testing
c   changes to parameters such as adding MAPX time series
c   to a segment.  They don't change the QINE t.s. in the
c   Processed Data Base but want to find a match to get segment
c   status while testing these changes.
c
c  umemov moves n words (4 characters long each) from src to dest
c   arguments with syntax -- call umemov(src, dest, n)
c
      Call umemov(SEGNAM,  orig_segnam, 2)
c
      Do 15 j = 8, 1, -1
c
        if (orig_segnam(j:j) .ne. blank1) then
c
          Call umemov(blank8,  test_segnam, 2)
c
          Do 5 k = 1, j
          test_segnam(k:k) = orig_segnam(k:k)
 5        Continue
c
          Do 10 i = 1, 8
            idt = delta_t(i)
            Call findints(test_segnam, 4hQINE, idt, TS, TS, istat)
            if(istat .gt. 0)go to 20
 10       Continue
        end if
 15   Continue
c
      WRITE(*,600)segnam
 600  Format(" QINE time series not found for segment ",2a4)
      seg_status(1) = status(1,1)
      seg_status(2) = status(2,1)
      go to 999
c
 20   Continue
c  fctime
      Call set_fctime_cb()
c  fctim2
      NHOPDB = 12
c
c  Have already created the global_toplevel widget in GetFGroupID
c
      icount=1
      Call GetRDates(ISTRHR, ILCDHR, IENDHR, ISDAHR, icount)
c
      if(iendhr .lt. ilcdhr) then
c
c  end run hour less than end of obs - just look at QINE value
c   for last period of the run
c
        jhour = iendhr + nhopdb
        num = 1
      else if(istrhr .ge. ilcdhr) then
c
c  the run starts at or after the end of obs time
c   (i.e., an all future run) - check QINE values for entire run period
c
        jhour = istrhr + nhopdb + idt
        num = (iendhr - istrhr - 1)/idt + 1
      else
c
c  run starts before the end of obs time and ends at or after
c   the end of the run - in either case, check QINE values
c   from the end of obs time to the end of the run
c
        jhour = ilcdhr + nhopdb
        num = (iendhr - ilcdhr)/idt + 1
      end if
      ifptr = 0
c
      Call rprd(segnam, 4hQINE, jhour, idt, num, 4hCMS , -999.0,
     1          BUF, ifptr, LIWORK, IWKBUF, ier)
c
      if(ier .ne. 0 .and. ier .ne. 2) then
         Write(*,606)ier
 606     Format(" The status returned from rprd is ",i2)
         seg_status(1) = status(1,1)
         seg_status(2) = status(2,1)
         go to 999
      end if
c
c      Write(ipr,633) segnam, jhour, idt, num, ier
 633  Format('In get_segment_status, segnam, jhour, idt, num, ier =',
     1 1x,2a4, i10, i3, i4, i3)
c      Write(ipr,634) (BUF(i), i = 1, num)
 634  Format('BUF ='/(8(1x,f9.2)))
c      Write(ipr,635) wrnq, floodq
 635  Format('Warning flow = ',f12.2,', Flood flow = ',f12.2)
      istatus = 2
      Do 30 i = 1, num
      if((BUF(i) .ge. wrnq) .and. (BUF(i) .lt. floodq)) then
        istatus = 3
c        write(*,620)segnam, i, BUF(i), wrnq
 620    Format("for segment ",2a4,", period ",i3,", flow ",f9.1,
     1         " is greater than alert flow ",f9.1)
      end if
      if(BUF(i) .ge. floodq) then
        istatus = 4
c        write(*,621)segnam, i, BUF(i), floodq
 621    Format("for segment ",2a4,", period ",i3,", flow ",f9.1,
     1         " is greater than flood flow ",f9.1)
        go to 40
      end if
 30   Continue
c
 40   seg_status(1) = status(1,istatus)
      seg_status(2) = status(2,istatus)
      go to 999
c
 990  WRITE(*,'(A)') 'Problem reading file'
      WRITE(IPR,'(A)') 'Problem reading file'
      GO TO 999
C
 991  WRITE(*,'(A)') 'Problem opening file'
      WRITE(IPR,'(A)') 'Problem opening file'
      GO TO 999
C
 999  Write(*,605)segnam, seg_status
 605  Format(" For segment ",2a4," the flow status is ",2a4)
      return
      end



