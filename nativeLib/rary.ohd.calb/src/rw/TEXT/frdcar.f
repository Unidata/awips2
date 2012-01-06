C MODULE FRDCAR
C-----------------------------------------------------------------------
C  Routine to read data from an NWSRFS DATACARD file.
C     ------------------------------------------------------------------
C     variables:
C
C     extloc    .... external location information array
C     ierr      .... error flag (0 if no error)
C     iopnum    .... operation number
C     convf1    .... multiplication factor in units conversion
C     mo        .... first month of data in file
C     opname    .... operation (routine) name 
C     sname     .... subroutine name (used in diagnostics)
C     iunit     .... unit number for file to read
C     itime     .... data interval for TS
C     iyear     .... year of data to read (4 digit)
C     tsformat  .... format of TS data
C     ------------------------------------------------------------------

      subroutine frdcar ( extloc, ld, d, md, mo, iyear, ierr )

      include 'common/ionum'
      include 'common/fctime'
      include 'common/fdbug'
      include 'common/fprog'

      dimension d(md), extloc(*)  
      character oldopn*8
      character tsformat*12
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/rw/RCS/frdcar.f,v $
     . $',                                                             '
     .$Id: frdcar.f,v 1.3 1999/04/22 14:19:50 page Exp $
     . $' /
C    ===================================================================
C

      iopnum=-1
      call fstwhr ('FRDCAR  ',iopnum,oldopn,iopold)
      
      ierr = 0

      if (itrace.ge.1) then
         write(iodbug,*) 'ENTER FRDCAR'
         endif

      call mdyh1 ( lda, lhr, mo, lday, iyear, idum, 100, 0, zcode )

      iunit  = extloc(1) + .01
      iamon1 = extloc(2) + .01
      iamon2 = extloc(3) + .01
      convf1 = extloc(4)
      convf2 = extloc(5)
      itime  = extloc(6) + .01
      write (tsformat,'(3a4)') (extloc(i), i = 37, 39)

      call cardrd (1, iunit, tsformat, iamon1, iamon2, convf1,
     +             convf2, itime, mo, iyear, d(ld), 1, ierr )
      if (ierr.ne.0) then
         write (ipr,*) '**ERROR** Problem reading from DATACARD-TS ',
     +      'file on unit ', iunit,'.'
         call error
         endif
         
      call fstwhr (oldopn,iopold,oldopn,iopold)

      if (itrace.ge.1) then
         write(iodbug,*) 'EXIT FRDCAR'
         endif

      return

      end
