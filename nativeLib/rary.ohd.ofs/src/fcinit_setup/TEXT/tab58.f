C$PRAGMA C (RMRESJFS5FILES)
C$PRAGMA C (MVRESJFILESTOPERM)
C tab58.f
C
C Operations Table routine for RES-J
C
C History:
C 10/15/97      Eric Markstrom, RTi Created Routine
C
C to    O       a real array, internally dimensioned to(1)
C               stores operations table entries for this operation
C
C LEFT  I       an integer variable indicating space left
C               in T array
C
C IUSET O       integer variable indicating how much space in T
C               array is used by RES-J operation
C
C NXT   I       integer variable which specifies starting location
C               of to array in T array
C
C LPO   I       integer variable which specifies starting location
C               of PO array in P array
C
C PO    I       real array, internally dimensioned PO(1), empty for
C               this operation
C
C LCO   I       integer variable which specifies starting location
C               of CO array in C array
C 
C TS    I       real array, contains information on time series used
C               by this segment
C
C MTS   I       integer variable whic is the dimension of the TS array
C 
C NWORK I       integer variable which specifies first location in 
C               the D array available for use as working space
C
C NDD   I       integer variable which specifies maximum number of 
C               days that can be held in the D array
C
C LWORK O       integer variable which indicates how much working space
C               is needed by RES-J
C
C IDT   O       integer variable which is the computation time interval
C               for RES-J
C
      subroutine tab58( to, left, iuset, nxt, lpo, po, lco,
     1     ts, mts, lwork, idt )

      EXTERNAL  JNUM
      INTEGER   JNUM
C
C   COMMON BLOCKS
C
C    RFS IO UNIT NUMBERS
      INCLUDE 'common/ionum'
C     INTEGER IN                ! Unit number of standard input device
C     INTEGER IPR               ! Unit number of standard print file
C     INTEGER IPU               ! Unit number of punch device

C    WHICH PART OF THE PROGRAM IS BEING EXECUTED
      INCLUDE 'common/where'
C     INTEGER ISEG(2)           ! Identifier for segment
C     INTEGER IOPNUM            ! Operation number currently executed
C     INTEGER OPNAME(2)         ! 8 character name of operation currently being executed

C    RFS DEBUG INFO
      INCLUDE 'common/fdbug'
C     INTEGER IODBUG            ! Unit number ofr debug printout
C     INTEGER ITRACE            ! Trace level
C     INTEGER IDBALL            ! debug info requested for all ops?
C     INTEGER NDEBUG            ! number of ops with debug requested
C     INTEGER IDEBUG(20)        ! List of ops with debug requested

      integer to(*)
      integer i_idt, left, iuset, nxt, lpo, lco, mts, idt
      integer locts
      real po(*), ts(*)
      real r_id(2), r_dt, dimn
      real r_po
      character    ch_po(4)
      character*8  oname,sname
      EQUIVALENCE   (ch_po(1), r_po)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_setup/RCS/tab58.f,v $
     . $',                                                             '
     .$Id: tab58.f,v 1.3 2002/02/11 13:17:21 michaelo Exp $
     . $' /
C    ===================================================================
C
C
C    DATA
      data  oname / 'RES-J   ' /
      data  sname / 'TAB58   ' /

C    DEBUG INITIALIZATION
      CALL FOPCDE ( oname, itab58 )
      CALL FPRBUG ( sname, 1, itab58, ibug )

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** ENTER tab58'
      endif


      numTimeSeries = po(2) 
      idt = po(3)                       ! computational time step may be 1 hour

      iuset = 4  + numTimeSeries
               
      call checkt( iuset, left, ierr )

      if (ierr.GE.1) then
        write(ipr, 901)
901     format( 1H0, 10X, '**ERROR** THERE IS INSUFFICIENT SPACE IN',
     1    'THE T ARRAY FOR THIS OPERATION' )
        call error
        return
      end if
        
      lwork = 0                         !! no work space needed
      

      to(1) = 58                        !! operation number
      to(2) = NXT + iuset               !! position of T array for next operation
      to(3) = LPO                       !! position of P array for this operation
      to(4) = LCO                       !! position of C array for this operation


      do i = 0, ( numTimeSeries - 1 )
        r_id(1)  = po( 5 + (5 *  i) )
        r_id(2)  = po( 6 + (5 *  i) )
        r_dt     = po( 7 + (5 *  i) )
        r_po     = po( 8 + (5 *  i) )
        i_idt    = JNUM( ch_po(3) )  * 10 + JNUM( ch_po(4) )  

        CALL FINDTS( r_id, r_dt, i_idt, locd, locts, dimn )
        ts(locts+8) = 1.01      ! make sure all output timeseries are set as having values

        to(5 + i) = locd

C       debug info 

      end do


C................................................
C EJM RTi Modification 12/01/97
C   rm all old RESJ fs5files and mv temp RES-J
C   fs5files to permanent RES-J fs5files
C   works for both resegdefs and segdefs
C
ccc       ier = rmRESJfs5files( OLDP, iseg  )      ! remove the old fs5files for the segment
ccc       ier = mvRESJFilesToPerm( PO, iseg )       ! move the temporary RESJ files to
                                                ! the permanent fs5files
C END EJM MODIFICATIONS

cc jgofus ier not initiallized       if(ier.eq.0) PO(1)=1
      
C       

      if (ITRACE.GT.0) then
        write(IODBUG, *) '*** EXIT ',sname
      endif
      ierr = 0
      return
      end
