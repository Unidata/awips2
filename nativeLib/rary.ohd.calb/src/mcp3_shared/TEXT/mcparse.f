C
C mcparse - parse command line arguments for MCP3
C
C-----------------------------------------------------------------------
C history:
C
C 1.0 (1-25-93)         Steven A. Malers, RTi   Created routine.
C 1.1 (9-17-93)         SAM, RTi                Updated to offer more
C                                               debug options.
C-----------------------------------------------------------------------
C notes:        (1)     This routine will scan the command line
C                       arguments and assign return variables
C                       accordingly.  Available arguments are listed
C                       below and are printed by the "mcusage" routine:
C
C                               -copy   Print the program copyright.
C                               -do#    Set the debug level for a given
C                                       operation.
C                               -ds1234 Set the debug switch on for
C                                       the given 4-character system
C                                       debug code.
C                               -h, ?   Print the program usage.
C                               -rcoIn  Read carryover from input file.
C                               -wcoOut Write carryover to output file.
C                               -t#     Set the trace level.
C
C               (2)     The input and output files are also specified,
C                       on the command line and are taken as the first
C                       and second non-dashed arguments.
C
C               (3)     This routine depends on the intrinsic routines
C                       "iargc" (number of command line arguments), and
C                       "getarg" (get a specific command line argument).
C                       These are generally common from machine to
C                       machine, but may not be in all cases.
C
C               (4)     The 'fdbug' common block is for debugging
C                       operations.  The 'sysbug' common block is for
C                       debugging the whole system.
C-----------------------------------------------------------------------
C variables:
C
C arg           .... command line argument
C i             .... counter for command line arguments
C infile        .... name of input file
C outfile       .... name of output file
C nargs         .... number of arguments (not including program name)
C syscode       .... 4-character system debug code
C-----------------------------------------------------------------------

        subroutine mcparse ( infile, outfile )

        character*(*)   infile, outfile
        character       argv*100
        integer         i, iargc, nargs, syscode

        include         'common/fdbug'
        include         'common/rdwtco'
        include         'common/sysbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/mcparse.f,v $
     . $',                                                             '
     .$Id: mcparse.f,v 1.3 2002/02/11 13:30:22 michaelo Exp $
     . $' /
C    ===================================================================
C

C       Get the number of command line arguments after program name...

        nargs = iargc ()

C       Get command line arguments...

        infile  = ' '
        outfile = ' '
        do 100 i = 1, nargs
                call getarg ( i, argv )
                if ((argv(1:5).eq.'-copy').or.(argv(1:5).eq.'-COPY'))
     +                  then
                        call mccright
                        stop
                else if ((argv(1:3).eq.'-do').or.(argv(1:3).eq.'-DO'))
     +                  then
                        if ( argv(4:4) .eq. ' ' ) then
                                !
                                ! Debug for all operations...
                                !
                                idball = 1
                        else    !
                                ! Debug for given operation number...
                                !
                                read(argv,50) iopnum
50                              format(3x,i2)
                                do 70 j = 1, ndebug
                                        if ( idebug(j) .eq. iopnum )then
                                                goto 80
                                        endif
70                              continue
                                if ( ndebug .lt. 20 ) then      ! add operation
                                        ndebug = ndebug + 1
                                        idebug(ndebug) = iopnum
                                endif
80                              continue
                        endif
                else if ((argv(1:3).eq.'-ds').or.(argv(1:3).eq.'-DS'))
     +                  then
                        if ( argv(4:4) .eq. ' ' ) then
                                !
                                ! Debug for all system...
                                !
                                iall = 1
                        else    !
                                ! Debug for given operation number...
                                !
                                read(argv,82) syscode
82                              format(3x,a4)
                                do 83 j = 1, ndebgs
                                      if ( idebgs(j) .eq. syscode )then
                                                goto 85
                                        endif
83                              continue
                                if ( ndebgs .lt. 20 ) then      ! add debug code
                                        ndebgs = ndebgs + 1
                                        idebgs(ndebgs) = syscode
                                endif
85                              continue
                        endif
                else if ((argv(1:2).eq.'-h').or.(argv(1:2).eq.'-H').or.
     +                  (argv(1:1).eq.'?')) then
                        call mcusage
                        stop
                else if ((argv(1:4).eq.'-rco').or.(argv(1:4).eq.'-RCO'))
     +                  then
                        read(argv,86) rdcofl
86                      format(4x,a)
                        jrdco = 1
                else if ((argv(1:2).eq.'-t').or.(argv(1:2).eq.'-T'))then
                        read(argv,90) itrace
90                      format(2x,i2)
                else if ((argv(1:4).eq.'-wco').or.(argv(1:4).eq.'-WCO'))
     +                  then
                        read(argv,86) wtcofl
                        jwtco = 1
                else    !
                        ! Assume that it's the input or output file...
                        !
                        if ( infile(1:1) .eq. ' ' ) then
                                infile  = argv
                        else
                                outfile = argv
                        endif
                endif
100     continue

        return
        end
