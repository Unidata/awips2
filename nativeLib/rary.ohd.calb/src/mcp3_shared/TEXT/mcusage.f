C
C    mcusage - print MCP3 program usage
C
C-----------------------------------------------------------------------
C history:
C
C 1.0 (1-25-93)         Steven A. Malers, RTi   Created routine.
C 1.1 (9-17-93)         SAM, RTi                Documented code better.
C-----------------------------------------------------------------------
C notes:        (1)     This routine prints a short summary of the
C                       program usage.  The message lists command line
C                       arguments but does not describe input file
C                       syntax.
C-----------------------------------------------------------------------
C variables:
C
C istdout       .... unit number for standard output
C-----------------------------------------------------------------------
      subroutine mcusage

      include 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/mcusage.f,v $
     . $',                                                             '
     .$Id: mcusage.f,v 1.2 1996/07/11 19:32:55 dws Exp $
     . $' /
C    ===================================================================
C

      write(istdout,10)
10    format ( /,
     +'-------------------------------------------------------------',/,
     +'MCP3 Usage:  mcp3 [ options ] infile outfile',//,
     +'The Manual Calibration Program reads a formatted input file',/,
     +'"infile" and creates the output file "outfile".',//,
     +'The following command line arguments are recognized:',//,
     +'    -copy     Print copyright information.',/,
     +'    -do[#]    Turn on the debug switch for the specified',/,
     +'              operation.  This option can be repeated up to',/,
     +'              20 times.  If an operation number is not given,',/,
     +'              then debugging is turned on for all operations.',/,
     +'              Debug messages are printed to the output file.',/,
     +'    -ds[code] Turn on the debug switch for the specified',/,
     +'              4-character system code.  This option can be',/,
     +'              repeated up to 20 times.  If a system code is',/,
     +'              not given, then debugging is turned on for all',/,
     +'              system debug codes.  Debug messages are printed',/,
     +'              to the output file.',/,
     +'    -h, ?     Print this help message.',/,
     +'    -rcoIn    Read the carryover array from file "In".',/,
     +'    -t#       Enable level # trace generation.  Trace',/,
     +'              messages are printed to the output file.',/,
     +'    -wcoOut   Write the carryover array to file "Out".',/,
     +'-------------------------------------------------------------')
      return
      end
