C
C    mccright - print MCP3 program copyright
C
C-----------------------------------------------------------------------
C history:
C
C 1.0 (9-17-93)         Steven A. Malers, RTi   Created routine.
C-----------------------------------------------------------------------
C notes:        (1)     This routine prints a copyright for the program.
C-----------------------------------------------------------------------
C variables:
C
C istdout       .... unit number for standard output
C-----------------------------------------------------------------------
      subroutine mccright

      include 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mcp3_shared/RCS/mccright.f,v $
     . $',                                                             '
     .$Id: mccright.f,v 1.2 1996/07/11 19:30:51 dws Exp $
     . $' /
C    ===================================================================
C

      write(istdout,10)
10    format ( /,
     +'MCP3 Copyright information',//,
     +'This program is part of the National Weather Service River',/,
     +'Forecast System (NWSRFS).  Consequently, the original',/,
     +'authorship rights of this software belong to the NWS.  This',/,
     +'software is in the public domain.  Contact the NWS if you',/,
     +'have questions:',//,
     +'     NOAA/National Weather Service',/,
     +'     Hydrologic Research Laboratory, W/OH3',/,
     +'     1325 East-West Highway',/,
     +'     Silver Spring, MD  20910',/,
     +'     (301) 713-0640',//,
     +'Enhancements to the original NWS software have been made in',/,
     +'coordination with the NWS by:',//,
     +'     Riverside Technology, inc.',/,
     +'     2821 Remington Street',/,
     +'     Fort Collins, CO  80525',/,
     +'     (303) 223-2944',/,
     +'     (303) 223-2955 FAX',//,
     +'Any reproduction or further enhancement of this version of',/,
     +'MCP3 must carry this copyright notice accessed with a command',/,
     +'line option.',//)
      return
      end
