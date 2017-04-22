C MODULE FRESTR
C-----------------------------------------------------------------------
C
      SUBROUTINE FRESTR
C
C  ROUTINE TO PRINT SUBTOTAL OF ERRORS AND WARNINGS SINCE LAST CALLED
C  AND TOTAL NUMBER OF ERROS AND WARNINGS IN RUN.
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/errdat'
      INCLUDE 'common/toterz'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/frestr.f,v $
     . $',                                                             '
     .$Id: frestr.f,v 1.3 1998/07/02 19:19:47 page Exp $
     . $' /
C    ===================================================================
C
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) '*** ENTER FRESTR'
C
      WRITE (IPR,10) NERRS,NWARN,NERRST,NWARNT
      IF (IOERR.NE.IPR) WRITE (IOERR,10) NERRS,NWARN,NERRST,NWARNT
C
10    FORMAT ('0',132('*') /
     *   '0THE ABOVE STEPS HAVE RESULTED IN ',
     *       I4,' ERRORS AND ',I4,' WARNINGS.',
     *   ' CURRENT RUN TOTAL IS ',I5,' ERRORS AND ',I5,' WARNINGS.' /
     *   '0',132('*'))
C
      NERRS=0
      NWARN=0
C
      RETURN
C
      END
