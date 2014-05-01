C MODULE ETSDEF
C-----------------------------------------------------------------------
C
      SUBROUTINE ETSDEF (TTS,MTTS,TS,MTS,NWORK,
     *  IREADC,TSID,DTYPE,UNITS,IDT,TSTYPE,EFILETP,ETSTYPEN,LOC,
     *  IER)

      INCLUDE 'common/sionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/calb_stubs/RCS/etsdef.f,v $
     . $',                                                             '
     .$Id: etsdef.f,v 1.1 2001/01/26 14:31:15 dws Exp $
     . $' /
C    ===================================================================
C

      WRITE(ISTDERR,*) 'Enter ETSDEF dummy.  Routine is not active.'

      RETURN
      END
