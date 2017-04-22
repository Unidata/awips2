C MODULE TSPRT
C-----------------------------------------------------------------------
C
C  ROUTINES TO PRINT TIME SERIES DEFINITION INFORMATION.
C
      SUBROUTINE TSPRT_HEADER
C
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_top/RCS/tsprt.f,v $
     . $',                                                             '
     .$Id: tsprt.f,v 1.3 2002/02/11 19:05:09 dws Exp $
     . $' /
C    ===================================================================
C
C
      WRITE (IPR,10)
10    FORMAT ('0TIME SERIES USED BY SEGMENT:')
C
      WRITE (IPR,20)
20    FORMAT (
     * '0',3X,1X,
     *    'TIME    ',2X,
     *    '    ',2X,'DATA    ',2X,'TIME    ',2X,'    ',2X,
     *    'ESP ',2X,'ESP ' /
     * ' ',3X,1X,
     *    'SERIES  ',2X,
     *    'DATA',2X,'TIME    ',2X,'SERIES  ',2X,'FILE',2X,
     *    'FILE',2X,'TS  ' /
     * ' ',3X,1X,
     *    'ID      ',2X,
     *    'TYPE',2X,'INTERVAL',2X,'TYPE    ',2X,'TYPE',2X,
     *    'TYPE',2X,'TYPE   ',2X,
     *    'EXTERNAL INFORMATION' /
     * ' ',3X,1X,
     *    '--------',2X,
     *    '----',2X,'--------',2X,'--------',2X,'----',2X,
     *    '----',2X,'-------',2X,
     *    71('-')
     *  )
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_GENERAL (NUMTS,TSID,DTYPE,ITIME,TSTYPE,FILETP,
     *   EFILETP,ETSTYPE)
C
      CHARACTER*4 DTYPE,FILETP,EFILETP
      CHARACTER*8 TSID,TSTYPE,ETSTYPE
C
      INCLUDE 'common/ionum'
C
C
      NUMTS=NUMTS+1
      WRITE (IPR,10) NUMTS,TSID,DTYPE,ITIME,TSTYPE,FILETP,
     *   EFILETP,ETSTYPE
10    FORMAT (' ',I3,1X,A,2X,A,2X,I2,' HOURS',2X,A,2X,A,2X,
     *   A,2X,A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_FPDB_INPUT (FILETP,TSID,DTYPE)
C
      CHARACTER*4 FILETP,DTYPE
      CHARACTER*8 TSID
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) FILETP,TSID,DTYPE
10    FORMAT (T63,A,': ID=',A,' TYPE=',A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_FPDB_OUTPUT (FILETP,TSID,DTYPE,PLOCAT,DESCRP)
C
      CHARACTER*4 FILETP,DTYPE
      CHARACTER*4 BLANK4
      CHARACTER*8 TSID
      CHARACTER*20 DESCRP
C
      DIMENSION PLOCAT(2)
C
      INCLUDE 'common/ionum'
C
C
      BLANK4=' '
      WRITE (IPR,10) FILETP,TSID,DTYPE,PLOCAT,
     *   BLANK4,DESCRP(1:LENSTR(DESCRP))
10    FORMAT (T63,A,': ID=',A,' TYPE=',A,' LAT=',F6.2,' LON=',F7.2 /
     *        T63,A,'  DESCRP=',A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_CARD (FILETP,STAID,DESCRP,
     *   ITMO1,ITYR1,ITMO2,ITYR2,PATHNAME)
C
      CHARACTER*4 FILETP
      CHARACTER*12 STAID
      CHARACTER*20 DESCRP
      CHARACTER*112 PATHNAME
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) FILETP,STAID,DESCRP,ITMO1,ITYR1,ITMO2,ITYR2
10    FORMAT (T63,A,': ID=',A,' DESCRP=',A,
     *   ' POR=',I2.2,'/',I4.4,'-',I2.2,'/',I4.4)
C
      WRITE (IPR,20) PATHNAME(1:LENSTR(PATHNAME))
20    FORMAT (T42,'PATHNAME=',A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_PATHNAME (PATHNAME)
C
      CHARACTER*112 PATHNAME
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) PATHNAME(1:LENSTR(PATHNAME))
10    FORMAT (T42,'PATHNAME=',A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_CALB_INPUT (EXTLOC)
C
      DIMENSION EXTLOC(*)
C
      INCLUDE 'common/ionum'
C
C     
      ITIME=EXTLOC(5)
      LHEAD=EXTLOC(6)
      KMO=EXTLOC(10)
      KDA=EXTLOC(11)
      KYR=EXTLOC(12)
      KHRMIN=EXTLOC(13)
      KSEC=EXTLOC(14)
C
      WRITE (IPR,10) (EXTLOC(I),I=1,4),ITIME,LHEAD,(EXTLOC(I),I=7,9),
     *   KMO,KDA,KYR,KHRMIN,KSEC
10    FORMAT (' EXTERNAL LOCATION: ',8X,
     * '10',8X,'20',8X,'30',8X,'40',8X,'50',8X,'60',8X,'70',8X,'80' /
     * 20X,8('----+----+') /
     * 20X,3A4,A4,I4,I6,'-',3A4,'-',I2,'/',I2,'/',I2,'-',I4,'.',I4)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_CALB_OUTPUT (EXTLOC)
C
      DIMENSION EXTLOC(*)
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) (EXTLOC(I),I=1,6)
10    FORMAT (T63,'FILENAME=',3A4,' ID=',3A4)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_ARRAY_SPACE (LTS,MTS,LD)
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10)
10    FORMAT ('0',70('*'))
C
      WRITE (IPR,20) LTS,MTS
20    FORMAT('0',I5,' OF THE ',I5,
     * ' SPACES IN THE TS ARRAY HAVE BEEN USED.')
C
      WRITE (IPR,30) LD
30    FORMAT ('0',I5,' SPACES HAVE BEEN ALLOCATED TO TIME SERIES DATA',
     * ' IN THE D ARRAY.')
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_ESP_INPUT (EFILETP,STAID,ETSIDF,EDTYPE,ITIME,
     *   IDELTE)
C
      CHARACTER*4 EFILETP,EDTYPE
      CHARACTER*8 ETSIDF
      CHARACTER*12 STAID
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) EFILETP,STAID,ETSIDF,EDTYPE,ITIME,IDELTE
10    FORMAT (T63,A,': STAID=',A,' TSID=',A,' DTYPE=',A,
     *   ' ITIME=',I2.2,' IDELTE=',I1)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_ESP_OUTPUT (EFILETP,STAID,IWRITE)
C
      CHARACTER*(*) EFILETP,STAID
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) EFILETP,STAID,IWRITE
10    FORMAT (T63,A,': STAID=',A,' IWRITE=',I1)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_REPL_INPUT (EFILETP,ETSIDN,EDTYPEN,ITIMEN,
     *   EFILETPN)
C
      CHARACTER*4 EFILETP,EDTYPEN,EFILETPN
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) EFILETP,ETSIDN,EDTYPEN,ITIMEN,EFILETPN
10    FORMAT (T63,A,': TSIDN=',A,' DTYPEN=',A,
     *   ' ITIMEN=',I2.2,' EFILETPN=',A)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_REPL_OUTPUT (EFILETP,ETSIDF,IWRITE)
C
      CHARACTER*4 EFILETP
      CHARACTER*8 ETSIDF
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) EFILETP,ETSIDF,IWRITE
10    FORMAT (T63,A,': STAID=',A,' IWRITE=',I1)
C
      RETURN
C
      END

C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_BLEND_TS (EFILETP,GENTYP,TTS)
C
      CHARACTER*4 EFILETP
      CHARACTER*8 GENTYP
      DIMENSION TTS(*)
      DIMENSION ITTS(2)
C
      INCLUDE 'common/ionum'
C
C
      ITTS(1)=TTS(3)
      ITTS(2)=TTS(4)
      WRITE (IPR,10) EFILETP,GENTYP,(TTS(I),I=1,2),(ITTS(I),I=1,2)
10    FORMAT (T63,A,'(',A,'): ',2F5.2,2I5)
C
      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TSPRT_CREAT_PE (EFILETP,GENTYP,TTS)
C
      CHARACTER*4 EFILETP
      CHARACTER*8 GENTYP
      DIMENSION TTS(*)
C
      INCLUDE 'common/ionum'
C
C
      WRITE (IPR,10) EFILETP,GENTYP,(TTS(I),I=1,12)
10    FORMAT (T63,A,'(',A,'): ' /
     *        T63,5X,12F5.2)
C
      RETURN
C
      END
