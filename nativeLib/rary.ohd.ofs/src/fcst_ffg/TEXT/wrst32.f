C MEMBER WRST32
C  (from old member FCWRST32)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 07/27/95.12:20:50 BY $WC21DT
C
      SUBROUTINE WRST32 (ISTAT,TYPE,ID)
C..........................................
C     PRINTS MESSAGES FOR STATUS CODES RETURNED FROM WPPREC AND
C     CALLS ERROR.
C..........................................
C     WRITTEN BY -- JANICE LEWIS, HRL  --  OCTOBER 1991
C..........................................
      DIMENSION ID(2)
      INCLUDE 'common/ionum'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_ffg/RCS/wrst32.f,v $
     . $',                                                             '
     .$Id: wrst32.f,v 1.3 1996/05/07 11:19:35 page Exp $
     . $' /
C    ===================================================================
C
C..........................................
C     CHECK STATUS CODE--PRINT APPROPRIATE MESSAGE

      GO TO (100,110,120,130), ISTAT

C  STATUS=1
  100 WRITE(IPR,105) TYPE,ID
  105 FORMAT ('0**ERROR** - IN WPPREC - SYSTEM ERROR WHILE WRITING ',
     *   'TYPE ',A4,' FOR IDENTIFIER ',2A4,' TO DATA FILE.')
      GO TO 200

C  STATUS=2
  110 WRITE(IPR,115) TYPE,ID
  115 FORMAT ('0**ERROR** - IN WPPREC - FILE INTO WHICH ',A4,
     *   ' PARAMETER RECORD FOR IDENTIFIER ',2A4,' IS TO BE ',
     *   'WRITTEN IS FULL.')
      GO TO 200

C  STATUS=3
  120 WRITE(IPR,125) TYPE,ID
  125 FORMAT ('0**ERROR** - IN WPPREC - ',2A4,' IS AN INVALID ',
     *   'IDENTIFIER BECAUSE IT IS ONE OF THE RESERVED WORDS. ')
      GO TO 200

C  STATUS=4
  130 WRITE(IPR,135) TYPE,ID
  135 FORMAT ('0**ERROR** - IN WPPREC - PARAMETER TYPE (',A4,') TO ',
     *   'BE WRITTEN FOR STATION ',2A4,' DOES NOT MATCH PARAMETER ',
     *   'TYPE IN THE EXISTING PARAMETER RECORD.')
C
200   RETURN
      END
