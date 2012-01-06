C MEMBER CNVT26
C  (from old member FCCNVT26)
C
C DESC CONVERT 'CMSD' TO 'CFSD' OR 'ACFT'
C------------------------------------------------------------------
      SUBROUTINE CNVT26(UNITSM,DIM,UNITSE,CFACT,CONST,ISTOR,IER)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/cnvt26.f,v $
     . $',                                                             '
     .$Id: cnvt26.f,v 1.1 1995/09/17 18:51:17 dws Exp $
     . $' /
C    ===================================================================
C
C------------------------------------------------------------------
C  SUBROUTINE TO INTERCEPT 'CMSD' UNITS AND TO SET THE CONVERSION TO
C  ENGLISH 'ACFT' OR 'CFSD' BASED ON THE ISTOR VALUE. ISTOR = 0 FOR
C  'CFSD' AND = 1 FOR 'ACFT'.
C  OTHERWISE IT TRANSFERS CONTROL TO FCONVT.
C------------------------------------------------------------------
C  WRITTEN BY JOE OSTROWSKI - HRL - SEPT 1983
C------------------------------------------------------------------
C
      DATA CMSD/4HCMSD/,ACFT/4HACFT/
C
C  SEE IF UNITS METRIC ARE 'CMSD'
C
      IF (UNITSM.NE.CMSD) GO TO 100
C
C  IF WE HAVE 'CMSD', CHECK TO SEE IF WE NEED THE ACRE-FOOT CONVERSION.
C
      IF (ISTOR.EQ.0) GO TO 100
C
C  SET THE 'ACRE-FOOT' CONVERSION FACTOR.
C
      UNITSE = ACFT
      CFACT = 86400./43560.*((1.0/.3048)**3.0)
      GO TO 9000
C
C-------------------------------------------------------
C  REGULAR CONVERSION IS NEEDED.
C
  100 CONTINUE
      CALL FCONVT(UNITSM,DIM,UNITSE,CFACT,CONST,IER)
C
 9000 CONTINUE
      RETURN
      END
