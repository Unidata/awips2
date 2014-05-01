C MODULE FUN013
C-----------------------------------------------------------------------
C
C  THIS FUNCTION SETS VALUES IN COMMON BLOCK UDEBUG FOR:
C     - HCL (HYDROLOGIC COMMAND LANGUAGE)
C     - PPD (PREPROCESSOR DATA BASE)
C     - PPP (PREPROCESSOR PARAMETRIC DATA BASE)
C     - PRD (PROCESSED DATA BASE)
C     - UTL (SYSTEM UTILITY ROUTINES)
C
      SUBROUTINE FUN013
C
      CHARACTER*8 TECHNAME
      INCLUDE 'common/ionum'
      INCLUDE 'udebug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_top/RCS/fun013.f,v $
     . $',                                                             '
     .$Id: fun013.f,v 1.3 2002/02/11 20:34:35 dws Exp $
     . $' /
C    ===================================================================
C
C
C  GET TECHNIQUE VALUES
      TECHNAME='HCLTR'
      CALL HPAST (TECHNAME,IHCLTR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='HCLDB'
      CALL HPAST (TECHNAME,IHCLDB,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PPDTR'
      CALL HPAST (TECHNAME,IPDTR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PPDDB'
      CALL HPAST (TECHNAME,IPDDB,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PPPTR'
      CALL HPAST (TECHNAME,IPPTR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PPPDB'
      CALL HPAST (TECHNAME,IPPDB,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PRDTR'
      CALL HPAST (TECHNAME,IPRTR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='PRDDB'
      CALL HPAST (TECHNAME,IPRDB,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='UTLTR'
      CALL HPAST (TECHNAME,IUTLTR,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
      TECHNAME='UTLDB'
      CALL HPAST (TECHNAME,IUTLDB,ISTAT)
      IF (ISTAT.GT.0) CALL FPHPWN (ISTAT,TECHNAME)
C
C PRINT DEBUG VALUES
      WRITE (IPR,20)
     *   IHCLTR,IHCLDB,
     *   IPDTR,IPDDB,
     *   IPPTR,IPPDB,
     *   IPRTR,IPRDB,
     *   IUTLTR,IUTLDB
20    FORMAT('0DEBUG VARIABLES SET:' /
     *   '0',3X,1X,'TRACE',1X,'DEBUG' /
     *   ' ',3X,1X,'-----',1X,'-----' /
     *   ' ','HCL',1X,I5,1X,I5 /
     *   ' ','PPD',1X,I5,1X,I5 /
     *   ' ','PPP',1X,I5,1X,I5 /
     *   ' ','PRD',1X,I5,1X,I5 /
     *   ' ','UTL',1X,I5,1X,I5 /
     *   )
C
      RETURN
C
      END
