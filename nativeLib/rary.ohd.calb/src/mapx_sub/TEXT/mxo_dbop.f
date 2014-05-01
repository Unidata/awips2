C MODULE MXO_DBOP
C  =====================================================================
C  pgm: MXO_DBOP .. Open ofs files for ppp database
C  use:     CALL MXO_DBOP(CMD,ISTAT)
C
C   in: CMD ...... Control command as follows: - CHAR*(*)
C   in:              'O' .. Open database files if not already open
C   in:              'C' .. Close database file if already open
C  out: ISTAT .... Status of the open operation from routine "rpppco"
C  out:            as follows: - INT
C  out:               0 .. no error from RPPPCO (always 0 for "C" CMD)
C  out:               1 .. for error from RPPPCO
C  out:              66 .. for an unknown command sent to this routine
C  out: (file) ... Error messages may be sent routine WLIN
C
C  rqd: RPPPCO,UCLOSL,WLIN
C  =====================================================================
      SUBROUTINE MXO_DBOP(CMD,ISTAT)

      EXTERNAL        RPPPCO,UCLOSL,WLIN

      CHARACTER*(*)   CMD
      CHARACTER*1     MYCMD
      INTEGER         ISTAT,IOPEN

      SAVE            IOPEN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxo_dbop.f,v $
     . $',                                                             '
     .$Id: mxo_dbop.f,v 1.1 2001/06/13 09:23:58 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   IOPEN / -1 /

        MYCMD = CMD(1:1)

        IF (MYCMD .EQ. 'O') THEN
          IF (IOPEN .EQ. -1) THEN
            CALL RPPPCO(ISTAT)
            IF (ISTAT .EQ. 0) THEN
              IOPEN = 1
            ELSE
              CALL WLIN('B',' ')
              CALL WLIN('E','cannot open files in routine RPPPCO')
              CALL WLIN('B',' ')
            ENDIF
          ENDIF
        ELSEIF (MYCMD .EQ. 'C') THEN
          IF (IOPEN .EQ. 1) THEN
            CALL UCLOSL()
            IOPEN = -1
            ISTAT = 0
          ENDIF
        ELSE
          ISTAT = 66
          CALL WLIN('E','bad command given to routine MXO_DBOP')
        ENDIF

      RETURN
      END
