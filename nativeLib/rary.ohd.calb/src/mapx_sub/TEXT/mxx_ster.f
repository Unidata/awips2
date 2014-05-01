C MODULE MXX_STER
C  =====================================================================
C  pgm: MXX_STER .. output error messages for routine "mxx_stim"
C
C  use:     CALL MXX_STER(ISTART,IEND,IERRS,IERRE,ISTAT)
C
C   in: ISTART ..... starting date as an integer, mmddyyyy - INT
C   in: IEND ....... ending date as an integer, mmddyyyy - INT
C   in: IERRS ...... start date status: - INT
C   in:                0   = valid start date
C   in:                pos = BAD start date
C   in: IERRE ...... end date status: - INT
C   in:                0   = valid end date
C   in:                pos = BAD end date
C  out: ISTAT ...... output status, 0 if no error, 1 if error - INT
C  =====================================================================
      SUBROUTINE MXX_STER(ISTART,IEND,IERRS,IERRE,ISTAT)

      INTEGER        ISTART,IEND,IERRS,IERRE,ISTAT,JE
      CHARACTER*200  LIN
      CHARACTER*25   MSG2
      CHARACTER*23   MSG3
      CHARACTER*32   MSG4
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_ster.f,v $
     . $',                                                             '
     .$Id: mxx_ster.f,v 1.1 2001/06/13 09:28:29 mgm Exp $
     . $' /
C    ===================================================================
C

      DATA   MSG2 / 'bad numbers in start date' /
      DATA   MSG3 / 'bad numbers in end date' /
      DATA   MSG4 / 'start date later than end date: ' /

        ISTAT = 0
        IF (IERRS.GT.0 .OR. IERRE.GT.0) ISTAT = 1

        IF (IERRS .GT. 0) THEN
          LIN = ' '
          WRITE(LIN,'(A,I10)',IOSTAT=JE) MSG2,ISTART
          IF (JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

        IF (IERRE .EQ. 5) THEN
          LIN = ' '
          WRITE(LIN,'(A,I10,I10)',IOSTAT=JE) MSG4,ISTART,IEND
          IF (JE.EQ.0) CALL WLIN('E',LIN)
        ELSEIF (IERRE .GT. 0) THEN
          LIN = ' '
          WRITE(LIN,'(A,I10)',IOSTAT=JE) MSG3,IEND
          IF (JE.EQ.0) CALL WLIN('E',LIN)
        ENDIF

      RETURN
      END
