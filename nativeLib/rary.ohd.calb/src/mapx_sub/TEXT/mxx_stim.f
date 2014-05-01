C MODULE MXX_STIM
C  =====================================================================
C  pgm: MXX_STIM .. get MAPX begin and end times in hours since 1900
C
C  use:     CALL MXX_STIM(ISTART,IEND,NSTART,NEND,N1,N2,IERRS,IERRE)
C
C   in: ISTART ..... starting date as an integer, mmddyyyy - INT
C   in: IEND ....... ending date as an integer, mmddyyyy - INT
C  out: NSTART ..... starting date as hours from 1900 for the
C  out:              the start of the month given in ISTART - INT
C  out: NEND ....... ending date as hours from 1900 for the
C  out:              very end of the month given in IEND - INT
C  out: N1 ......... starting date as hours from 1900 for the
C  out:              start of the day given in ISTART - INT
C  out: N2 ......... ending date as hours from 1900 for the
C  out:              very end of the day given in IEND - INT
C  out: IERRS ...... start date status: 0   = valid start date
C  out:                                 pos = BAD start date
C  out: IERRE ...... end date status:   0   = valid end date
C  out:                                 pos = BAD end date
C
C  rqd: DDGCH2,DDGCDM,DDSC,DDYCDL
C  =====================================================================
      SUBROUTINE MXX_STIM(ISTART,IEND,NSTART,NEND,N1,N2,IERRS,IERRE)

      EXTERNAL    DDGCH2,DDGCDM,DDSC,DDYCDL

      INTEGER     ISTART,IEND,NSTART,NEND,N1,N2,IERRS,IERRE
      INTEGER     ISTAT,NY,NM,ND,NODIM,IDIV,IR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_stim.f,v $
     . $',                                                             '
     .$Id: mxx_stim.f,v 1.1 2001/06/13 09:28:38 mgm Exp $
     . $' /
C    ===================================================================
C

        NSTART = 0
        NEND   = 0
        N1     = 0
        N2     = 0
        IERRS  = 4
        IERRE  = 4

        IF (ISTART.GE.010100   .AND. IEND.GE.010100   .AND.
     $      ISTART.LT.12312100 .AND. IEND.LT.12312100       ) THEN

C      Get julian day for start and end dates
C      (Note, dates shifted one hour for NEXRAD uses hour 1-24)
C
C      Get year, month, day from mmddyyyy integer
C      Check if numbers are good for year, month, day     - DDSC
C      If good, correct year to 4 digits if needed        - DDYCDL
C      Get hours from 1900 (N1) for begin of given day    - DDGCH2
C      Get hours from 1900 (NSTART) for begin of month    - DDGCH2

           IDIV = 10000
           IF (ISTART .LT. 1000000) IDIV = 100
           IR = ISTART/IDIV
           NY = ISTART - (IDIV*IR)
           NM = IR/100
           ND = IR - 100*NM
          CALL DDSC(NY,NM,ND,ISTAT)
          IF (ISTAT .GT. 0) THEN
            IERRS = ISTAT
          ELSE
            IERRS = 0
            CALL DDYCDL(NY,NM,ND)
            CALL DDGCH2(N1,NY,NM,ND,1)
            CALL DDGCH2(NSTART,NY,NM,1,1)
          ENDIF

C      Repeat the process above except use ending date:
C       end of given day (N2), end of given month (NEND)
C      Finally check if beginning date is later than the end date

           IDIV = 10000
           IF (IEND .LT. 1000000) IDIV = 100
           IR = IEND/IDIV
           NY = IEND - (IDIV*IR)
           NM = IR/100
           ND = IR - 100*NM
          CALL DDSC(NY,NM,ND,ISTAT)
          IF (ISTAT .GT. 0) THEN
            IERRE = ISTAT
          ELSE
            IERRE = 0
            CALL DDYCDL(NY,NM,ND)
            CALL DDGCH2(N2,NY,NM,ND,23)
            N2 = N2+1
            CALL DDGCDM(NY,NM,NODIM)
            CALL DDGCH2(NEND,NY,NM,NODIM,23)
            NEND = NEND+1

            IF (IERRS .EQ. 0) THEN
              IF (N1 .GT. N2) IERRE = 5
            ENDIF
          ENDIF

        ENDIF

      RETURN
      END
