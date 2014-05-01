C MODULE MXC_READ
C  =====================================================================
C  pgm: MXC_READ .. Read calibration mapx control file in free format
C
C  out: ISTAT ........ status (numbers additive, need masking): - INT
C  out:                  0 ... no errors
C  out:                  1 ... bad input unit number for control file
C  out:                  2 ... bad read from control file
C  out:                  4 ... given basin name has too many characters
C  out:                  8 ... too many basin names are given
C
C  rqd: MXC_NXWD,KKA2ID,KKCAPS
C
C  cmt: List of input variables used in this routine:
C  cmt:   @A: ISTART    (integer)    start date as mmddyyyy
C  cmt:       IEND      (integer)    end date as mmddyyyy
C  cmt:   @B: LLRUN     (integer)    flag for getting hrap file
C  cmt:       NAMLLG    (string)     filename for hrap
C  cmt:   @C: DIRGRD    (string)     pathname for xmrg dir
C  cmt:   @D: PREF      (string)     prefix for xmrg files
C  cmt:       NYY       (integer)    digits in year num of xmrg file
C  cmt:   @E: DT        (integer)    time series inverval
C  cmt:       ICV       (integer)    flag to make CVX and ZPX files
C  cmt:       RMISS     (integer)    flag for missing data code
C  cmt:   @F: UNIT      (string)     unit type
C  cmt:       NCOL      (integer)    number of columns in output
C  cmt:   @G: DIRMAP    (string)     pathname of time series output dir
C  cmt:   @I: BASINS    (string)     list of basin names
C  cmt:   @J: XTIME     (string)     8-char extension of existing files
C  =====================================================================
      SUBROUTINE MXC_READ(ISTART,IEND,LLRUN,NAMLLG,DIRGRD,
     $                    PREF,NYY,DT,ICV,RMISS,UNIT,
     $                    NCOL,DIRMAP,MAXBSN,NAREA,BASINS,XTIME,
     $                    ICD,ISTAT)
      
      EXTERNAL        MXC_NXWD,KKA2ID,KKCAPS

      CHARACTER*(*)   NAMLLG,DIRGRD,DIRMAP
      CHARACTER*(*)   PREF,UNIT,XTIME
      CHARACTER*8     BASINS(*)
      INTEGER         ISTART,IEND,LLRUN,NYY,DT,ICV
      INTEGER         RMISS,NCOL,MAXBSN,NAREA
      INTEGER         ICD,ISTAT
      CHARACTER*80    TEXT,WORD
      CHARACTER*1     CRDID
      INTEGER         IERR,LAST,LWORD,IDEFLT,ITEMP
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxc_read.f,v $
     . $',                                                             '
     .$Id: mxc_read.f,v 1.2 2005/06/09 19:27:42 dws Exp $
     . $' /
C    ===================================================================
C

        ISTAT = 0
        IF (ICD.LT.1 .OR. ICD.GT.99) THEN
          ISTAT = ISTAT+1
        ELSE

C  READ CONTROL FILE LINES 
          IDEFLT = -9090
          IERR  = 0
          CRDID = ' '
  110     IF (IERR .NE. 0) GOTO 140
            READ(ICD,'(A80)',IOSTAT=IERR) TEXT
            IF (IERR .GT. 0) THEN
              ISTAT = ISTAT+2

            ELSEIF (IERR.EQ.0 .AND. TEXT.NE.' ') THEN
              LAST = 0
              CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
              IF (LWORD .GT. 0) THEN
                IF (WORD(1:1) .EQ. '#') THEN
                   LAST = -1
                ELSEIF (WORD(1:1).EQ.'@' .AND. LWORD.EQ.2) THEN
                   CRDID = WORD(2:2)
                   CALL KKCAPS(CRDID)
                ELSE
                   LAST = 0
                ENDIF
              ENDIF

              IF (LAST .GE. 0) THEN
                SELECT CASE (CRDID)

C  READ START & END DATES, AND PARAMETER OF RUN OPTION
                CASE ('A')
                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,ISTART,IDEFLT)

                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,IEND,IDEFLT)

C  READ PATH TO HRAP SEGMENT FILE
                CASE ('B')
                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,ITEMP,IDEFLT)
                  IF (ITEMP .NE. IDEFLT) THEN
                    LLRUN = ITEMP
                    CALL MXC_NXWD(TEXT,LAST,NAMLLG,LWORD,NAMLLG)
                  ELSE
                    IF (LWORD .GT. 0) NAMLLG = WORD
                  ENDIF

C  READ PATH TO HRAP GRID FILES
                CASE ('C')
                  CALL MXC_NXWD(TEXT,LAST,DIRGRD,LWORD,DIRGRD)

C  READ TYPE OF GRIDDED DATA (FOR NEXRAD IT'S 'xmrg')
                CASE ('D')
                  CALL MXC_NXWD(TEXT,LAST,PREF,LWORD,PREF)

                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,NYY,IDEFLT)

C  READ TIME INTERVAL, OPTION TO GENERATE OUTPUT SERIES, AND OPTION
C  TO FILL MISSED DATA
               CASE ('E')
                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,DT,IDEFLT)

                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,ICV,IDEFLT)

                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,RMISS,IDEFLT)

C  READ UNIT OF MAPX DATA AND DESIRED NUMBER OF COLUMNS IN OUTPUT FILES
                CASE ('F')
                  CALL MXC_NXWD(TEXT,LAST,UNIT,LWORD,UNIT)
                  CALL KKCAPS(UNIT)

                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                  CALL KKA2ID(WORD,LWORD,NCOL,IDEFLT)

C  READ PATH TO MAPX (CVX & ZPX) OUTPUT FILES
                CASE ('G')
                  CALL MXC_NXWD(TEXT,LAST,DIRMAP,LWORD,DIRMAP)

C  READ NUMBER OF BASINS AND BASIN IDs
                CASE ('I')
                  CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
  120             IF (LAST .LT. 0) GOTO 130
                    IF (NAREA .LT. MAXBSN) THEN
                      IF (LWORD.GT.0 .AND. LWORD.LE.8) THEN
                        NAREA = NAREA+1
                        BASINS(NAREA) = WORD(1:LWORD)
                      ELSE
                        IF (ISTAT .LT. 4 ) ISTAT = ISTAT+4
                      ENDIF
                    ELSE
                        IF (ISTAT .LT. 8 ) ISTAT = ISTAT+8
                    ENDIF
                    CALL MXC_NXWD(TEXT,LAST,WORD,LWORD,' ')
                    GOTO 120
  130             CONTINUE

C  READ OPTIONAL FILE EXTENSION FOR DUPLICATE FILES
                CASE ('J')
                  CALL MXC_NXWD(TEXT,LAST,XTIME,LWORD,XTIME)

                END SELECT
              ENDIF
            ENDIF
            GOTO 110
  140     CONTINUE

        ENDIF

      RETURN
      END
