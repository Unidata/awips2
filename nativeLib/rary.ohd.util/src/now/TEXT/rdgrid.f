C$PRAGMA C (GET_APPS_DEFAULTS)
C  =====================================================================
C  pgm: RDGRID .. Get grid data for given HOC-date
C
C  use:     CALL RDGRID(TYPE,HTHOC,BCOL,NCOL,BROW,NROW,BUFF,LBUFF,ISTAT)
C
C   in: TYPE ........ code for data type - CHAR*4
C   in: HTHOC ....... hydrologic time in hours from Jan 1, 1900 - INT*4
C   in: BCOL ........ beginning column (x-origin) - INT
C   in: NCOL ........ number of columns (short integers per row) - INT
C   in: BROW ........ beginning row (y-origin) - INT
C   in: NROW ........ number of rows - INT
C  out: BUFF(1) ..... integer values row after row - INT*2
C   in: LBUFF ....... maximum dimension of "BUFF" - INT*4
C  out: ISTAT ....... status code: - INT
C  out:                 -2 ... file not found
C  out:                 -1 ... file in use and skipped
C  out:                  0 ... file read with no errors
C  out:                pos ... error occurred
C   in: (common) .... block common "UPDAIO" containing unit numbers and
C   in:               file status, used for i/o messages in "UP" rtns
C
C  rqd: DDGHC2,PAGRID,UPOPEN,UPCLOS,UPEXIS,KKTRIM
C  rqd: GET_APPS_DEFAULTS
C  =====================================================================
      SUBROUTINE RDGRID(TYPE,HTHOC,BCOL,NCOL,BROW,NROW,BUFF,LBUFF,ISTAT)


      EXTERNAL   UPEXIS,UPOPEN,UPCLOS,KKTRIM,PAGRID,DDGHC2

      INTEGER*4  HTHOC,LBUFF,HOC,LRBUFF
      INTEGER*2  BUFF(1),RBUFF(5000)
      INTEGER    BCOL,NCOL,BROW,NROW,ISTAT
      INTEGER    NOFC,IYR,IMO,IDA,IHR
      INTEGER    PBEG,PEND,IU,IC,IRECL,II
      INTEGER*4  BC,BR,NC,NR
      INTEGER*4  IBC,IBR,INC,INR,IEC,IER,EC,ER,COL1,COL2,COLX,ROW
      INTEGER*4  COLA,COLB,ROLX
      CHARACTER*128  DIR,PTH
      CHARACTER*32   PREFIX
      CHARACTER*4    TYPE

      INCLUDE 'updaio'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/now/RCS/rdgrid.f,v $
     . $',                                                             '
     .$Id: rdgrid.f,v 1.7 2005/09/16 19:44:29 hank Exp $
     . $' /
C    ===================================================================
C

      DATA    LRBUFF / 5000 /


 1112   FORMAT(' upgrid',4X,'  ** WARNG no exst',I4,' ',A)
 1113   FORMAT(' upgrid',4X,'  ** ERROR bad opn',I4,' ',A)
 1114   FORMAT(' upgrid',4X,'     open for grid',I4,' ',A)
 1116   FORMAT(' upgrid',4X,'  ** ERROR bad rea',I4,' ',A)
 1117   FORMAT(' upgrid',4X,'  ** ERROR bad clo',I4,' ',A)
 1118   FORMAT(' upgrid',4X,'  ** ERROR big fil',4X,' ',A)
 1119   FORMAT(' upgrid',4X,'  ** ERROR big row',4X,' ',A)
 1120   FORMAT(' ** Input XMRG file is not in standard format **')
 
C                         Convert entered hydro time to data base time

        HOC = HTHOC+12

C                         Get full pathname of grid data directory

        DIR = ' '
        CALL GET_APPS_DEFAULTS('ofs_griddb_dir',14,DIR,NOFC)
        IF( NOFC .LT. 128 ) DIR(NOFC+1:NOFC+1) = ' '

C                         Get month-day-year-hr from hr-of-century, HOC,
C                           and make pathname of desired data base file

        CALL DDGHC2(HOC,IYR,IMO,IDA,IHR)
        PREFIX = ' '
        PREFIX = TYPE

C                         Check for existance of given file.
C                         (try three types of pathnames if needed)
 
        IU = 3
        CALL PAGRID('b',PTH,DIR,PREFIX,IYR,IMO,IDA,IHR)
        CALL UPEXIS(IU,PTH,IC)
        IF(IC.LT.0) THEN
          CALL PAGRID('g',PTH,DIR,PREFIX,IYR,IMO,IDA,IHR)
          CALL UPEXIS(IU,PTH,IC)
        ENDIF
        IF(IC.LT.0) THEN
          CALL PAGRID('o',PTH,DIR,PREFIX,IYR,IMO,IDA,IHR)
          CALL UPEXIS(IU,PTH,IC)
        ENDIF
        CALL KKTRIM(PTH,PBEG,PEND)
          IF(IC.LT.0) ISTAT = -2
          IF(IC.LT.0 .AND. UE.GE.0) WRITE(UE,1112) IU,PTH(PBEG:PEND)
          IF(IC.LT.0) GO TO 200
          IF(IC.GT.0) ISTAT = IC
          IF(IC.GT.0 .AND. UE.GE.0) WRITE(UE,1113) IU,PTH(PBEG:PEND)
          IF(IC.NE.0) GO TO 200
 
C                         Load array, BUFF, with data from file, PTH
C                         Open and read file, first four long words
C                          (integer*4) are:
C                               1) beginning column number
C                               2) beginning row number
C                               3) number of long words per row
C                               4) number of rows
 
        ISTAT = 0
        IRECL = 0
        CALL UPOPEN(IU,PTH,IRECL,'U',IC)
          IF(IC.NE.0) ISTAT = IC
          IF(IC.NE.0 .AND. UE.GE.0) WRITE(UE,1113) IU,PTH(PBEG:PEND)
          IF(IC.NE.0) GO TO 200
        IF(UU.GE.0) WRITE(UU,1114) IU,PTH(PBEG:PEND)
 
        READ(IU,END=160,ERR=160,IOSTAT=IC) BC,BR,NC,NR

C                         Check if row buffer is big enough

          IF( NC .LE. LRBUFF ) GO TO 114
            IF(UE.GE.0) WRITE(UE,1119) PTH(PBEG:PEND)
            ISTAT = 98
              GO TO 180
  114     CONTINUE

C                         Read data file description so now place any
C                          values obtained that might be in the asked
C                          for output "box" which has been originally
C                          initz -999

C                         Fill out all needed values:
C                          BC, EC ... begin and end column of data file
C                          BR, ER ... begin and end row of data file BC,
C                                      BR is the x-y origin of data file
C                          NC, NR ... num of cols and rows of data file
C                          IBC, IEC . begin and end column desired
C                          IBR, IER . begin and end row of desired outpt
C                                      IBC,IBR is the x-y origin of otpt
C                          INC, INR . num of cols, rows in desired outpt

           EC = BC+NC-1
           ER = BR+NR-1
          IBC = BCOL
          IBR = BROW
          INC = NCOL
          INR = NROW
          IEC = IBC+INC-1
          IER = IBR+INR-1
          
C                         See if desired box overlaps data file box

          IF( IEC.LT.BC .OR. IBC.GT.EC ) GO TO 180
          IF( IER.LT.BR .OR. IBR.GT.ER ) GO TO 180

C                         It does, now see if output array is big enough

          IF( INC*INR .LE. LBUFF ) GO TO 116
            IF(UU.GE.0) WRITE(UE,1118) PTH(PBEG:PEND)
            ISTAT = 99
              GO TO 180
  116     CONTINUE
 
C                         Set COL1, COL2 to begin-end index in RBUFF
C                          input row that corresponds to desired file
C                          column numbers, COLX is offset column in
C                          output grid

          COL1 = 1
          COL2 = NC
          COLX = 1
          IF( IBC .GT. BC ) COL1 = IBC-BC+1
          IF( IEC .LT. EC ) COL2 = IEC-BC+1
          IF( IBC .LT. BC ) COLX = BC-IBC+1

C                         Set ROLX to the number of skipped rows in
C                          output grid due to offset

          ROLX = 0
          IF( IBR .LT. BR ) ROLX = BR-IBR
c
c  Read past the second record in the XMRG file; contains user id and 
c  which version of Stage III created it (manual, auto, or P1);
C  not needed
c
          read(iu,end=160,err=160,iostat=ic)
c
C                         Now loop thru each row in data file to the
C                          file end "ER" or until no more rows are
C                          desired "IER", of course, skip to first
C                          desired row "IBR", COLB becomes the BUFF
C                          array total offset for next row

        ROW = BR
  120   READ(IU,END=160,ERR=160,IOSTAT=IC) (RBUFF(II),II=1,NC)
        
C       This if was changed for bug r27-2 in order to avoid the
C       infinite loop that happens when you don't increase the row
C       counter (Hank Herr, 2005-09-12).
        IF( ROW .LT. IBR ) THEN
          ROW = ROW+1
          GO TO 120
        ENDIF
        
        IF( ROW .GT. IER ) GO TO 180
          COLB = ROLX*INC + COLX - 1
          ROLX = ROLX+1
          DO 124 COLA=COL1,COL2
            COLB = COLB+1
  124       BUFF(COLB) = RBUFF(COLA)
          ROW = ROW+1
          IF( ROW .LE. ER ) GO TO 120
            GO TO 180

C                         Do below if have read error!

  160     IF(IC.NE.0) ISTAT = IC
          IF(IC.NE.0 .AND. UE.GE.0) then
              WRITE(UE,1116) IC,PTH(PBEG:PEND)
              write(UE,1120)
          endif
 
C                         Finished with data file, close it

  180   CALL UPCLOS(IU,PTH,IC)
          IF(IC.NE.0) ISTAT = IC
          IF(IC.NE.0 .AND. UE.GE.0) WRITE(UE,1117) IC,PTH(PBEG:PEND)

  200   CONTINUE

      RETURN
      END
