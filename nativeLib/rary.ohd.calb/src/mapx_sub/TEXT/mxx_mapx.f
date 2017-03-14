C$PRAGMA C (MXX_V2OH)
C MODULE MXX_MAPX
C  =====================================================================
C  pgm: MXX_MAPX .. Top subroutine to run the MAPX using NEXRAD files
C
C  cmt: NOTE: Format statement 10 has a hardwired limit to the number
C  cmt:       of basins that can be used during output to the temporary
C  cmt:       file in string INFTMP (updated to 200 May 2005).
C  =====================================================================
      SUBROUTINE MXX_MAPX(LP,ISTART,IEND,MAXBSN,NAREA,BASINS,
     +                    FLLATL,GRD_DIR,PREF,NYY,DT,ICV,DIR_MAP,RMISS,
     +                    FLMISS,OUTUNIT,NCOL,XTIME,ISTAT)
      
      EXTERNAL        KKLAST
      INTEGER         KKLAST

      PARAMETER (MAXCOL=1000)
      PARAMETER (MAXSGM=10000)

C  BASIC LENGTH OF DATE+z IN XMRG FILE: 
C   For MMDDYYHHz   use LXMRGDAT=9
C   For MMDDYYYYHHz use LXMRGDAT=11
      
      CHARACTER*50    TEXT
      CHARACTER*(*)   FLLATL,GRD_DIR,FLMISS
      CHARACTER*128   GRD_FLZ,BSNAME
      CHARACTER*128   GRD_READ,GRD_YY,GRD_YYYY
      CHARACTER*128   XMRG_LST
      CHARACTER*300   COM_LST
      CHARACTER*128   OUTFILE
      CHARACTER*128   INFTMP,INFCV,INFZP,INFLS
      CHARACTER*140   INTEMP
      CHARACTER*300   COM_IN
      CHARACTER*128   UNCMP,FLMAPX,DIR_MAP,FLMAPCV,FLZEROP
      CHARACTER*1     PREF*32,CC*4, CCX*4,FORMAT*6,COLUMNS*2,CT*2
      CHARACTER*8     BASINS(*)
      CHARACTER*(*)   XTIME
      CHARACTER*3     OUTUNIT,OUTUNX
      CHARACTER*4     TYMAPX
      CHARACTER*3     TYCVX
      CHARACTER*3     TYZPX
      CHARACTER*16    TMPGRD
      CHARACTER*200   LIN
      INTEGER    UGRD,UMISS,ISTAT,RMISS,JSTAT
      INTEGER    ISTART,IEND,DT,ICC(3)
      INTEGER*2  GRDPCP(MAXCOL)
      INTEGER    XOR,YOR,NX,NY
      INTEGER    SEG_BND(3,MAXSGM),NSGM(MAXBSN),NSUM(MAXBSN)
      REAL       MAPX(MAXBSN),MAPCV(MAXBSN),ZEROP(MAXBSN)
      REAL       SMAPX(MAXBSN),SMAPCV(MAXBSN),SZEROP(MAXBSN)
      INTEGER         JSTNUM
      CHARACTER*8     JSTBSN(MAXBSN)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_mapx.f,v $
     . $',                                                             '
     .$Id: mxx_mapx.f,v 1.5 2005/06/09 19:27:50 dws Exp $
     . $' /
C    ===================================================================
C

      DATA  INFTMP / '/var/tmp/mapx_tmp.out' /
      DATA  INFCV  / '/var/tmp/mapx_cv.out'  /
      DATA  INFZP  / '/var/tmp/mapx_zp.out'  /
      DATA  TYMAPX / 'mapx' /
      DATA  TYCVX  / 'cvx'  /
      DATA  TYZPX  / 'zpx'  /
      DATA  INFLS  / '/var/tmp/temp2'        /
      DATA  TMPGRD / '/var/tmp/grd_tmp'      /

        CALL WLIN('B',' ')
        CALL WLIN('M','  ========>  BEGIN PROCESSING MAPX RUN')

        ISTAT = 0

C  Get begin/end hours from 1900 for ISTART/IEND as:
C    1) NSTART/NEND for begin to end of whole months
C    2) N1/N2 for begin to end of whole days

        CALL MXX_STIM(ISTART,IEND,NSTART,NEND,N1,N2,IERRS,IERRE)
        CALL MXX_STER(ISTART,IEND,IERRS,IERRE,IER)
            IF (IER .NE. 0) THEN
              ISTAT = 3
              GOTO 777
            ENDIF

        UGRD  = 80
        UMISS = 81
        CALL UPOPEN(UMISS,FLMISS,0,'F',IERR)
        IF (IERR .NE. 0) THEN
          FLMISS = '/var/tmp/temp.MISSED_LOG'
          CALL UPOPEN(UMISS,FLMISS,0,'F',IERR)
          IF (IERR .EQ. 0) THEN
            LIN = ' '
            WRITE(LIN,'(A,A)',IOSTAT=JE) 'missed_log file is: ',FLMISS
            IF (JE.EQ.0) CALL WLIN('C',LIN)
          ENDIF
        ENDIF
        IF (IERR .NE. 0) ISTAT = 5
        IF (ISTAT .NE. 0) GOTO 777
      
        CALL MXX_RHRP(FLLATL,SEG_BND,NSGM,BASINS,MAXBSN,NAREA,
     $                NUSED,LNUM,LP,JSTAT,JSTNUM,JSTBSN)

C           LNUM is the total number of lines in file FLLATL that were
C            read by routine MXX_RHRP (plus one if all are read).

        CALL MXX_RHE1(FLLATL,LNUM,JSTAT)
        CALL MXX_RHE2(FLLATL,NAREA,NUSED,MAXBSN,BASINS,JSTAT,
     $                JSTNUM,JSTBSN)

        IF (JSTAT .GT. 0) ISTAT = JSTAT
        IF (ISTAT .GT. 0) GOTO 777

        IF (NAREA .GT. NUSED) NAREA = NUSED

c  ****************************************************************
c        Option to generate only MAPX card format 
c        if mapx_tmp file was generated before and exists
c        To run this option, uncomment statement 'goto 999'
c 
c       goto 999
c
c  **************************************************************** 
      
C  GET MAX AND MIN SEGMENT NUMBER FROM ALL AREAS
      MAXROW=0
      MINROW=9999
      IF (NSGM(NAREA).GT.0 .AND. NSGM(NAREA).LE.MAXSGM) THEN
        DO I=1,NSGM(NAREA)
         IF(SEG_BND(1,I) .LE. MINROW) MINROW=SEG_BND(1,I)
         IF(SEG_BND(1,I) .GE. MAXROW) MAXROW=SEG_BND(1,I)
        ENDDO
      ENDIF

C  OPEN TEMPORAL FILES TO STORE MAPX (MAPCV & ZEROP) OVER ALL AREAS
C  EACH RECORD OF FILE INCLUDE: YEAR, MONTH, DATE, HOUR
C	AND MAPX OVER NAREAS PER ONE TIME STEP
C  FILES WILL BE DELETED AT THE END OF RUN  
C
C         DIR_MAP is the output dir for mapx01 files, etc.
C         GRD_DIR is the directory for xmrg files.

      IHED = 1
      LFL  = KKLAST(0,DIR_MAP)
      LDIR = KKLAST(0,GRD_DIR)
CC    IF (LDIR .GT. 80) LDIR = 80
      IUN10 = 10
      CALL UPOPEN(IUN10,INFTMP,0,'F',JE)

C  WRITE HEADER RECORDS            
      WRITE(IUN10,'(A,A)') '$MAPX FROM FILES: ',GRD_DIR(1:LDIR)
      WRITE(IUN10,'(A,I3,A,A)') 'NEXRAD      MAP   L   MM ',DT,
     +         '              ','   ' 
      IF(ICV .NE. 0) THEN
       IUN11 = 11
       IUN12 = 12
       CALL UPOPEN(IUN11,INFCV,0,'F',JE)
       CALL UPOPEN(IUN12,INFZP,0,'F',JE)
      WRITE(IUN11,'(A,A)') '$MAPC FROM FILES: ',GRD_DIR(1:LDIR)
      WRITE(IUN11,'(A,I3,A,A)') 'NEXRAD    CV   DLS   DLS ',DT,
     +         '              ','   ' 
      WRITE(IUN12,'(A,A)') '$MAPZ FROM FILES: ',GRD_DIR(1:LDIR)
      WRITE(IUN12,'(A,I3,A,A)') 'NEXRAD     ZP  DLS   DLS ',DT,
     +         '              ','   '            
      ENDIF                
            
      CALL WLIN('B',' ')
      CALL WLIN('M','      ====>  PROCESSING NEXRAD GRIDS')

      DO J=1,NAREA
       SMAPX(J)=0.
       SMAPCV(J)=0.
       SZEROP(J)=0.
      ENDDO 

C  BASIC LENGTH OF DATE+z IN XMRG FILE:
C   For YY (MMDDYYHHz)   NYY is 2,     for YYYY   NYY is 4
        LXMRGDAT=7+NYY

C  DEFINE BASIC LENGTH OF XMRG FILE DATE (MMDDYYHHz)
      LNP     = KKLAST(0,PREF)
      LN_XMRG = LNP+LXMRGDAT
      LN_DIR  = KKLAST(1,GRD_DIR)
C      I2000=0

C   START TIME LOOP OF MAPX CALCULATIONS <><><><><><><><><><><><><><><>
      NMX=-999

CDT      DO I=NSTART,NEND,DT
CC              this do ends at statement number 55

      DO I=NSTART,NEND,1
       
       IFLAG=-1
       ITIME=I
       CALL DDGHC2(ITIME,IYX,IMX,IDX,IHRX)
       IMW=IMX
       IDW=IDX
       IYW=IYX
       IHRW=IHRX
       
C   SHIFT DATE ONE HOUR AHEAD IF IHRX = 0 BECAUSE IN NEXRAD IT'S 24 HOURS       
CDT       IF(IHRX .EQ. 0 .AND. DT .NE. 24) THEN
       IF(IHRX .EQ. 0) THEN
        ITIME=I-1
        CALL DDGHC2(ITIME,IYW,IMW,IDW,IHRW)
        IHRW=IHRW+1
        IF(I .EQ. N1 ) GOTO 55
       ENDIF
       
C   CHECK IF START-END IS OUTSIDE SELECTED DATES TO RUN
       IF(I .LT. N1 .OR. I .GT. N2) THEN
        DO II=1,NAREA
         MAPX(II)=-999.
         MAPCV(II)=-999.
         ZEROP(II)=-999.
        ENDDO
         GOTO 103
       ENDIF
                     
C  SELECT ALL XMRG FILE NAMES FOR THE MONTH INTO temp FILE

       IF(IMX .NE. NMX) THEN
        IF(NMX .NE. -999) CLOSE (1)
        NYX=IYX
        LYEAR=4
        IF(LXMRGDAT .EQ. 9) THEN
          LYEAR=2
          IF(IYX .LT. 2000) THEN
            NYX=IYX-1900
          ELSE
            NYX=IYX-2000
          ENDIF
          WRITE(CC,'(I2.2)',IOSTAT=IERR) NYX
        ELSE
          WRITE(CC,'(I4.4)',IOSTAT=IERR) NYX
        ENDIF   
        WRITE(CT,'(I2.2)',IOSTAT=IERR) IMX

C         PREF is the xmrg file prefix, CT is month, CC is year so
C          XMRG_LST may be something like "xmrg03..2005*"

        XMRG_LST=PREF(1:KKLAST(1,PREF))//CT//'..'//CC(1:LYEAR)//'*'

        IUN13 = 13
        CALL UPOPEN(IUN13,INFLS,0,'F',JE)

        COM_LST = 'cd ' //
     $            GRD_DIR(1:KKLAST(1,GRD_DIR)) //
     $            '; ls -1 ' //
     $            '| sed -n "/' //
     $            XMRG_LST(1:KKLAST(1,XMRG_LST)) //
     $            '/p"' //
     $            ' 2>&- > ' //
     $            INFLS

        CALL SYSTEM(COM_LST)
        REWIND (IUN13)
        NMX=IMW
       ENDIF                        

C   *******  GENERATE PATH OF GRID FILES  ************************

C   READ XMRG FILES FROM temp FILE
666   READ(IUN13,'(A)',END=664) TEXT
      LNFILE = KKLAST(1,TEXT)

C  GENERATE NAME WITH SIMULATION DATE
      LSTRT=LNP+3
      IF(LYEAR .EQ. 2) THEN
       CALL PAGRID('o',GRD_YY,'',PREF,IYX,IMX,IDX,IHRX)
       LEND=LSTRT+5
      ELSE
       CALL PAGRID('b',GRD_YY,'',PREF,IYX,IMX,IDX,IHRX)
       LEND=LSTRT+7
      ENDIF
               
C  CHECK IF XMRG FILE DATE IS <, ==, OR > SIMULATION DATE
      IF(TEXT(LSTRT:LEND) .LT. GRD_YY(LSTRT+1:LEND+1)) GOTO 666
      IF(TEXT(LSTRT:LEND) .GT. GRD_YY(LSTRT+1:LEND+1)) THEN
       IFLAG=-1
      ELSE
       IFLAG=0

       CALL MXX_FLTY(GRD_DIR,TEXT,TMPGRD,GRD_READ,KSTAT)

       IF (KSTAT .EQ. 3) THEN
         LIN = ' '
         WRITE(LIN,'(A,A)',IOSTAT=JE) 'not XMRG file: ',TEXT(1:LNFILE)
         IF (JE.EQ.0) CALL WLIN('E',LIN)
         IFLAG=-1
       ELSEIF (KSTAT .GT. 0) THEN
         IFLAG=-1
       ENDIF

      ENDIF                       
         
  664 CONTINUE

C   READ HEADER OF NEXRAD FILE
      IF(IFLAG .NE. 0) THEN
       WRITE(UMISS,100) ' NO FILE ON DATE:',IYX,'/',IMX,'/',IDX,':',IHRX
100    FORMAT(A17,I5,A,I2,A,I2,A,I2)         
C   ZERO OR -999. MAPX VALUES WILL BE ASSIGNED TO ALL AREAS         
      ELSE
       CALL MXX_RHHD(UGRD,GRD_READ,XOR,YOR,NX,NY,IYX,IHED,IFLAG)
       IF(IFLAG .EQ. -7) WRITE(UMISS,102) ' HEADER ERROR ON DATE:',
     +                     IYX,'/',IMX,'/',IDX,':',IHRX,
     +              ' ::XOR=',XOR,' YOR=',YOR,' NX=',NX,' NY=',NY
102      FORMAT(A22,I5,A,I2,A,I2,A,I2,A7,I9,A5,I9,A4,I9,A4,I9)      
      ENDIF
 
C   CALL SUBROUTINE TO CALCULATE MAPX OVER ALL AREAS
30       CALL MXX_CALC(UGRD,SEG_BND,MAXBSN,NAREA,NSGM,MAXROW,MINROW,
     +                 XOR,YOR,NX,NY,GRDPCP,MAPX,NSUM,
     +                 MAPCV,ZEROP,IFLAG)
       IF(IFLAG .EQ. 99) 
     +          WRITE(UMISS,101) ' ERROR: END OF HRAP FILE ON DATE:',
     +                        IYX,'/',IMX,'/',IDX,':',IHRX
                
101    FORMAT(A32,1X,I4.4,A,I2.2,A,I2.2,A,I2.2)
           
       CLOSE(UGRD)
Cnew   CALL UPCLOS(UGRD,' ',IERR)

C   REMOVE TEMPORARY FILE IF IT WAS CREATED
       IF (KSTAT .LT. 0) THEN
         CALL SYSTEM('rm -f ' // TMPGRD)
       ENDIF        
       
C  CUMULATE MAPX, CV, AND ZP PER TIME INTERVAL DT
103   DO J=1,NAREA
       IF(MAPX(J) .GE. 0. .AND. SMAPX(J) .GE. 0.) THEN
        SMAPX(J)=SMAPX(J)+MAPX(J)
        SMAPCV(J)=SMAPCV(J)+MAPCV(J)
        SZEROP(J)=SZEROP(J)+ZEROP(J)
       ELSE
        IF(MAPX(J) .NE. -999.) THEN
         SMAPX(J)=-99.
         SMAPCV(J)=-99.
         SZEROP(J)=-99.
        ELSE
         SMAPX(J)=-999.
         SMAPCV(J)=-999.
         SZEROP(J)=-999.
        ENDIF 
       ENDIF 
      ENDDO
      
C  WRITE DATA INTO TEMPORAL FILES AT DESIRED TIME INTERVAL DT
       IF(MOD(I-NSTART+1,DT) .EQ. 0.) THEN
        WRITE(IUN10,10) IYW,IMW,IDW,IHRW,(SMAPX(JJ),JJ=1,NAREA)

        IF(ICV .NE. 0) THEN
         DO JJ=1,NAREA
          IF(SMAPCV(JJ) .GE. 0.) THEN
           SMAPCV(JJ)=SMAPCV(JJ)/DT
           SZEROP(JJ)=SZEROP(JJ)/DT
          ENDIF
         ENDDO 
         WRITE(IUN11,10) IYW,IMW,IDW,IHRW,(SMAPCV(JJ),JJ=1,NAREA)
         WRITE(IUN12,10) IYW,IMW,IDW,IHRW,(SZEROP(JJ),JJ=1,NAREA)
        ENDIF
       
        DO J=1,NAREA
         SMAPX(J)=0.
         SMAPCV(J)=0.
         SZEROP(J)=0.
        ENDDO
       ENDIF
10     FORMAT(I4,3I3,200F9.3)       
55    ENDDO
C                  <><><><><><><><><><><><><><><><><><><><><><><><><>

      CALL UPCLOS(IUN10,' ',JE)
      IF(ICV .NE. 0) THEN
        CALL UPCLOS(IUN11,' ',JE)
        CALL UPCLOS(IUN12,' ',JE)
      ENDIF 

999   CONTINUE
      CALL WLIN('B',' ')
      CALL WLIN('M','      ====>  GENERATE CARD FORMAT FILES')

C  WRITE MAPX (STORED TEMPORARYLY IN FILE 'mapx_tmp.out') INTO
C  PERMANENT FILES IN CARD FORMAT FOR EACH AREA SEPARATELY
C

      WRITE(CT,'(I2.2)',IOSTAT=IERR) DT
      CALL KKI2AP(NCOL,COLUMNS,LCOL) 

C  LOOP TROUGH ALL SELECTED BASINS
      LFL = KKLAST(1,DIR_MAP)
      DO I=1,NAREA
       CALL KKI2AP(I,CC,LCC)
       LID    = KKLAST(1,BASINS(I))
       LENTH  = KKLAST(1,TYMAPX)
       FLMAPX = DIR_MAP(1:LFL) // '/' // BASINS(I)(1:LID) //
     $          '.' // TYMAPX(1:LENTH) // CT

C  CHECK IF MAPX FILE EXIST
        CALL MXX_EXTN(BASINS(I),FLMAPX,XTIME,JE)
        LOUT = KKLAST(1,FLMAPX)
        
        LFIL = KKLAST(1,INFTMP)
        OUTFILE = FLMAPX(1:LOUT)
        FORMAT='%9.3f'
        LF=5
        CCX=CC(1:LCC)
        BSNAME=BASINS(I)(1:LID)
        LU = KKLAST(0,OUTUNIT)
         
      XRMISS = RMISS

      IFLG=MXX_V2OH(%REF(INFTMP),%VAL(LFIL), %REF(OUTFILE),%VAL(LOUT),
     +             %REF(FORMAT),%VAL(LF), %REF(COLUMNS),%VAL(LCOL),
     +             %REF(CCX),%VAL(LCC), %REF(OUTUNIT),%VAL(LU),
     +             %REF(BSNAME),%VAL(LID),%VAL(XRMISS))

        CALL MXX_WPTH(TYMAPX,CT,FLMAPX)

       LIN = ' '
       WRITE(LIN,'(A,A)',IOSTAT=JE)
     $   '      ====>  TIME SERIES GENERATED FOR: ',BSNAME(1:LID)
       IF (JE.EQ.0) CALL WLIN('M',LIN)

C  GENERATE CARD FORMAT FILES OF CV AND ZP
      IF(ICV .NE. 0) THEN                  
        LENTH  = KKLAST(1,TYCVX)
        FLMAPCV = DIR_MAP(1:LFL) // '/' // BASINS(I)(1:LID) //
     $            '.' // TYCVX(1:LENTH) // CT

C  CHECK IF MAPX FILE EXIST        
        CALL MXX_EXTN(BASINS(I),FLMAPCV,XTIME,JE)
        LOUT = KKLAST(1,FLMAPCV)

        LFIL = KKLAST(1,INFCV)
        OUTFILE = FLMAPCV(1:LOUT)
        OUTUNX='DLS'
        LUX=3
        XRMISS = RMISS
        IFLG=MXX_V2OH(%REF(INFCV),%VAL(LFIL), %REF(OUTFILE),%VAL(LOUT),
     +             %REF(FORMAT),%VAL(LF), %REF(COLUMNS),%VAL(LCOL),
     +             %REF(CCX),%VAL(LCC), %REF(OUTUNX),%VAL(LUX),
     +             %REF(BSNAME),%VAL(LID),%VAL(XRMISS))

        CALL MXX_WPTH(TYCVX,CT,FLMAPCV)

        LENTH  = KKLAST(1,TYZPX)
        FLZEROP = DIR_MAP(1:LFL) // '/' // BASINS(I)(1:LID) //
     $            '.' // TYZPX // CT

C  CHECK IF MAPX FILE EXIST        
        CALL MXX_EXTN(BASINS(I),FLZEROP,XTIME,JE)
        LOUT = KKLAST(1,FLZEROP)

        LFIL = KKLAST(1,INFZP)
        OUTFILE = FLZEROP(1:LOUT)
        XRMISS = RMISS
        IFLG=MXX_V2OH(%REF(INFZP),%VAL(LFIL), %REF(OUTFILE),%VAL(LOUT),
     +             %REF(FORMAT),%VAL(LF), %REF(COLUMNS),%VAL(LCOL),
     +             %REF(CCX),%VAL(LCC), %REF(OUTUNX),%VAL(LUX),
     +             %REF(BSNAME),%VAL(LID),%VAL(XRMISS))

        CALL MXX_WPTH(TYZPX,CT,FLZEROP)
       ENDIF
                       
      ENDDO

      CALL UPCLOS(IUN13,INFLS,JE)
      CALL UPDELE(IUN13,INFLS,JE)

      CALL UPDELE(IUN10,INFTMP,JE)
      IF(ICV .NE. 0) THEN
        CALL UPDELE(IUN11,INFCV,JE)
        CALL UPDELE(IUN12,INFZP,JE)
      ENDIF

  777   CONTINUE

        CALL WLIN('M','  ========>  END PROCESSING MAPX RUN')
        CALL WLIN('B',' ')

      RETURN
      END
