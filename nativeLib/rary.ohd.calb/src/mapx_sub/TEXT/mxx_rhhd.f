C MODULE MXX_RHHD
C  =====================================================================
C  pgm: MXX_RHHD .. Read header records of hrap grids, opens xmrg file
C  =====================================================================
      SUBROUTINE MXX_RHHD(IUNIT,FNAME,XOR,YOR,MX,MY,IYX,IHED,IFILE)

C  YEAR WHEN ONE MORE RECORD WAS ADDED TO HEADER
      PARAMETER (IY_2REC=1987)

C
C  INPUT PARAMETERS
C	IUNIT - DEVICE NUMBER TO OPEN GRID FILE
C	FNAME - GRIDED FILE NAME
C	IYX - YEAR TO READ DATA
C	IHED - NUMBER OF HEADER RECORDS (DEFAULT IS 1)
C  OUTPUT PARAMETERS
C	XOR - X-ORIGIN LOCATION (HRAP COORDINATES)
C	YOR - Y-ORIGIN LOCATION (HRAP COORDINATES)	
C	MX - NUMBER OF COLUMNS PER ROW
C	MY - NUMBER OF ROWS
C	IFILE - STATUSE OF READING FILE
C  NOTE: NUMBER OF HEADER RECORDS WAS CHNGED DURING 1997. HOWEVER,
C        NOT ALL RFC DID IT AT THE SAME TIME. THIS PROGRAM WILL
C        CHECK IF THE NUMBER OF HEADER RECORDS IS MORE THAN 1, AND
C        IT WILL CHANGE PARAMETER IHED TO BE CONSISTENT WITH
C        AN ACTUAL NUMBER OF RECORDS
C
      INTEGER    XOR, YOR, IHED
      CHARACTER*80 FNAME,FN
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/mapx_sub/RCS/mxx_rhhd.f,v $
     . $',                                                             '
     .$Id: mxx_rhhd.f,v 1.1 2001/06/13 09:27:04 mgm Exp $
     . $' /
C    ===================================================================
C

      lenf = index(fname,' ') -1
      if(lenf.eq.-1) lenfn=80
      fn=fname(1:lenf)
c
      IFILE = 0

C   OPEN FILE AND READ HEADER RECORDS IF 'IHED' .NE. 0
        OPEN(IUNIT,FILE=FN,STATUS='OLD',FORM='UNFORMATTED',ERR=99)
cnew    CALL UPOPEN(IUNIT,FN,0,'U',IERR)
cnew    IF (IERR .NE. 0) GOTO 99

          IF(IHED .GT. 0) THEN
            READ(IUNIT,IOSTAT=IERR) XOR, YOR, MX, MY
            IF (IERR .LT. 0) GOTO 99
            IF (IERR .GT. 0) GOTO 77

            IF(XOR .GT. 1500 .OR. XOR .LT. 0) GOTO 77
            IF(YOR .GT. 1500 .OR. YOR .LT. 0) GOTO 77
            IF(MX .GT. 1500 .OR. MX .LT. 0) GOTO 77
            IF(MY .GT. 1500 .OR. MY .LT. 0) GOTO 77
            IF(IHED .EQ. 1 .AND. IYX .GE. IY_2REC) THEN
              DO I=1,MY+1
                READ(IUNIT,IOSTAT=IERR)
                IF (IERR .LT. 0) GOTO 11
                IF (IERR .GT. 0) GOTO 77
              ENDDO
              IHED=2
11            REWIND (IUNIT)
              READ(IUNIT,IOSTAT=IERR) XOR, YOR, MX, MY
              IF (IERR .LT. 0) GOTO 99
              IF (IERR .GT. 0) GOTO 77

              IF(XOR .GT. 1500 .OR. XOR .LT. 0) GOTO 77
              IF(YOR .GT. 1500 .OR. YOR .LT. 0) GOTO 77
              IF(MX .GT. 1500 .OR. MX .LT. 0) GOTO 77
              IF(MY .GT. 1500 .OR. MY .LT. 0) GOTO 77
            ENDIF

            DO I=2,IHED
              READ(IUNIT,IOSTAT=IERR)
              IF (IERR .NE. 0) GOTO 99
            ENDDO

          ELSE
            GOTO 99
          ENDIF

      RETURN
 99   IFILE = -1
      RETURN
 77   IFILE = -7
      RETURN
      END
