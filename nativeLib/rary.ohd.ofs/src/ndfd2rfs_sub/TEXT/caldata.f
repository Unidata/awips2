C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (SETUPAPI)
C23456---1---------2---------3---------4---------5---------6---------712
C
      SUBROUTINE CALDATA(BASNID,NGRID,
     &           VALTIME1,LENTIME1,TSTEMP,
     &           VALTIME2,LENTIME2,TSMAXT,
     &           VALTIME3,LENTIME3,TSMINT,
     &           NALLBASN,NALLGRID,
     &           KK,IYEAR,JULDAY,IHOUR,TS)
C
C-----------------------------------------------------------------------
C     This program were going to write the future data which extracted
C     from NDFD to process database of NWS
C     INPUT:
C       BASNID(*)   : BASIN ID
C       NGRID(*)    : # OF GRIDS FOR EVERY BASNIM
C       VALTIME1(*) : TIMESTEP FOR TEMP DATA 
C       LENTIME1    : # OF VALTIME1 
C       VALTIME2(*) : TIMESTEP FOR TMAX DATA 
C       TSTEMP(*,*) : TEMP DATA FOR EVERY GRID AND EVERY TIMESTEP
C       LENTIME2    : # OF VALTIME2 
C       TSMAXT(*,*) : TMAX DATA FOR EVERY GRID AND EVERY TIMESTEP
C       VALTIME3(*) : TIMESTEP FOR TMIN DATA 
C       LENTIME3    : # OF VALTIME3 
C       TSMINT(*,*) : TMIN DATA FOR EVERY GRID AND EVERY TIMESTEP
C       NALLBASN    : TOTAL BASINS
C       NALLGRID    : TOTAL GRIDS
C     OUTPUT:
C       KK          : # OF TSTEMP6
C       IYEAR       : IYEAR
C       JULDAY      : JULDAY
C       IHOUR       : IHOUR
C       TS(*,*)     : AVERAGED TEMP DATA OVER NGRID(I) OF BASNID(I) 
C
      PARAMETER (LARRAY=60000)
      PARAMETER (MARRAY=600)
      PARAMETER (NARRAY=60)
      CHARACTER*8 BASNID(LARRAY)
      CHARACTER*8 TMPMATID(LARRAY)                        !cfan 2007/09
      CHARACTER *4 TSTYPE, TSUNIT
      CHARACTER*12  VALTIME1(NARRAY),VALTIME2(NARRAY),VALTIME3(NARRAY)
      REAL*8  DATA(LARRAY,NARRAY)
      INTEGER  NGRID(LARRAY)
      REAL*8    TSTEMP(NARRAY,LARRAY),
     &          TSTEMP3(NARRAY,LARRAY),TSTEMP6(NARRAY,LARRAY),
     &          TSMAXT(NARRAY,LARRAY),TSMINT(NARRAY,LARRAY)
      REAL      TS(LARRAY,MARRAY)
C     TSTEMP : every 3 hours, out to 72 hours, every 6 hours out to 168 hours
C     TSTEMP3: every 3 hours, out to 168 hours
C     TSTEMP6: every 6 hours, out to 168 hours
C     TSMAXT : every 24 hours, out to 168 hours
C     TSMINT : every 24 hours, out to 168 hours
C     TS     : modified every 6 hours, upto 168 hours
      INTEGER ISWAP(1)
      INTRINSIC MAXLOC, MINLOC
      CHARACTER*200 LINE,VAR_NAME,VAR_VALUE
c     character*4 time_zone
      COMMON/TMPMATID/TMPMATID                           !cfan 2007/09

      INCLUDE 'common/fcdflt'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'uunits'
c      COMMON /HDFLTS/ IHDFLT(25)
      INCLUDE 'common/x'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sugnlx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_sub/RCS/caldata.f,v $
     . $',                                                             '
     .$Id: caldata.f,v 1.3 2004/11/02 20:18:02 xfan Exp $
     . $' /
C    ===================================================================
C

      TSTYPE= 'MAT '
      TSUNIT= 'DEGF'

C
C     MAXLOC, MINLOC is a FORTRAN 90 function that returns the index
C             value for the maximum and minimum element is the array
C
C    ===================================================================
C

c     call get_ofs_default_tzc(time_zone)

   2  CONTINUE

C      WRITE(*,*)'NALLBASN, NALLGRID====',NALLBASN, NALLGRID
C      do i=1,NALLBASN
C      write(*,*)BASNID(I),ngrid(i)
C      enddo
c      DO I=1,41
c      WRITE(*,'(I5,3(I5,2x,A12)')I,LENTIME1,VALTIME1(I),
c     &    LENTIME2,VALTIME2(I),LENTIME3,VALTIME3(I)
c      ENDDO

          READ(VALTIME1(1),'(I4,3I2)',ERR=22)
     &       IYEAR0,IMONTH0,IDAY0,IHOUR0
          READ(VALTIME1(LENTIME1),'(I4,3I2)',ERR=22)
     &       IYEAR1,IMONTH1,IDAY1,IHOUR1
C
          JULDAY=0
          IF (IHOUR0/6*6 .EQ. IHOUR0) THEN
           IHOUR=IHOUR0
           IDAY=IDAY0
          ELSE
           IHOUR=IHOUR0+3
           IDAY=IDAY0
           IF (IHOUR .EQ. 24) THEN
             IHOUR=0
             IDAY=IDAY0+1
             JULDAY=1
           ENDIF
          ENDIF

          DO I=1,IMONTH0-1
           CALL DAYS_OF_MONTH(I,IYEAR0,IDAY00)
           JULDAY=JULDAY+IDAY00
          ENDDO
          IYEAR = IYEAR0 - 1900
          JULDAY= JULDAY + IDAY0

          CALL CALDATE(JULDAY1,IMONTH1,IDAY1,IYEAR1,1)

          KK=((JULDAY1-1)*24+IHOUR1-(JULDAY-1)*24-IHOUR)/6+1

c        WRITE(LINE,'(I4)')IYEAR0
c        CALL WRITEMSGF(57344+128+0,' IYEAR ='//LINE(1:4))
c        WRITE(LINE,'(I4)')IMONTH0
c        CALL WRITEMSGF(57344+128+0,' IMONTH='//LINE(1:4))
c        WRITE(LINE,'(I4)')IDAY0
c        CALL WRITEMSGF(57344+128+0,' IDAY  ='//LINE(1:4))
c        WRITE(LINE,'(I4)')IHOUR0
c        CALL WRITEMSGF(57344+128+0,' IHOUR ='//LINE(1:4))

C INITIAL TSTEMP for all HRAP's grids for all BASNs
C TSTEMP will always be 
C  a minimum of 34 (a forecast starting at 21Z) or
C  a maximum of 41 (a forecast starting at 0Z)
C forecast values for each HRAP pixel
C For example:
C        x|xxxxxxx|xxxxxxx|-x-x-x-|-x-x-x-|-x-x-x-|-x-x-x-| (34)
C ......
C !xxxxxxx|xxxxxxx|xxxxxxx|-x-x-x-|-x-x-x-|-x-x-x-|-x-x-x-| (41)
C
C LENTIME1 = 35 (when IHOUR=21Z) to 41 (when IHOUR=0Z)
C LEN3TIME = LENTIME1 + 16 
C
C Get TSTEMP3 by interpolate every 6 hour data of the last 4 days

      LEN3TIME = LENTIME1 + 16

      DO J=1,NALLGRID
      DO K=1,LENTIME1-16
      TSTEMP3(K,J)=TSTEMP(K,J)
      ENDDO
      DO K=1,16
      TSTEMP3(LENTIME1-16+(K-1)*2+1,J)
     &      =0.5*(TSTEMP(LENTIME1-16+K-1,J)+TSTEMP(LENTIME1-16+K,J))
      TSTEMP3(LENTIME1-16+(K-1)*2+2,J)
     &      =TSTEMP(LENTIME1-16+K,J)
      ENDDO
      ENDDO

C Get Tmax and Tmin to adjust TS3
C Tmax will have 7 or 8 forecast values
C Tmin will have 6 or 7 forecast values
C For example (Tmax):
C        xXxxxxxxxXxxxxxxxX-x-x-x-X-x-x-x-X-x-x-x-X-x-x-x-X (7)
C ......
C XxxxxxxxXxxxxxxxXxxxxxxxX-x-x-x-X-x-x-x-X-x-x-x-X-x-x-x-X (8)
C For example (Tmin):
C        x|xxxXxxx|xxxXxxx|-x-X-x-|-x-X-x-|-x-X-x-|-x-X-x-| (6)
C ......    
C !xxxXxxx|xxxXxxx|xxxXxxx|-x-X-x-|-x-X-x-|-x-X-x-|-x-X-x-| (7)

C      WRITE(*,*)'TSMAX====',LENTIME2,  (TSMAXT(ii,1),II=1,LENTIME2)
C      WRITE(*,*)'TMIN=====',LENTIME3,  (TSMINT(ii,1),II=1,LENTIME3)

C Get TSTEMP6 by interpolate every 3 hour data TSTEMP3 using weight 1/4,1/2,1/4

      DO J=1,NALLGRID
      IF (IHOUR0/6*6 .EQ. IHOUR0) THEN
C
C  0-----3-----6-----9----12----15----18----21-----0
C  1     0
C  1/4   1/2   1/4    ......          1/4   1/2   1/4
C

C  KK     = 25    26     27     28     29   when
C  IHOUR0 = 21    18&15  12&9   6&3    0

        TSTEMP6(1,J)=TSTEMP3(1,J)
        DO K=2,KK
        TSTEMP6(K,J)=0.25*TSTEMP3(2*K-3,J)
     &              +0.50*TSTEMP3(2*K-2,J)+0.25*TSTEMP3(2*K-1,J)
        ENDDO
      ELSE
C
C  3-----6-----9----12----15----18----21-----0
C  1/2   1/2
C        1/4   1/2  1/4 ......  1/4   1/2   1/4
C
        TSTEMP6(1,J)=0.50*TSTEMP3(1,J)+0.50*TSTEMP3(2,J)
        DO K=2,KK
        TSTEMP6(K,J)=0.25*TSTEMP3(2*K-2,J)
     &              +0.50*TSTEMP3(2*K-1,J)+0.25*TSTEMP3(2*K,J)
        ENDDO
      ENDIF
      ENDDO

c      GO TO 1111    !test for no adjustment
c      DO JJ=1,KK
c       WRITE(*,*)JJ,TSTEMP6(JJ,1),KK
c      ENDDO
C Adjust 6 hour average TSTEMP6 by TSMAXT and TSMINT
C Don't include any incomplete 24 hour periods
C For first  72 hours,
C   IF TSMAXT > TSTEMP6, adjust TSTEMP6: 
C      0.75*TSTEMP6+0.25*TSMAXT/0.75*TSTEMP6+0.25*TSMINT
C For beyond 72 hours,
C   IF TSMINT < TSTEMP6, adjust TSTEMP6: 
C      0.50*TSTEMP6+0.50*TSMAXT/0.50*TSTEMP6+0.50*TSMINT
C
C X-----6-----x-----18-----X-----6-----x-----18-----X
C
C 0-----6-----N-----18-----0-----6-----N-----18-----0
C
C max    max     max     max     max     max     max    max     
C       kk-24   kk-20   kk-16   kk-12   kk-8    kk-4     kk
C |-+-+-+-|---+---|---+---|---+---|---+---|---+---|---+---|
C           kk-22   kk-18   kk-14   kk-10   kk-6    kk-2 
C    min     min     min     min     min      min    min

      DO J=1,NALLGRID
        IF (IHOUR0 .LE. 18 .AND. IHOUR0 .GT. 0) THEN
C
C IHOUR<=18, it means the second day has both MAX and MIN
C For MAX
        DO JJ=KK-24,KK-16,4     !first upto 72 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMAXT(JJ/4+1,J),
     &                  0.75,0.25,1,IERR)
        ENDDO
        DO JJ=KK-12,KK,4        !upto 168 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMAXT(JJ/4+1,J),
     &                  0.50,0.50,1,IERR)
        ENDDO
C For MIN
        DO JJ=KK-22,KK-18,4     !first upto 72 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMINT(JJ/4+1,J),
     &                  0.75,0.25,0,IERR)
        ENDDO
        DO JJ=KK-14,KK-2,4        !upto 168 hours
        CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMINT(jj/4+1,J),
     &                 0.50,0.50,0,IERR)
        ENDDO
        ELSE
C IHOUR>18, it means the second day doesn't have both MAX and MIN
C For MAX
         CALL GETMAXMIN(TSTEMP6(1,J),1,TSMAXT(1,J),
     &                  0.75,0.25,1,IERR)
        DO JJ=KK-20,KK-16,4     !first upto 72 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMAXT(JJ/4+1,J),
     &                  0.75,0.25,1,IERR)
        ENDDO
        DO JJ=KK-12,KK,4        !upto 168 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMAXT(JJ/4+1,J),
     &                  0.50,0.50,1,IERR)
        ENDDO
C For MIN
        DO JJ=KK-22,KK-18,4     !first upto 72 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMINT(JJ/4+1,J),
     &                  0.75,0.25,0,IERR)
        ENDDO
        DO JJ=KK-14,KK-2,4        !upto 168 hours
         CALL GETMAXMIN(TSTEMP6(JJ,J),2,TSMINT(jj/4+1,J),
     &                  0.50,0.50,0,IERR)
        ENDDO
        ENDIF
      ENDDO

c1111  CONTINUE 
c      DO JJ=1,KK
c       WRITE(*,*)JJ,TSTEMP6(JJ,1),KK
c      ENDDO

C Get averaged value for every BASNID(I) over NGRID(I) grids

      DO I=1,NALLBASN        !-------------i=1,nallbasn

        IF (I.EQ.1) THEN
          JBEGIN=1
          JEND=NGRID(1)
        ELSE
          JBEGIN=NGRID(I-1)+JBEGIN
          JEND  =NGRID(I)+JEND
        ENDIF

        WRITE(LINE,'(I5,2x,A8,30H  data processed successfully!)')
CFAN &        I,BASNID(I)
C
C       There is no difference when BASNID is the same as MATID.
C       If the basin is divided into LWR and UPR, it should be
C       shown MATID with LWR here, not the BASNID.
C
     &        I,TMPMATID(I)                                !cfan 2007/09
        CALL WRITEMSGF(57344+128+20,LINE(1:45))

        DO K=1,KK            !-------------k=1,kk

          II   =0
          SUM  =0.

          DO J=JBEGIN,JEND   !-------------j=jbegin,jend
            IF (TSTEMP6(K,J).GT.-998. .AND. TSTEMP6(K,J) .LT. 9998) THEN
              II=II+1
              SUM=SUM+TSTEMP6(K,J)
            ENDIF
          ENDDO              !-------------j==========

          IF (TSUNIT .EQ. 'DEGC') THEN
            IF (II .GT. 0) THEN
              TS(I,K)=SUM/II
            ELSE
              TS(I,K)=-999.0
            ENDIF
          ELSEIF  (TSUNIT .EQ. 'DEGF') THEN
            IF (II .GT. 0) THEN
             TS(I,K)=(SUM/II-32.)*5./9.
            ELSE
              TS(I,K)=-999.0
            ENDIF
          ENDIF

        ENDDO                !-------------k===========

CFAN    WRITE(LINE,'(35F4.0)') (TS(I,L),L=1,KK)
C
C       Change format in case of missing data -999. can't show up.
C
        WRITE(LINE,'(35F5.0)') (TS(I,L),L=1,KK)                !cfan 2007/09
        CALL WRITEMSGF(57344+128+50,LINE(1:175))

      ENDDO                  !-------------i===========


      RETURN
22    CALL WRITEMSGF(57344+384+0,' DATA FORMAT IN VALTIME IS WRONG!')
      STOP ' DATA FORMAT IN VALTIME IS WRONG!'

      End

C23456---1---------2---------3---------4---------5---------6---------712
      SUBROUTINE GETMAXMIN(ARR,NARR,TEMP,FAC1,FAC2,IFLAG,IERR)
C-----------------------------------------------------------------------
C
C    IFLAG   I    Int  1     1, for MAX
C                            0, for MIX
C    ARR     I/O  Real NARR
C    NARR    I    Int  1
C    TEMP    I    Real
C    FAC1,2  I    Real       0.75 or 0.5 or 0.25
C                            FAC1+FAC2=1.0
C    IERR    O    Int        1, for error
C                            0, for no error
C
C    When IFLAG=1
C    IF TEMP > MAX(ARR(NARR)), replay the MAX(ARR(NARR)) by TEMP
C    When IFLAG=0
C    IF TEMP < MIN(ARR(NARR)), replay the MIN(ARR(NARR)) by TEMP
C
      REAL*8 ARR(NARR),TEMP
      INTEGER ISWAP(1)
      INTRINSIC MAXLOC, MINLOC
C
C     MAXLOC, MINLOC is a FORTRAN 90 function that returns the index
C             value for the maximum and minimum element is the array
C
      IF (IFLAG.EQ.1) THEN
        ISWAP=MAXLOC(ARR(1:NARR))
        IF (ARR(ISWAP(1)) .LT. TEMP) 
     &      ARR(ISWAP(1))=FAC1*ARR(ISWAP(1))+FAC2*TEMP
        IERR=0
      ELSEIF (IFLAG.EQ.0) THEN
        ISWAP=MINLOC(ARR(1:NARR))
        IF (ARR(ISWAP(1)) .GT. TEMP) 
     &      ARR(ISWAP(1))=FAC1*ARR(ISWAP(1))+FAC2*TEMP
        IERR=0
      ELSE
        IERR=1
      ENDIF
      RETURN

      END

