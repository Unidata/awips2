C$PRAGMA C (get_apps_defaults)
C$PRAGMA C (SETUPAPI)
C$PRAGMA C (genericprobe)

C23456---1---------2---------3---------4---------5---------6---------712
C-----------------------------------------------------------------------
C
      SUBROUTINE GETINFO(BASNID,NGRID,ALAT,ALON,NUMBASN,NUMGRID)
C
C-----------------------------------------------------------------------
C     FIRST PART get all grids lat,lon for every basin from nwsrfs DB
C    ===================================================================
C
C
      PARAMETER (LARRAY=50000)
      PARAMETER (MBPTS=50000)
      PARAMETER (MSEGS=10000)
      DIMENSION ARRAY(LARRAY),ARRAY1(LARRAY),ARRAY0(LARRAY)
      CHARACTER*8 BASNID(LARRAY),PID,TID,PXID,TEXT,STAID,RMATID,SBASNID
      CHARACTER*8 TMPMATID(LARRAY)                        !cfan 2007/09
      CHARACTER*20 DESCRP
      CHARACTER*11 logFile, inputFile
      REAL*8    FLAT(LARRAY),FLON(LARRAY),DATA(100,50)
      REAL*8    ALAT(LARRAY),ALON(LARRAY)
      DIMENSION IY(MSEGS),IXB(MSEGS),IXE(MSEGS)
      DIMENSION PXIDI(2)
      CHARACTER*4 MATID(2),CARD
      LOGICAL   TEST/.FALSE./
      INTEGER  LENtIME, F_INTERP, NUMPNTS
      CHARACTER*20  VALTIME(50)
      INTEGER  NGRID(LARRAY)
      CHARACTER*50  LINE
      COMMON/TMPMATID/TMPMATID                           !cfan 2007/09

C
      INCLUDE 'uio'
      INCLUDE 'common/x'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suerrx'
      INCLUDE 'scommon/sworkx'
      INCLUDE 'scommon/suoptx'
      INCLUDE 'scommon/sugnlx'
      INCLUDE 'scommon/suddsx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ndfd2rfs_sub/RCS/getinfo.f,v $
     . $',                                                             '
     .$Id: getinfo.f,v 1.3 2004/11/02 20:17:34 xfan Exp $
     . $' /
C    ===================================================================
C
C
C  READ DATABASE CONTROL RECORDS
C
      CALL RPPPCO(IERR)
      IF (IERR .EQ. 0) THEN
       CALL WRITEMSGF(49152+128+0,' PPP databse opened successfully!')
      ELSE
       STOP
      ENDIF
C
C  READ PARAMETER RECORD

      IPTR0=0
      IPTR=0
      IPRERR=0
      NUMBASN=0
      NUMGRID=0
C
190   CONTINUE

C  GET INPUT FIELD
      RMATID=' '

C      CALL WRITEMSGF(16384+128+80,' RPPREC')

      CALL RPPREC (RMATID,'MAT ',IPTR0,LARRAY,ARRAY,NFILL,IPTRNX0,IERR)
CFAN
C
C     DR18846: Incorrect MAT computations generated
C
C     In the old NDFD2RFS, when the preprocessor sees that the MATID and 
C     it's associated BASNID don't match, it just skips that basin.
C     But NERFC has more request:
C     When there is an upper/lower basin division, they generally define 
C     the "lower" MAT areas this way:
C        MAT area ID         BASIN ID
C        -----------         --------
C        NANM1LWR            NANM1ME
C        NWCN6LWR            NWCN6HUD
C     They would prefer that it processed those temperatures according to 
C     the BASNID given in the MAT area definition.
C     This wouldn't work for upper basins, as they define those areas with 
C     centroids, so there is no BASIN ID. They have another method for 
C     getting upper basin FMAT.
C     They really think this is a great tool for getting the WFO forecast 
C     temperatures into OFS.
C
CFAN  IF (ARRAY(2).NE.ARRAY(11) .AND. ARRAY(3).NE.ARRAY(12)) THEN
C
C     IF (ARRAY(2).NE.ARRAY(11) .OR. ARRAY(3).NE.ARRAY(12)) THEN     !cfan
      IF (ARRAY(11) .EQ. ARRAY(12)) THEN                             !cfan
        IF (IPTRNX0 .GT. IPTR0) THEN
          IPTR0=IPTRNX0
          WRITE(LINE,'(8H MATID =,A4,A4,13H  but BASNID=,A4,A4)')
     &          ARRAY(2),ARRAY(3),ARRAY(11),ARRAY(12)
          CALL WRITEMSGF(57344+256+80, LINE)
          GO TO 190
        ELSE
          GO TO 1000
        ENDIF
      ELSE
        NUMBASN = NUMBASN + 1
c       BASNID(NUMBASN) = RMATID                                    !cfan
        WRITE(BASNID(NUMBASN),'(2A4)')ARRAY(11),ARRAY(12)           !cfan
        WRITE(TMPMATID(NUMBASN),'(2A4)')ARRAY(2),ARRAY(3)           !cfan 2007/08
      ENDIF
      
      CALL RPPREC 
     & (BASNID(NUMBASN),'BASN',IPTR,LARRAY,ARRAY,NFILL,IPTRNX,IERR)

C      CALL WRITEMSGF(24576+128+80,' RPPREC')



      IF (IERR.GT.0) THEN
         ISTAT=IERR
         IF (IERR.EQ.2.AND.IPRERR.EQ.0) GO TO 110
            CALL SRPPST 
     &       (BASNID(NUMBASN),'BASN',IPTR,LARRAY,NFILL,IPTRNX,IERR)
            GO TO 110
         ENDIF
C
C  GET VERSION NUMBER
      NPOS=1
      IVER=ARRAY(NPOS)
C
C  GET BASIN IDENTIFIER
      DO 20 I=1,2
         NPOS=NPOS+1
         IPOS=(I-1)*4+1
         CALL SUBSTR (ARRAY(NPOS),1,4,BASNID(NUMBASN),IPOS)
20       CONTINUE
C
C  GET BASIN DESCRIPTION
      DO 30 I=1,5
         NPOS=NPOS+1
         IPOS=(I-1)*4+1
         CALL SUBSTR (ARRAY(NPOS),1,4,DESCRP,IPOS)
30       CONTINUE
c         write(*,'(A,x,A)')BASNID(NUMBASN),DESCRP

C
C  GET BASIN ELEVATION
      NPOS=NPOS+1
      ELEV=ARRAY(NPOS)
C
C  GET BASIN AREA SPECIFIED BY USER
      NPOS=NPOS+1
      AREA=ARRAY(NPOS)
C
C  GET BASIN AREA COMPUTED
      NPOS=NPOS+1
      CAREA=ARRAY(NPOS)
C
C  GET BASIN CENTROID
      NPOS=NPOS+1
      XC=ARRAY(NPOS)
      NPOS=NPOS+1
      YC=ARRAY(NPOS)
C 
C  GET IDENTIFIER OF MAP AREA THAT USES THIS BASIN
      DO 40 I=1,2
         NPOS=NPOS+1
         IPOS=(I-1)*4+1
         CALL SUBSTR (ARRAY(NPOS),1,4,PID,IPOS)
40       CONTINUE
C
C  STORE IDENTIFIER OF MAT AREA THAT USES THIS BASIN
      DO 50 I=1,2
         NPOS=NPOS+1
         IPOS=(I-1)*4+1
         CALL SUBSTR (ARRAY(NPOS),1,4,TID,IPOS)
50       CONTINUE
C
C  GET MAP AND MAT UPDATE INICATORS
      NPOS=NPOS+1
      MAPFLG=ARRAY(NPOS)
      NPOS=NPOS+1
      MATFLG=ARRAY(NPOS)
C
C  STORE IDENTIFIER OF MAPX AREA THAT USES THIS BASIN
      PXID=' '
      DO 60 I=1,2
         NPOS=NPOS+1
         IPOS=(I-1)*4+1
         CALL SUBSTR (ARRAY(NPOS),1,4,PXID,IPOS)
60       CONTINUE
      CALL SUBSTR (PXID,1,8,PXIDI,1)
      IF (PXIDI(1).EQ.-999.0.AND.PXIDI(2).EQ.-999.0.OR.
     *    PXIDI(2).EQ.0.0.AND.PXIDI(2).EQ.0.0) THEN
          PXID=' '
          ENDIF
C
C  GET GRID SPACING FACTOR
      NPOS=NPOS+1
      LFACTR=ARRAY(NPOS)
C
C  GET NUMBER OF PAIRS OF BASIN BOUNDARY POINTS
      NPOS=NPOS+1
      NBPTS=ARRAY(NPOS)
C
C  GET NUMBER OF GRID SEGMENTS
      NPOS=NPOS+1
      NSEGS=ARRAY(NPOS)
C
C  CHECK ARRAY SIZES
      IF (NBPTS.GT.MBPTS) THEN
         ISTAT=1
         WRITE (LP,140) 'LAT/LON PAIRS',NBPTS,MBPTS,BASNID
         CALL SUERRS (LP,2,-1)
         ENDIF
      IF (NSEGS.GT.MSEGS) THEN
         ISTAT=1
         WRITE (LP,140) 'GRID SEGMENTS',NSEGS,MSEGS,BASNID
         CALL SUERRS (LP,2,-1)
         ENDIF
      IF (ISTAT.GT.0) GO TO 110
C
C  GET BASIN LATITUDE POINTS
      DO 90 I=1,NBPTS
         NPOS=NPOS+1
         FLAT(I)=ARRAY(NPOS)
90       CONTINUE
C
C  GET BASIN LONGITUDE POINTS
      DO 100 I=1,NBPTS
         NPOS=NPOS+1
         FLON(I)=ARRAY(NPOS)*(-1.)
100      CONTINUE
C
C  GET GRID POINT DEFINITION
      NPOS=NPOS+1
      CALL SFCONV (IY,ARRAY(NPOS),NSEGS)
      NPOS=NPOS+NSEGS
      CALL SFCONV (IXB,ARRAY(NPOS),NSEGS)
      NPOS=NPOS+NSEGS
      CALL SFCONV (IXE,ARRAY(NPOS),NSEGS)
C
110   CONTINUE 
C
C  CONVERT FROM GRID COORIDINATES to LAT/LON
C

       NGRID(NUMBASN) = 0

       DO I = 1, NSEGS
        DO J= IXB(I),IXE(I)
         X=J
         Y=IY(I)
        CALL SBLLGD (FLONGRID,FLATGRID,1,X,Y,0,IERR)
        NGRID(NUMBASN) = NGRID (NUMBASN) + 1
        ALAT(NUMGRID+NGRID(NUMBASN))=FLATGRID
        ALON(NUMGRID+NGRID(NUMBASN))=FLONGRID*(-1)
        ENDDO
      ENDDO

       NUMGRID = NUMGRID + NGRID(NUMBASN)
c       WRITE(*,*)NUMBASN, BASNID(NUMBASN), NGRID(NUMBASN), NUMGRID
       WRITE(CARD,'(I4)')NGRID(NUMBASN)
       WRITE(SBASNID,'(I5)')NUMBASN
      CALL WRITEMSGF(57344+128+50,
     &               SBASNID//'  '//BASNID(NUMBASN)//'   '
     &             //CARD//' HRAP Grids.' )

9999   continue
       IF (IPTRNX0 .GT. IPTR0) THEN
       IPTR0=IPTRNX0
       ELSE
       GO TO 1000
       ENDIF

      IF (TEST) THEN
        GO TO 1000
      ELSE
        GO TO 190
      ENDIF

1000  continue
      close (12)
C
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
140   FORMAT ('0*** ERROR - IN SRBASN - ',
     *   'NUMBER OF ',A,' TO BE PROCESSED (',I4,
     *   ') EXCEEDS MAXIMUM (',I4,') FOR BASIN ',A,'.')
C
      END
