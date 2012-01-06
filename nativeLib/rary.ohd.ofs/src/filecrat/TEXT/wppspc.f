C MEMBER WPPSPC
C  (from old member PPWPPSPC)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 11/15/93.14:38:28 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE WPPSPC (ITYPE,NSTAS,NVPS,ISTAT)
C
C          SUBROUTINE:  WPPSPC
C
C             VERSION: 1.0.2  SRS CORRECTION TO CALCULATION FOR # OF
C                             PARAMETER RECORDS  1-9-85
C
C             VERSION:  1.0.0
C
C                DATE:  12-8-82
C
C              AUTHOR:  JANINE FRANZOI
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE WRITE THE SPECIAL PARAMETER TYPE CONTROL RECORD
C    AND UPDATES THE FILE CONTROL INFORMATION.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C         ITYPE    A    I      1     SPECIAL PARAMETER
C         NSTA     I    I      1     MAX NUMBER OF STATIONS
C         NVPS     I    I      1     NUM VALUES PER STATION
C         ISTAT    I    O      1     STATUS INDICATOR
C                                      0=NORMAL RETURN
C                                      1=ERROR
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'udebug'
      INCLUDE 'pppcommon/pppdta'
      INCLUDE 'pppcommon/ppunts'
      INCLUDE 'pppcommon/ppdtdr'
      INCLUDE 'pppcommon/ppmctl'
      INCLUDE 'pppcommon/ppxctl'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      DIMENSION IARR(16)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filecrat/RCS/wppspc.f,v $
     . $',                                                             '
     .$Id: wppspc.f,v 1.1 1995/09/17 19:08:56 dws Exp $
     . $' /
C    ===================================================================
C
C
C***********************************************************************
C
C          DATA:
C
C
C***********************************************************************
C
C
      IF (IPPTR.GT.0) WRITE (IOGDB,20)
C
      ISTAT=0
C
      CALL UMEMST (0,IARR,LRECPP)
C
C  GET FILE UNIT FOR SPECIAL PARAMETER TYPE
      INDX=IPCKDT(ITYPE)
      IFILE=IPDTDR(2,INDX)
      IF (IFILE.GT.NMPFIL) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,30) IFILE,NMPFIL,ITYPE
         ISTAT=1
         GO TO 10
         ENDIF
C
C  CALCULATE NUMBER OF RECORDS IN EACH ENTRY (VALUES STORED AS I*2)
      LRECP2=LRECPP*2
      NERECS=IUNRCD(NSTAS,LRECP2)
C
C  SET UP CONTROL RECORD
      IARR(1)=9
      IARR(2)=IPMCTL(2,IFILE)+2
      IARR(3)=NERECS
      IARR(4)=12
      IARR(5)=ITYPE
      IARR(6)=NVPS
      IARR(8)=NSTAS
C
      IUNIT=KPPRMU(IFILE)
C
C  CHECK IF ENOUGH UNUSED RECORDS
      NTRECS=NERECS*12*NVPS+1
      IF (IPMCTL(2,IFILE)+NTRECS.GT.IPMCTL(1,IFILE)) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,40) NTRECS,ITYPE,IUNIT,
     *      IPMCTL(2,IFILE),IPMCTL(1,IFILE)
         ISTAT=1
         GO TO 10
         ENDIF
C
C  WRITE CONTROL RECORD TO FILE
      IREC=IPMCTL(2,IFILE)+1
      CALL UWRITT (IUNIT,IREC,IARR,IERR)
      IF (IERR.GT.0) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,50)
         ISTAT=1
         GO TO 10
         ENDIF
C
C  UPDATE PARAMETER CONTROL RECORD AND DATA DIRECTORY FILE RECORD
      IPMCTL(2,IFILE)=IPMCTL(2,IFILE)+NTRECS
      IPMCTL(3,IFILE)=IPMCTL(3,IFILE)+1
      IPDTDR(3,INDX)=IREC
      IPDTDR(4,INDX)=IREC
      IPDTDR(5,INDX)=1
C
10    IF (IPPTR.GT.0) WRITE (IOGDB,60)
C
      RETURN
C
C-----------------------------------------------------------------------
C
20    FORMAT (' *** ENTER WPPSPC')
30    FORMAT ('+*** ERROR - ORDINAL FILE NUMBER ',I3,
     *   ' EXCEEDS MAXIMUM ALLOWED (',I3,
     *   ') FOR PARAMETER TYPE ',A4,'.')
40    FORMAT ('+*** ERROR - NUMBER OF RECORDS (',I4,') NEEDED TO ',
     *   'WRITE TYPE ',A4,' TO UNIT ',I3,'. ',I4,' OF THE ',I4,
     *   ' RECORDS ARE USED.')
50    FORMAT ('+*** ERROR - IN WPPSCP - DAIO WRITE ERROR.')
60    FORMAT (' *** EXIT WPPSPC')
      END
