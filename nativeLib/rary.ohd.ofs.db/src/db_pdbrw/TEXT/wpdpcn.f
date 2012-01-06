C MEMBER WPDPCN
C  (from old member PDWPDPCN)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 03/01/94.14:02:17 BY $WC20SV
C
C @PROCESS LVL(77)
C
       SUBROUTINE WPDPCN (IPP24,IND,ISTAT)
C
C          ROUTINE:  WPDPCN
C
C             VERSION:  1.0.0
C
C                DATE:  5-22-84
C
C              AUTHOR:  SONJA R SIEGEL
C                       DATA SCIENCES INC
C
C***********************************************************************
C
C          DESCRIPTION:
C
C    THIS ROUTINE SETS AN INDICATOR FOR A PRECIPITATION STATION TO
C    SPECIFY WHETHER MORE THAN ONE STATION IS AT THE SAME LOCATION.
C
C***********************************************************************
C
C          ARGUMENT LIST:
C
C         NAME    TYPE  I/O   DIM   DESCRIPTION
C
C       IPP24      I    I      1    ARRAY LOCATION FOR THE BEGINNING
C                                   OF THE POINTER VALUES FOR THE STA
C       IND       I     I      1    INDICATOR 0=NO OTHER STATIONS
C                                             1=MORE THAN 1 STATION
C       ISTAT     I     O      1    STATUS=1 IPP24 OUT OF RANGE
C                                          2 IPP24 NOT BEGIN OF POINTRS
C                                          3 IPP24 SPEC UNDEF STA
C                                          4 INVALID IND
C                                          5 SYSTEM ERROR
C
C
C***********************************************************************
C
C          COMMON:
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'pdbcommon/pdunts'
      INCLUDE 'pdbcommon/pdsifc'
C
C***********************************************************************
C
C          DIMENSION AND TYPE DECLARATIONS:
C
      INTEGER*2 IPNTRS(32)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/db_pdbrw/RCS/wpdpcn.f,v $
     . $',                                                             '
     .$Id: wpdpcn.f,v 1.1 1995/09/17 18:44:59 dws Exp $
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
      IF (IPDTR.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,80) IPP24,IND
C
      ISTAT=0
C
C  FIND PP24 IN INDEX
      IDX=IPDCKD('PP24')
      IF (IDX.EQ.0) GO TO 50
C
C  CHECK RANGE OF INPUT IPP24
      IF (IPP24.LT.1.OR.IPP24.GT.IDDTDR(18,IDX)) GO TO 20
C
C  CHECK THAT IPP24 IS BEGINNING OF PARM SLOTS
      NP=IDDTDR(5,IDX)
      IF (MOD(IPP24,NP).NE.1) GO TO 30
C
C  CHECK INDICATOR FOR 0 OR 1
      IF (IND.LT.0.OR.IND.GT.1) GO TO 40
C
C  GET POINTERS
      LRC=LRCPDD*2
      N=(IPP24-1)/LRC
      IPOS=IPP24-(N*LRC)
      IREC=IDDTDR(14,IDX)+N
      IFILE=IDDTDR(4,IDX)
      CALL UREADT (KPDDDF(IFILE),IREC,IPNTRS,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
C
C  CHECK STATION SLOT
      IF (IPDDB.GT.0) WRITE (IOGDB,90) IPNTRS(IPOS),IND
      IF (IPNTRS(IPOS).EQ.0) GO TO 60
C
C  SET PNTR ACCORDING TO IND
      IF (IND.EQ.0.AND.IPNTRS(IPOS).LT.0) GO TO 10
      IF (IND.EQ.1.AND.IPNTRS(IPOS).GT.0) GO TO 10
      GO TO 70
C
C  RESET SLOT
10    IPNTRS(IPOS)=-IPNTRS(IPOS)
      CALL UWRITT (KPDDDF(IFILE),IREC,IPNTRS,ISTAT)
      IF (ISTAT.NE.0) GO TO 50
      GO TO 70
C
C  ERROR CONDITIONS
20    ISTAT=1
      GO TO 70
30    ISTAT=2
      GO TO 70
40    ISTAT=4
      GO TO 70
50    ISTAT=5
      GO TO 70
60    ISTAT=3
C
70    IF (IPDTR.GT.0.OR.IPDDB.GT.0) WRITE (IOGDB,100) IPP24,IPOS,IREC,
     *  ISTAT
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
80    FORMAT (' *** ENTER WPDPCN : IPP24,IND=',2I8)
90    FORMAT (' IPNTRS(IPOS)=',I8,' IND=',I2)
100   FORMAT (' *** EXIT WPDPCN : IPP24,IPOS,IREC,ISTAT: ',4I8)
C
      END
