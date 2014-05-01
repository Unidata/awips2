C MEMBER PTDATF
C  (from old member PRDPRINT)
C-----------------------------------------------------------------------
C
C                             LAST UPDATE: 05/12/95.10:54:24 BY $WC20SV
C
C @PROCESS LVL(77)
C
      SUBROUTINE PTDATF
C
C  ROUTINE TO PRINT THE PROCESSED DATA BASE STATUS INFORMATION
C
      CHARACTER*4 XDIMNS(13)
     *   /'L   ','L2  ','L3  ','L/T ','L3/T',
     *    'E/L2','PRES','TEMP','DLES','TIME',
     *    'DIR ','E   ','E/T '/
      CHARACTER*4 XCLRCD(4)
     *   /'PP  ','FC  ','ANY ','N/A '/
      CHARACTER*4 XPRSCD(3)
     *   /'INST','ACCM','MEAN'/
      CHARACTER*8 TSTYPE
C
      INCLUDE 'uio'
      INCLUDE 'udebug'
      INCLUDE 'prdcommon/punits'
      INCLUDE 'prdcommon/pdftbl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'prdcommon/ptsctl'
C
      EQUIVALENCE (KPRDTU(1),KMAPTS)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/prdutil/RCS/ptdatf.f,v $
     . $',                                                             '
     .$Id: ptdatf.f,v 1.2 1997/09/22 17:59:03 page Exp $
     . $' /
C    ===================================================================
C
C
C
C  PRINT TIME SERIES FILE STATUS
      CALL ULINE (LP,2)
      WRITE (LP,70)
      CALL ULINE (LP,4)
      WRITE (LP,80)
      DO 10 I=1,5
         IIFILE=TSCNTR(1,I)-KPRDTU(1)+1
         WRITE (LP,90) TSCNTR(1,I),IIFILE,(TSCNTR(J,I),J=2,4)
         CALL ULINE (LP,1)
10       CONTINUE
C
C  PRINT USER DIMENSION
      CALL ULINE (LP,2)
      WRITE (LP,110) MAXTMS,NUMTMS
      CALL ULINE (LP,2)
      WRITE (LP,120) MINDAY
C
C  PRINT DATA TYPE INFORMATION
      CALL ULINE (LP,2)
      WRITE (LP,130)
      CALL ULINE (LP,2)
      WRITE (LP,100) MAXDTP,NUMDTP
C
      IF (NUMDTP.EQ.0) THEN
         WRITE (LP,105)
         CALL ULINE (LP,2)
         GO TO 60
         ENDIF
C
      CALL ULINE (LP,5)
      WRITE (LP,140)
      DO 50 I=1,NUMDTP
         IDIMNS=DATFIL(12,I)
         IPRSCD=DATFIL(6,I)
         ICLNGC=DATFIL(11,I)+1
         TSTYPE='REGULAR'
         ITYPE=DATFIL(1,I)
         IF (DATFIL(7,I)) 30,20,40
20          TSTYPE='MIXED'
            GO TO 40
30       II=-DATFIL(7,I)
         ITYPE=DATFIL(1,II)
         TSTYPE='FUTURE'
40       IIFILE=DATFIL(2,I)-KPRDTU(1)+1
         CALL ULINE (LP,1)
         WRITE (LP,150) ITYPE,TSTYPE,DATFIL(2,I),IIFILE,
     *       (DATFIL(J,I),J=3,5),XPRSCD(IPRSCD),DATFIL(7,I),
     *       XCLRCD(ICLNGC),XDIMNS(IDIMNS),DATFIL(15,I)
         IF (IPRDB.GT.0)
     *      WRITE (IOGDB,160) I,
     *         DATFIL(1,I),
     *         (DATFIL(J,I),J=8,10),
     *         (DATFIL(J,I),J=13,14)
50       CONTINUE
C
60    RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
70    FORMAT ('0- PROCESSED DATA BASE FILE STATUS -')
80    FORMAT ('0',9X,'FILE',5X,'MAXIMUM   NEXT AVAILABLE   NUMBER OF' /
     *   1X,'UNIT',5X,'NAME',5X,'RECORDS',7X,'RECORD',7X,'DATA TYPES' /
     *   1X,4('-'),3X,8('-'),3X,7('-'),3X,14('-'),3X,10('-'))
90    FORMAT (1X,I3,4X,'PRDTS',I1,4X,I7,2I14)     
110   FORMAT ('0MAXIMUM TIME SERIES = ',I5,3X,
     *   'TIME SERIES DEFINED = ',I5)
120   FORMAT ('0MINIMUM DAYS OF REGULAR DATA TO BE RETAINED ',
     *   'WHEN UPDATING MIXED TIME SERIES = ',I3)
100   FORMAT ('0MAXIMUM DATA TYPES = ',I3,3X,
     *   'DATA TYPES DEFINED = ',I3)
105   FORMAT ('0*** NOTE - NO DATA TYPES ARE DEFINED.')
130   FORMAT ('0- PROCESSED DATA BASE DATA TYPE STATUS -')
140   FORMAT ('0',18X,'LOGICAL',14X,'NUMBER',11X,'SMALLEST   TIME',
     *      ' INTERVAL   FUTURE-   CALLING',16X,'NUMBER OF' /
     *   ' DATA',15X,'UNIT',7X,'FILE',5X,'TO KEEP   MAX',
     *      6X,'TIME',6X,'PROCESSING',5X,'REGULAR   ROUTINE',
     *      5X,'UNITS',5X,'TIME SERIES' / ' TYPE   CONTENTS   NUMBER',
     *      6X,'NAME',5X,'IN CORE   DAYS   INTERVAL',7X,'CODE',8X,
     *      'POINTER    CODE',5X,'DIMENSION',5X,'DEFINED' /
     *   1X,4('-'),3X,2(8('-'),3X,7('-'),3X),4('-'),3X,8('-'),
     *      3X,13('-'),3X,2(7('-'),3X),9('-'),3X,11('-'))
150   FORMAT (1X,A4,3X,A,3X,I4,6X,'PRDTS',I1,7X,
     *   I2,7X,I3,6X,I2,7X,3X,A4,10X,I3,8X,
     *   A4,6X,A4,6X,I7)
160   FORMAT (' I=',I2,3X,
     *   'DATFIL(1,I)=',A4,3X,
     *   'DATFIL(8...10,I)=',3(I6,1X),3X,
     *   'DATFIL(13...14,I)=',2(I6,1X))
C
      END
