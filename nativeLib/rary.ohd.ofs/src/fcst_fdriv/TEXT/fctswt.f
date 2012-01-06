C MODULE FCTSWT
C-----------------------------------------------------------------------
C
      SUBROUTINE FCTSWT (TS,MTS,D,MD,IHZERO,NWORK,IERR)
C
C  THIS ROUTINE WRITES TIME SERIES DATA FROM THE D ARRAY TO
C  EXTERNAL DATA FILES AFTER THE EXECUTION OF THE OPERATIONS TABLE
C
      CHARACTER*4 FILEID
      CHARACTER*8 RTNNAM/'FCTSWT'/
C
      DIMENSION TS(MTS),D(MD)
      PARAMETER (LEXTLOC=50)
      DIMENSION EXTLOC(LEXTLOC)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/fprog'
      INCLUDE 'common/fctime'
      INCLUDE 'clbcommon/crwctl'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fdriv/RCS/fctswt.f,v $
     . $',                                                             '
     .$Id: fctswt.f,v 1.4 1999/07/06 14:38:23 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GE.1) WRITE (IODBUG,10)
C
      IERROR=0
      IERR=0
C
      IOPNUM=0
      CALL UMEMOV (RTNNAM,OPNAME,2)
      LWKBUF=MD-(NWORK-1)
C
      IF (IFBUG('PRTS').EQ.1) GO TO 20
      IF (IFBUG('TSRW').EQ.1) GO TO 20
      GO TO 30
20    CALL FDMPA ('TS  ',TS,MTS)
C
C  SEARCH TS ARRAY FOR OUTPUT AND UPDATE TIME SERIES
30    LOC=1
40    NCTS=TS(LOC)
      IF (NCTS.EQ.0) GO TO 170
      NXLOC=TS(LOC+1)
      IF ((NCTS.EQ.1).OR.(NCTS.EQ.4)) GO TO 160
C
C  FOUND AN OUTPUT OR UPDATE TIME SERIES
C  CHECK IF OUTPUT DURING THE OPERATIONS TABLE
      IOT=TS(LOC+10)
      IF (IOT.EQ.1) GO TO 160
C
C  OUTPUT TIME SERIES IN THIS ROUTINE
C  GET GENERAL INFORMATION FOR THE DATA TYPE
      DTYPE=TS(LOC+4)
      CALL FDCODE (DTYPE,UNITS,DIM,MSG,NPDT,TSCALE,NADD,IER)
C
C  GET EXTERNAL LOCATION INFORMATION
      NUMEXT=TS(LOC+11)
      DO 50 I=1,NUMEXT
         J=LOC+11+I
         EXTLOC(I)=TS(J)
50       CONTINUE
C
C  FIND WHERE DATA ARE IN THE D ARRAY AND THE TIME INTERVAL
      LD=TS(LOC+7)
      IDT=TS(LOC+5)
C
C  GET FILE TYPE
      CALL UMEMOV (TS(LOC+9),FILEID,1)
C
      IF (FILEID.EQ.'FPDB') THEN
C     NWSRFS OFS PROCESSED DATA BASE
         IER=0
         CALL FWTPDB (EXTLOC,IDT,UNITS,NPDT,D(LD),LWKBUF,D(NWORK),IER)
         CALL UMEMOV (RTNNAM,OPNAME,2)
         IF (IER.EQ.0) GO TO 90
         IERR=1
         GO TO 160
         ENDIF
C
      IF (FILEID.EQ.'CALB') THEN
C     NWSRFS CALIBRATION DISK FILES
         IER=0
         CALL FWTCAL (EXTLOC,DTYPE,IDT,NPDT,UNITS,DIM,LD,NWORK,D,MD,
     *      MO,IYEAR,IER)
         CALL UMEMOV (RTNNAM,OPNAME,2)
         IF (IER.EQ.0) GO TO 90
         IERR=1
         GO TO 160
         ENDIF
C
      IF (FILEID.EQ.'CARD') THEN
C     NWSRFS DATACARD FILES
         IER=0
         CALL FWTCAR (EXTLOC,DTYPE,LD,D,MD,MO,IYEAR,IER)
         CALL UMEMOV (RTNNAM,OPNAME,2)
         IF (IER.EQ.0) GO TO 90
         IERR=1
         GO TO 160
         ENDIF
C         
      IF (FILEID.EQ.'HMDB') THEN
C        RTi HMDB FILES
         IER=0
         CALL FWTHMD (EXTLOC,DTYPE,IDT,NPDT,UNITS,LD,NWORK,D,MD,
     +      MO,IYEAR,IER)
         CALL UMEMOV (RTNNAM,OPNAME,2)
         IF (IER.EQ.0) GO TO 90
         IERR=1
         GO TO 160
         ENDIF
C
      WRITE (IPR,80) FILEID
      CALL STOP
      IERR=1
      GO TO 160
C
90    IF (IFBUG('TSRW').EQ.1) THEN
         WRITE (IODBUG,110) TS(LOC+2),TS(LOC+3),DTYPE,IDT,LOC
         IF (MAINUM.GT.2) WRITE (IODBUG,*)
     *      ' MO=',MO,
     *      ' IYEAR=',IYEAR,
     *      ' '
         J=LD-1
         IHR=IHZERO+IDT
         IF (IHR.GT.24) IHR=24
         L1=J+((IDA-IDADAT)*24/IDT+(IHR-1)/IDT)*NPDT+1
         L2=J+((LDA-IDADAT)*24/IDT+(LHR/IDT))*NPDT
         IF (L1.GT.L2) GO TO 140
            WRITE (IODBUG,130)(D(I),I=L1,L2)
            GO TO 160
140      WRITE (IODBUG,150)
         ENDIF
C
C  INCREMENT TO THE NEXT TIME SERIES
160   LOC=NXLOC
C
C  CHECK IF END OF TS ARRAY
      IF (LOC.LE.MTS) GO TO 40
C
170   RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
10    FORMAT (' *** ENTER FCTSWT')
80    FORMAT ('0**ERROR** IN FCTSWT - ',A,' IS AN INVALID ',
     *   'FILE TYPE CODE.')
110   FORMAT (' ROUTINE FCDWT DEBUG - OUTPUT FROM TS ARRAY FOR ',
     *   2A4,1X,A4,I3,1X,'HOURS',3X,'TS LOC=',I4)
130   FORMAT (' ',12F10.3)
150   FORMAT (' NONE OUTPUT')
C
      END
