C MODULE FCRDCF
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ A RECORD FROM THE CARRYOVER FILE.
C
      SUBROUTINE FCRDCF (IR,IW1,IW2,X,ISTAT)
C
C  ARGUMENT LIST:
C    IR - RECORD NUMBER ON KFCRY TO BE WRITTEN TO
C   IW1 - FIRST WORD POSITION IN RECORD IR TO BE REPLACED
C   IW2 - FINAL WORD POSITION IN RECORD IR TO BE REPLACED
C     X - ARRAY OF VALUES TO BE WRITTEN
C
      CHARACTER*8 OLDOPN
      DIMENSION X(*)
      DIMENSION Y(100)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/errdat'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/fcunit'
      INCLUDE 'common/fccgd'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fcrdcf.f,v $
     . $',                                                             '
     .$Id: fcrdcf.f,v 1.4 2002/02/11 19:21:08 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER FCRDCF'
C
      IOPNUM=0
      CALL FSTWHR ('FCRDCF  ',IOPNUM,OLDOPN,IOLDOP)
C
      IBUG=0
      IF (IFBUG('COTR').EQ.1) IBUG=1
      IF (IFBUG('COBG').EQ.1) IBUG=2
C
      IF (IBUG.GT.0) WRITE (IODBUG,*) 'IN FCRDCF -',
     *   ' IR=',IR,
     *   ' IW1=',IW1,
     *   ' IW2=',IW2,
     *   ' '
C
      ISTAT=0      
C
C  CHECK WORD POSITIONS
      IF (IW1.GT.0.AND.IW2.LE.NWR) GO TO 40
         NWR1=NWR+1
         WRITE (IPR,30) IW1,IW2,NWR1
30    FORMAT ('0**ERROR** ONE OR BOTH OF THE REQUESTED WORD POSITIONS ',
     *   'ARE INVALID. ',
     *   'IW1 (',I3,') SHOULD BE GREATER THAN ZERO. ',
     *   'IW2 (',I3,') SHOULD BE LESS THAN ',I3,'.')
         CALL ERROR
         ISTAT=1
         GO TO 70
C
C  READ RECORD
40    CALL UREADT (KFCRY,IR,Y,IERR)
      IF (IERR.GT.0) THEN
         WRITE (IPR,45) IR,KFCRY
45    FORMAT ('0**ERROR** IN FCRDCF - READING RECORD ',I6,
     *   ' FROM UNIT ',I2,'.')           
         CALL ERROR
         ISTAT=1
         GO TO 70
         ENDIF
C
C  SET VALUES IN ARRAY
      DO 60 I=IW1,IW2
         X(I-IW1+1)=Y(I)
60       CONTINUE
C
70    CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT FCRDCF'
C
      RETURN
C
      END
