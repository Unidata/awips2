C MODULE FDATCK
C-----------------------------------------------------------------------
C
C  ROUTINE FDATCK COMPARES TWO DATES.
C
      SUBROUTINE FDATCK (IDA,IHR,NDA,NHR,IOP,ISW)
C
C  ARGUMENT LIST
C     IDA - JULIAN DATE OF FIRST DATE
C     IHR - INTERNAL CLOCK TIME OF FIRST DATE
C     NDA - JULIAN DATE OF SECOND DATE
C     NHR - INTERNAL CLOCK TIME OF SECOND DATE
C     IOP - RELATIONAL OPERATOR - LT, EQ, OR GT
C     ISW - OUTCOME OF COMPARISON, 1=TRUE
C                                  0=FALSE
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/where'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/sysbug'
      INCLUDE 'common/errdat'
C
      CHARACTER*4 IOP
      DIMENSION OPNOLD(2)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_fcex/RCS/fdatck.f,v $
     . $',                                                             '
     .$Id: fdatck.f,v 1.3 1999/01/19 20:13:19 page Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=0
      CALL UMEMOV (OPNAME,OPNOLD,2)
      CALL UMEMOV ('FDATCK  ',OPNAME,2)
C
      IBUG=0
      IF (IFBUG('COTR').EQ.1) IBUG=1
      IF (IFBUG('COBG').EQ.1) IBUG=2
C
      IF (IBUG.GE.1) WRITE(IODBUG,10)
10    FORMAT (' *** ENTER FDATCK')
C
      LDA=NDA
      LHR=NHR
      JDA=IDA
      JHR=IHR
C
      IF (LHR.NE.0) GO TO 20
         LHR=24
         LDA=LDA-1
C
20    IF (JHR.NE.0) GO TO 30
         JHR=24
         JDA=JDA-1
C
30    ISW=0
C
      IF (IBUG.GE.2) WRITE (IODBUG,40) IDA,IHR,IOP,NDA,NHR
40    FORMAT (' CHECK TO SEE IF ',2(1X,I5),' IS ',A2,2(1X,I5))
C
      IF (IOP.EQ.'EQ') GO TO 60
      IF (IOP.EQ.'LT') GO TO 70
      IF (IOP.EQ.'GT') GO TO 100
C
      WRITE (IPR,50) IOP
50    FORMAT ('0**ERROR** ''',A,
     *   ''' IS AN INVALID DATE CHECK OPERATION. ',
     *   'OPERATION MUST BE LT, EQ OR GT.')
      CALL ERROR
      GO TO 130
C
C  CHECK FOR THE EQUAL CONDITION
C
60    IF (JDA.NE.LDA.OR.JHR.NE.LHR) GO TO 130
      ISW=1
      GO TO 130
C
C   CHECK FOR THE LESS THAN CONDITION
C
70    LDA1=LDA-1
      IF (JDA.LE.LDA1) GO TO 80
      IF (JDA.EQ.LDA) GO TO 90
      GO TO 130
C
80    ISW=1
      GO TO 130
C
90    IF (JHR.LT.LHR) ISW=1
      GO TO 130
C
C   CHECK FOR THE GREATER THAN CONDITION
C
100   LDA1=LDA+1
      IF (JDA.GE.LDA1) GO TO 110
      IF (JDA.EQ.LDA) GO TO 120
      GO TO 130
C
110   ISW=1
      GO TO 130
C
120   IF (JHR.GT.LHR) ISW=1
      GO TO 130
C
130   IF (IBUG.GE.1) THEN
         WRITE (IODBUG,*) 'ISW=',ISW
         ENDIF
C
      IF (IBUG.GE.1) WRITE (IODBUG,180)
180   FORMAT (' *** EXIT FDATCK')
C
      CALL UMEMOV (OPNOLD,OPNAME,2)
C
      RETURN
C
      END
