C MODULE CDINPT
C-----------------------------------------------------------------------
C
C  ROUTINE TO READ ALL INPUT CARDS FOR AN FCINIT COMMAND.
C
      SUBROUTINE CDINPT (CARDS,MCARDS,NCARDS,CMDNAM,IER)
C
C  ARGUMENT LIST:
C
C   INPUT:
C     CMDNAM  - COMMAND NAME
C     MCARDS  - MAXIMUM NUMBER OF CARD IMAGES THAT CAN BE STORED
C
C   OUTPUT:
C      CARDS  - ARRAY HOLDING THE INPUT CARDS IN 20A4 FORMAT
C      NCARDS - NUMBER OF CARDS IN INPUT STREAM (EXCLUDING 'END' CARD)
C      IER    - ERROR CODE:
C                 0=NO ERROR
C                 1=MAXIMUM CARDS EXCEEDED
C
C......................................................................
C
C  ORIGINALLY WRITTEN BY -- JOE OSTROWSKI -- HRL -- 810309
C
C.................................................................
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
C
      CHARACTER*1 DLIM/' '/
      CHARACTER*8 CMDNAM,OPNOLD
      CHARACTER*80 CARDS(MCARDS),IBUF,STRNG
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_util/RCS/cdinpt.f,v $
     . $',                                                             '
     .$Id: cdinpt.f,v 1.3 2000/03/14 11:53:42 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'ENTER CDINPT'
C
      IBUG=IFBUG('CDRD')
C
      IOPNUM=0
      CALL FSTWHR ('CDINPT  ',IOPNUM,OPNOLD,IOLDOP)
C
      ICARDS=0
      IER=0
C
C  READ CARDS UNTIL 'END' CARD OR END-OF FILE IS ENCOUNTERED
30    ICARDS=ICARDS+1
      IF (ICARDS.GT.MCARDS) THEN
         WRITE (IPR,140) MCARDS
140   FORMAT ('0**ERROR** MAXIMUM NUMBER OF INPUT CARDS ',
     *   'ALLOWED (',I4,') EXCEEDED.')
         CALL ERROR
         IER=1
	 GO TO 150
	 ENDIF
      READ (IN,40,END=70) CARDS(ICARDS)
40    FORMAT (A)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'CARDS(ICARDS)=',CARDS(ICARDS)
      CALL UMEMOV (CARDS(ICARDS),IBUF,20)
      NSCAN=1
      CALL USCAN2 (IBUF,DLIM,NSCAN,STRNG,LSTRNG,IERR)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'STRNG=',STRNG
      IF (STRNG.NE.'END') GO TO 30
C
70    NCARDS=ICARDS-1
C
C  CHECK IF ANY CARDS FOUND BEFORE 'END' CARD
      IF (NCARDS.EQ.0) THEN
         WRITE (IPR,80) CMDNAM(1:LENSTR(CMDNAM))
80    FORMAT ('0**WARNING** NO INPUT CARDS FOUND FOR COMMAND ',A,'.')
         CALL WARN
         GO TO 160
	 ENDIF
C
C  PRINT CARDS
      WRITE (IPR,100) CMDNAM(1:LENSTR(CMDNAM))
100   FORMAT ('0INPUT CARDS FOR COMMAND ',A,':')
      DO 120 IJ=1,NCARDS
         WRITE (IPR,110) CARDS(IJ)
110   FORMAT (' ',A)
120      CONTINUE
      GO TO 160
C
C  READ UNTIL 'END' CARD FOUND
150   READ (IN,40,END=160) CARDS(1)
      WRITE (IPR,110) CARDS(1)
      CALL UMEMOV (CARDS(1),IBUF,20)
      NSCAN=1
      CALL USCAN2 (IBUF,DLIM,NSCAN,STRNG,LSTRNG,IERR)
      IF (IBUG.GE.1) WRITE (IODBUG,*) 'STRNG=',STRNG
      IF (STRNG.NE.'END') GO TO 150
C
160   CALL FSTWHR (OPNOLD,IOLDOP,OPNOLD,IOLDOP)
C
      IF (ITRACE.GT.0) WRITE (IODBUG,*) 'EXIT CDINPT'
C
      RETURN
C
      END
