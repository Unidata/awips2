C MODULE UINCLD
C-----------------------------------------------------------------------
C
C  ROUTINE TO CHECK FOR AND EXPAND 'INCLUDE' STATEMENTS.
C
      SUBROUTINE UINCLD (INCLKWD,RECOLD,ICKCOL,NSTLVL,RECNEW,LRECNEW,
     *   IPRERR,TYPMSG,NPUNIT,ISTAT)
C
C  THE 'INCLUDE' KEYWORD MUST BE THE FIRST FIELD IN THE RECORD BEING
C  CHECKED TO BE RECOGNIZED.
C
C  ARGUMENT LIST:
C
C     ARGUMENT   TYPE   I/O   DIM   CONTENTS
C     --------   ----   ---   ---   --------
C     INCLKWD    A(*)   I     1     CHARACTER STRING CONTAINING INCLUDE
C                                   KEYWORD
C     RECOLD     A(*)   I/O   1     RECOED TO BE CHECKED FOR INCLUDE
C                                   KEYWORD
C     ICKCOL     I*4    I     1     INDICATOR IF INCLUDE KEYWORD MUST
C                                   BEGIN IN SPECIFIED COLUMN OF RECORD:
C                                     0=CAN START IN ANY COLUMN
C                                    >0=MUST START IN SPECIFIED COLUMN
C     NSTLVL     I*4    I/O   1     NEST LEVEL INDICATOR:
C                                    INPUT: MUST BE SET TO 0 FIRST TIME
C                                           UINCLD CALLED
C                                    OUTPUT: INCLUDE NEST LEVEL
C                                     -1=END OF INCLUDE FOUND
C                                      0=INCLUDE KEYWORD NOT FOUND
C                                     >0=INCLUDE KEYWORD FOUND
C     RECNEW     A(*)   O     1     RECORD OBTAINED FROM PROCESSING AN
C                                   INCLUDE
C     LRECNEW    I*4    O     1     LENGTH OF RECORD
C     IPRERR     I*4    I     1     INDICATOR IF ERROR MESSAGES TO BE
C                                   PRINTED:
C                                    0=NO
C                                    1=YES
C     TYPMSG     A(*)   I     1     TYPE OF MESSAGE TO PRINT IF ERROR
C                                   ENCOUNTERED
C     NPUNIT     I*4    I     1     UNIT TO WHICH MESSAGES TO BE PRINTED
C     ISTAT      I*4    O     1     STATUS CODE:
C                                     0=NORMAL RETURN
C                                     1=MAXIMUM NEST LEVEL SPECIFIED
C                                       TO BE PROCESSED EXCEEDED
C                                     2=MAXIMUM NEST LEVEL ALLOWED
C                                       EXCEEDED
C                                     3=PATHNAME TOO LONG
C                                     4=FILE NOT FOUND
C                                     5=PATHNAME ALREADY BEING INCLUDED
C                                     6=ERROR OPENING FILE
C                                     7=ERROR READING FILE
C
      CHARACTER*(*) INCLKWD,RECOLD,RECNEW,TYPMSG
      CHARACTER*1 CHAR1
      CHARACTER*80 FILENAME
      CHARACTER*132 PATHNAME
      LOGICAL EXIST,OPENED
C
      INCLUDE 'ucmdbx'
      INCLUDE 'uoptnx'
      INCLUDE 'uinclx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/util/src/util_gen3/RCS/uincld.f,v $
     . $',                                                             '
     .$Id: uincld.f,v 1.6 2002/02/11 20:22:26 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'ENTER UINCLD - NSTLVL=',NSTLVL
         ENDIF
C
      ISTAT=0
C
      RECNEW=' '
      LRECNEW=0
      MPATHNAME=LEN(PATHNAME)
C
C  GET LENGTH IF INCLUDE KEYWORD
      CALL ULENTH (INCLKWD,LEN(INCLKWD),LINCLKWD)
C
      IF (RECOLD.EQ.' ') GO TO 30
C
C  GET LENGTH OF RECORD TO BE CHECKED FOR INCLUDE KEYWORD
      CALL ULENTH (RECOLD,LEN(RECOLD),LRECOLD)
C
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LINCLKWD=',LINCLKWD,
     *      ' INCLKWD=',INCLKWD
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LRECOLD=',LRECOLD,
     *      ' RECOLD=',RECOLD
         ENDIF
C
C  CHECK FOR COMMENT
      IF (ICINCL.EQ.1) THEN
         CALL UCKCMT (RECOLD,'$*',IRETRN)
         IF (IRETRN.NE.0) GO TO 30
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK FOR INCLUDE KEYWORD
C
      CALL UINCL2 (RECOLD,LRECOLD,FILENAME,LFILENAME,IERR)
      IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'UINCL2 CALLED : IERR=',IERR
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LFILENAME=',LFILENAME,
     *      ' FILENAME=',FILENAME
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'LINCLKWD=',LINCLKWD,
     *      ' INCLKWD=',INCLKWD
         ENDIF
      IF (IERR.GT.0) GO TO 30
      IF (FILENAME(1:LFILENAME).NE.INCLKWD(1:LINCLKWD)) GO TO 30
C
      IFINCL=0
      CALL UINDEX (RECOLD,LRECOLD,INCLKWD,LINCLKWD,ICOL)
      IF ((ICKCOL.GT.0.AND.ICOL.EQ.ICKCOL).OR.
     *    (ICKCOL.EQ.0.AND.ICOL.GT.0)) IFINCL=1
C
10    IF (IFINCL.EQ.1) THEN
         CALL ULINE (NPUNIT,3)
         WRITE (NPUNIT,110) INCLKWD(1:LINCLKWD),RECOLD(1:LRECOLD)
C     CHECK IF MAXIMUM INCLUDES ALLOWED EXCEEDED
         IF (NINCL+1.GT.MPINCL) THEN
            IF (TYPMSG.NE.'NONE') THEN
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL UEROR (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  CALL UWARN (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                  CHAR1='0'
                  ENDIF
               WRITE (NPUNIT,120) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *            'TO',MPINCL,'. INCLUDE WILL BE IGNORED.'
               ENDIF
            ISTAT=1
            GO TO 90
            ENDIF
C     GET FILENAME
         NSCAN=2
         IEND=LRECOLD
         IF (IEND.GT.72) IEND=72
         CALL USCAN2 (RECOLD(1:IEND),' ()',NSCAN,FILENAME,LFILENAME,
     *      IERR)
C     CHECK IF FILE EXISTS
         PATHNAME=' '
         IF (DIRINCL.EQ.' ') THEN
            IF (LFILENAME.GT.MPATHNAME) THEN
               IF (TYPMSG.NE.'NONE') THEN
                  IF (TYPMSG.EQ.'ERROR') THEN
                     CALL UEROR (NPUNIT,1,-1)
                     CHAR1='0'
                     IF (NOVPRT.GE.0) THEN
                        CHAR1='+'
                        ENDIF
                     ENDIF
                  IF (TYPMSG.EQ.'WARNING') THEN
                     CALL UWARN (NPUNIT,1,-1)
                     CHAR1='0'
                     IF (NOVPRT.GE.0) THEN
                        CHAR1='+'
                        ENDIF
                     ENDIF
                  IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                     CHAR1=' '
                     ENDIF
                  WRITE (NPUNIT,125) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *               LFILENAME,MPATHNAME
                  ENDIF
               ISTAT=3
               ELSE
                  PATHNAME=FILENAME
               ENDIF
            ELSE
               LDIRINCL=LENSTR(DIRINCL)
               LPATHNAME=LDIRINCL+LFILENAME
               IF (LPATHNAME.GT.MPATHNAME) THEN
                  IF (TYPMSG.NE.'NONE') THEN
                     IF (TYPMSG.EQ.'ERROR') THEN
                        CALL UEROR (NPUNIT,1,-1)
                        CHAR1='0'
                        IF (NOVPRT.GE.0) THEN
                           CHAR1='+'
                           ENDIF
                        ENDIF
                     IF (TYPMSG.EQ.'WARNING') THEN
                        CALL UWARN (NPUNIT,1,-1)
                        CHAR1='0'
                        IF (NOVPRT.GE.0) THEN
                           CHAR1='+'
                           ENDIF
                        ENDIF
                     IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                        CHAR1='0'
                        ENDIF
                     WRITE (NPUNIT,125) CHAR1,LPATHNAME,MPATHNAME
                     ENDIF
                  ISTAT=3
                  ELSE
                     PATHNAME=DIRINCL(1:LDIRINCL)//FILENAME
                  ENDIF
            ENDIF
         INQUIRE (FILE=PATHNAME,EXIST=EXIST)
         IF (.NOT.EXIST) THEN
            IF (TYPMSG.NE.'NONE') THEN
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL UEROR (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  CALL UWARN (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                  CHAR1='0'
                  ENDIF
               WRITE (NPUNIT,130) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *            PATHNAME(1:LENSTR(PATHNAME))
               ENDIF
            ISTAT=4
            RECOLD=' '
            GO TO 90
            ENDIF
C     CHECK IF MAXIMUM INCLUDES ALLOWED EXCEEDED
         IF (NINCL+1.GT.MINCL) THEN
            IF (TYPMSG.NE.'NONE') THEN
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL UEROR (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  CALL UWARN (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                  CHAR1='0'
                  ENDIF
               WRITE (NPUNIT,120) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *            'THAT CAN',MINCL,'.'
               ENDIF
            ISTAT=2
            GO TO 90
            ENDIF
         NINCL=NINCL+1
         PATHINCL(NINCL)=PATHNAME
         NRINCL(NINCL)=0
C     SET INCLUDE NEST LEVEL
         NSTLVL=NINCL
         CALL ULINE (NPUNIT,2)
         WRITE (NPUNIT,140)
     *      PATHINCL(NINCL)(1:LENSTR(PATHINCL(NINCL))),
     *      NSTLVL
         IF (ICMDBG.GT.0) THEN
            CALL ULINE (ICMPRU,1)
            WRITE (ICMPRU,*) 'NINCL=',NINCL,
     *         ' PATHINCL(NINCL)=',PATHINCL(NINCL),
     *         ' '
            ENDIF
C     CHECK IF INCLUDE IS SAME AS INCLUDE BEING PROCESSED
         IF (NINCL.GT.1) THEN
            DO 20 I=1,NINCL-1
               IF (PATHINCL(I).EQ.PATHINCL(NINCL)) THEN
                  IF (TYPMSG.NE.'NONE') THEN
                     IF (TYPMSG.EQ.'ERROR') THEN
                        CALL UEROR (NPUNIT,1,-1)
                        CHAR1='0'
                        IF (NOVPRT.GE.0) THEN
                           CHAR1='+'
                           ENDIF
                        ENDIF
                     IF (TYPMSG.EQ.'WARNING') THEN
                        CALL UWARN (NPUNIT,1,-1)
                        CHAR1='0'
                        IF (NOVPRT.GE.0) THEN
                           CHAR1='+'
                           ENDIF
                        ENDIF
                     IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                        CHAR1='0'
                        ENDIF
                     WRITE (NPUNIT,150) CHAR1,TYPMSG(1:LENSTR(TYPMSG))
                     ISTAT=5
                     ENDIF
                  NINCL=NINCL-1
                  RECOLD=' '
                  GO TO 40
                  ENDIF
20             CONTINUE
            ENDIF
         GO TO 40
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  CHECK IF ANY INCLUDE KEYWORDS FOUND
C
30    IF (NSTLVL.EQ.0) GO TO 90
C
40    INDEND=0
C
C  READ RECORD FROM FILE
      NIUNIT=99+NINCL
      PATHNAME=PATHINCL(NINCL)
      IF (NRINCL(NINCL).EQ.0) THEN
C     CHECK IF FILE ALREADY OPEN
         INQUIRE (UNIT=NIUNIT,IOSTAT=IOSTAT,OPENED=OPENED)
         IF (OPENED) THEN
            IF (TYPMSG.NE.'NONE') THEN
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL UEROR (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  CALL UWARN (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                  CHAR1='0'
                  ENDIF
               WRITE (NPUNIT,155) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *            PATHNAME(1:LENSTR(PATHNAME)),
     *            NIUNIT
               ENDIF
            ISTAT=6
            NINCL=NINCL-1
            IF (NINCL.EQ.0) THEN
               NSTLVL=-1
               GO TO 90
               ENDIF
            INDEND=1
            GO TO 80
            ENDIF
C     OPEN FILE            
         PATHNAME=PATHINCL(NINCL)
         OPEN (UNIT=NIUNIT,
     *      FILE=PATHNAME,
     *      STATUS='OLD',
     *      ACCESS='SEQUENTIAL',
     *      IOSTAT=IOSTAT)
         IF (IOSTAT.NE.0) THEN
            IF (TYPMSG.NE.'NONE') THEN
               IF (TYPMSG.EQ.'ERROR') THEN
                  CALL UEROR (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'WARNING') THEN
                  CALL UWARN (NPUNIT,1,-1)
                  CHAR1='0'
                  IF (NOVPRT.GE.0) THEN
                     CHAR1='+'
                     ENDIF
                  ENDIF
               IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
                  CHAR1='0'
                  ENDIF
               WRITE (NPUNIT,160) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *            PATHNAME(1:LENSTR(PATHNAME)),
     *            IOSTAT
               ENDIF
            ISTAT=6
            NINCL=NINCL-1
            IF (NINCL.EQ.0) THEN
               NSTLVL=-1
               GO TO 90
               ENDIF
            INDEND=1
            GO TO 80
            ENDIF
         ENDIF
C         
      READ (NIUNIT,'(A80)',IOSTAT=IOSTAT,ERR=50,END=60) RECNEW
      LRECNEW=LENSTR(RECNEW)
      NRINCL(NINCL)=NRINCL(NINCL)+1
      GO TO 70
C      
C  READ ERROR ENCOUNTERED
50    IF (TYPMSG.NE.'NONE') THEN
         IF (TYPMSG.EQ.'ERROR') THEN
            CALL UEROR (NPUNIT,1,-1)
            CHAR1='0'
            IF (NOVPRT.GE.0) THEN
               CHAR1='+'
               ENDIF
            ENDIF
         IF (TYPMSG.EQ.'WARNING') THEN
            CALL UWARN (NPUNIT,1,-1)
            CHAR1='0'
            IF (NOVPRT.GE.0) THEN
               CHAR1='+'
               ENDIF
            ENDIF
         IF (TYPMSG.EQ.'NOTE'.OR.TYPMSG.EQ.' ') THEN
            CHAR1='0'
            ENDIF
         WRITE (NPUNIT,170) CHAR1,TYPMSG(1:LENSTR(TYPMSG)),
     *      NRINCL(NINCL)+1,
     *      PATHNAME(1:LENSTR(PATHNAME))
         ENDIF
      ISTAT=7
      GO TO 65
C      
C  END-OF-FILE ENCOUNTERED
60    CALL ULINE (NPUNIT,2)
      WRITE (NPUNIT,180)
     *   NRINCL(NINCL),
     *   PATHNAME(1:LENSTR(PATHNAME))
C     
C  CLOSE FILE     
65    CLOSE (UNIT=NIUNIT)
      NINCL=NINCL-1
      IF (NINCL.EQ.0) THEN
         NSTLVL=-1
         GO TO 90
         ENDIF
      INDEND=1
      GO TO 80
C      
70    IF (ICMDBG.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'RECNEW=',RECNEW
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'NINCL=',NINCL,
     *      ' NRINCL(NINCL)',NRINCL(NINCL)
         ENDIF
C
80    IF (INDEND.EQ.1) GO TO 90
C
C  CHECK FOR COMMENT
      IF (ICINCL.EQ.1) THEN
         CALL UCKCMT (RECNEW,'$*',IRETRN)
         IF (IRETRN.NE.0) GO TO 90
         ENDIF
C
C  CHECK FOR INCLUDE KEYWORD
      IFINCL=0
      CALL UINCL2 (RECNEW,LEN(RECNEW),FILENAME,LFILENAME,IERR)
      IF (IERR.EQ.0.AND.FILENAME.EQ.INCLKWD) THEN
         CALL UINDEX (RECNEW,LEN(RECNEW),INCLKWD,LINCLKWD,ICOL)
         IF ((ICKCOL.GT.0.AND.ICOL.EQ.ICKCOL).OR.
     *       (ICKCOL.EQ.0.AND.ICOL.GT.0)) THEN
            RECOLD=RECNEW(1:LEN(RECOLD))
            CALL ULENTH (RECOLD,LEN(RECOLD),LRECOLD)
            IFINCL=1
            GO TO 10
            ENDIF
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    IF (ICMTRC.GT.0) THEN
         CALL ULINE (ICMPRU,1)
         WRITE (ICMPRU,*) 'EXIT  UINCLD - NSTLVL=',NSTLVL
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
110   FORMAT ('0*** NOTE - THE FOLLOWING ''',A,''' STATEMENT ',
     *   'WILL BE PROCESSED:' /
     *   T13,A)
120   FORMAT (A,'*** ',A,' - MAXIMUM NUMBER OF NESTED INCLUDES ',A,
     *   ' BE PROCESSED (',I3,') EXCEEDED',A)
125   FORMAT (A,'*** ',A,' - LENGTH OF PATHNAME (',I3,' ',
     *   'EXCEEDS MAXIMUM ALLOWED (',I3,').')
130   FORMAT (A,'*** ',A,' - FILE ',A,' DOES NOT EXIST.')
140   FORMAT ('0*** NOTE - RECORDS WILL BE INCLUDED FROM ',
     *   'FILENAME ',A,'. ',
     *   'NEST LEVEL IS ',I2,'.')
150   FORMAT (A,'*** ',A,' - INCLUDE SPECIFIED IS SAME AS ONE ',
     *   'CURRENTLY BEING PROCESSED AND WILL BE IGNORED.')
155   FORMAT (A,'*** ',A,' - OPENING FILE ',A,'. '
     *   'UNIT ',I3,' IS ALREADY OPENED.')
160   FORMAT (A,'*** ',A,' - OPENING FILE ',A,'. ',
     *   'IOSTAT=',I3)
170   FORMAT (A,'*** ',A,' - READ ERROR READING RECORD ',
     *   I6,' FROM FILE ',A,'.')
180   FORMAT ('0*** NOTE - ',I6,' RECORDS INCLUDED FROM FILE ',
     *   A,'.')
C
      END
