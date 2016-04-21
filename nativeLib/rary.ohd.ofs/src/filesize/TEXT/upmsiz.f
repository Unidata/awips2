C MODULE UPMSIZ
C-----------------------------------------------------------------------
C
      SUBROUTINE UPMSIZ (XCMD,MDSTRK,NDSTRK,LPUNCH,ISTAT)
C
C  ROUTINE TO READ OPTIONAL INPUT CARD IMAGES FOR COMMAND 'UPRM' SO THAT
C  THEY CAN BE PUNCHED OUT AND USED AS COMMAND INPUT TO PROGRAM FILECRAT
C
      CHARACTER*(*) XCMD
      CHARACTER*8 XWORD
      PARAMETER (MAXRQ=4)
      CHARACTER*8 NAMERQ(MAXRQ)/'TIME','LOCAL','NLSTZ','NHOCAL'/
      CHARACTER*8 RQNAME(MAXRQ)/MAXRQ*' '/
      CHARACTER*15 RQVAL(MAXRQ)/MAXRQ*' '/
      PARAMETER (MAXOP=3)
      CHARACTER*8 OPNAME(MAXOP)/MAXOP*' '/
      CHARACTER*15 OPVAL(MAXOP)/MAXOP*' '/
C
C jgg following added by jto to fix bug r25-12 12/05
      CHARACTER*8 CHAR
      CHARACTER*80 LINE
      CHARACTER DLIM
      DATA DLIM/' '/
      INTEGER CLEN
C end of additions
C
      DIMENSION NDSTRK(MDSTRK)
C
      INCLUDE 'uio'
      INCLUDE 'ufreei'
      INCLUDE 'uunits'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/filesize/RCS/upmsiz.f,v $
     . $',                                                             '
     .$Id: upmsiz.f,v 1.5 2006/05/09 13:13:26 jgofus Exp $
     . $' /
C    ===================================================================
C
C
      ISTAT=0
C
      NUMRQ=0
      NUMOP=0
C
C  READ FILE TYPE
10    CALL RPCARD (IBUF,IERR)
      CALL ULINE (LP,1)
      WRITE (LP,*) ' '
      CALL WPCARD (IBUF)
      IF (IERR.GT.0) GO TO 20
      CALL UFREE (1,72)
C
C  CHECK IF BLANK CARD
      IF (NFIELD.EQ.0) GO TO 10
C
      NFLD=1
      NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
      XWORD=' '
      CALL UPACK1 (IBUF(IFSTRT(NFLD)),XWORD,NCHAR)
C
C  CHECK FOR COMMENT
      IF (XWORD.EQ.'$') GO TO 10
C
C  CHECK FOR END CARD
      IF (XWORD.EQ.'END') GO TO 30
C
C  CHECK FOR EXACT NUMBER OF FIELD ON THE CARD
      IF (NFIELD.NE.2) THEN
         CALL UEROR (LP,1,-1)
         WRITE (LP,90) XWORD
         GO TO 10
         ENDIF
C
C  CHECK FOR VALID COMMAND
      IF (XWORD.EQ.'TIME'.OR.
     *    XWORD.EQ.'LOCAL'.OR.
     *    XWORD.EQ.'NLSTZ'.OR.
     *    XWORD.EQ.'NHOCAL'.OR.
     *    XWORD.EQ.'ZOFF'.OR.
     *    XWORD.EQ.'CLKZONE'.OR.
     *    XWORD.EQ.'INTERVAL') THEN
         ELSE
            CALL UEROR (LP,1,-1)
            WRITE (LP,100) XWORD
            GO TO 10
            ENDIF
C
      NFLD=2
C
      IF (XWORD.EQ.'TIME') THEN
         IF (NUMRQ+1.GT.MAXRQ) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'REQUIRED',MAXRQ
            GO TO 10
            ENDIF
         NUMRQ=NUMRQ+1
         RQNAME(NUMRQ)=XWORD
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         IF (NCHAR.LT.1.OR.NCHAR.GT.4) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,120) XWORD
            GO TO 10
            ENDIF
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),RQVAL(NUMRQ),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'LOCAL') THEN
         IF (NUMRQ+1.GT.MAXRQ) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'REQUIRED',MAXRQ
            GO TO 10
            ENDIF
         NUMRQ=NUMRQ+1
         RQNAME(NUMRQ)=XWORD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'INTEGER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),RQVAL(NUMRQ),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'NLSTZ') THEN
         IF (NUMRQ+1.GT.MAXRQ) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'REQUIRED',MAXRQ
            GO TO 10
            ENDIF
         NUMRQ=NUMRQ+1
         RQNAME(NUMRQ)=XWORD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'INTEGER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),RQVAL(NUMRQ),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'NHOCAL') THEN
         IF (NUMRQ+1.GT.MAXRQ) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'REQUIRED',MAXRQ
            GO TO 10
            ENDIF
         NUMRQ=NUMRQ+1
         RQNAME(NUMRQ)=XWORD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'INTEGER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),RQVAL(NUMRQ),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'ZOFF') THEN
         IF (NUMOP+1.GT.MAXOP) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'OPTIONAL',MAXOP
            GO TO 10
            ENDIF
         NUMOP=NUMOP+1
         OPNAME(NUMOP)=XWORD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'INTEGER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),OPVAL(NUMOP),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'CLKZONE') THEN
         IF (NUMOP+1.GT.MAXOP) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'OPTIONAL',MAXOP
            GO TO 10
            ENDIF
         NUMOP=NUMOP+1
         OPNAME(NUMOP)=XWORD
         IF (IFTYPE(NFLD).NE.3) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'CHARACTER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),OPVAL(NUMOP),NCHAR)
         GO TO 10
         ENDIF
C
      IF (XWORD.EQ.'INTERVAL') THEN
         IF (NUMOP+1.GT.MAXOP) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,110) 'OPTIONAL',MAXOP
            GO TO 10
            ENDIF
         NUMOP=NUMOP+1
         OPNAME(NUMOP)=XWORD
         IF (IFTYPE(NFLD).NE.1) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,130) XWORD(1:LENSTR(XWORD)),NFLD,'INTEGER'
            GO TO 10
            ENDIF
         NCHAR=IFSTOP(NFLD)-IFSTRT(NFLD)+1
         LRQVAL=LEN(RQVAL(NUMRQ))
         IF (NCHAR.GT.LRQVAL) THEN
            CALL UEROR (LP,1,-1)
            WRITE (LP,125) XWORD(1:LENSTR(XWORD)),NCHAR,LRQVAL
            GO TO 10
            ENDIF
         CALL UPACK1 (IBUF(IFSTRT(NFLD)),OPVAL(NUMOP),NCHAR)
         GO TO 10
         ENDIF
C
20    ISTAT=1
C
30    NDSTRK(KUPARM)=1
      CALL ULINE (LP,2)
      WRITE (LP,140) 'USERPARM',KUPARM,NDSTRK(KUPARM)
C
      IF (ISTAT.EQ.0) THEN
         CALL ULINE (LPUNCH,2)
C jgg following added by jto to fix bug r25-12 12/05
         WRITE (LPUNCH,150) ' '

         WRITE (LPUNCH,150) 'UPRM'
C     CHECK IF REQUIRED KEYWORDS SPECIFIED
         IF (NUMRQ.EQ.0) THEN
            DO 40 I=1,MAXRQ
               CALL UWARN (LP,2,-1)
               WRITE (LP,170) NAMERQ(I),'?'
               CALL ULINE (LPUNCH,1)
C jgg following lines replaced by jto with below for bug r25-12
C                WRITE (LPUNCH,160) NAMERQ(I)(1:LENSTR(NAMERQ(I))),
C      *            '?'
               LINE=' '//NAMERQ(I)
               CALL KKCONC(LINE,'?',DLIM)
               WRITE (LPUNCH,150) LINE(1:LENSTR(LINE))
C end of changes
40             CONTINUE
            ENDIF
         IF (NUMRQ.GT.0) THEN
            DO 50 I=1,NUMRQ
               IF (RQNAME(I).NE.' ') THEN
                  CALL ULINE (LPUNCH,1)
C jgg following lines replaced by jto with below for bug r25-12
C                   WRITE (LPUNCH,160) RQNAME(I)(1:LENSTR(RQNAME(I))),
C      *               RQVAL(I)(1:LENSTR(RQVAL(I)))
                  LINE=' '//RQNAME(I)
                  CALL KKCONC(LINE,RQVAL(I),DLIM)
                  WRITE (LPUNCH,150) LINE(1:LENSTR(LINE))
C end of changes
                  ENDIF
50          CONTINUE
            DO 70 I=1,MAXRQ
               DO 60 J=1,NUMRQ
                  IF (NAMERQ(I).EQ.RQNAME(J)) GO TO 70
60                CONTINUE
               CALL UWARN (LP,2,-1)
               WRITE (LP,170) NAMERQ(I)(1:LENSTR(NAMERQ(I))),
     *            '?'
               CALL ULINE (LPUNCH,1)
C jgg following lines replaced by jto with below for bug r25-12
C                WRITE (LPUNCH,160) NAMERQ(I)(1:LENSTR(NAMERQ(I))),
C      *            '?'
               LINE=' '//NAMERQ(I)
               CALL KKCONC(LINE,'?',DLIM)
               WRITE (LPUNCH,150) LINE(1:LENSTR(LINE))
C end of changes
70             CONTINUE
            ENDIF
C     CHECK IF OPTIONAL KEYWORDS SPECIFIED
         IF (NUMOP.GT.0) THEN
            DO 80 I=1,NUMOP
               IF (OPNAME(I).NE.' ') THEN
                  CALL ULINE (LPUNCH,1)
C jgg following lines replaced by jto with below for bug r25-12
C                   WRITE (LPUNCH,160) OPNAME(I)(1:LENSTR(OPNAME(I))),
C      *               OPVAL(I)(1:LENSTR(OPVAL(I)))
                  LINE=' '//OPNAME(I)
                  CALL KKCONC(LINE,OPVAL(I),DLIM)
                  WRITE (LPUNCH,150) LINE(1:LENSTR(LINE))
C end of changes

                  ENDIF
80             CONTINUE
            ENDIF
         CALL ULINE (LPUNCH,1)
C jgg following line replaced by jto with below for bug r25-12
C          WRITE (LPUNCH,160) 'END'
         WRITE (LPUNCH,150) ' END'
      ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
90    FORMAT ('+*** ERROR - INVALID NUMBER OF FIELD ON ',A,' CARD.')
100   FORMAT ('+*** ERROR - ',A,' IS AN INVALID COMMAND. ')
110   FORMAT ('+*** ERROR - MAXIMUM NUMBER OF ',A,' FIELDS (',I2,
     *   ') EXCEEDED. ')
120   FORMAT ('+*** ERROR - INVALID KEYWORD ON ',A,' CARD.')
125   FORMAT ('+*** ERROR - NUMBER OF CHARACTERS IN VALUE FOR KEYWORD ',
     *   A,' (',I2,') IS GREATER THAN ',I2,'.')
130   FORMAT ('+*** ERROR - VALUE FOR KEYWORD ',A,
     *   ' FOUND IN FIELD ',I2,
     *   ' IS NOT ',A,'.')
C jgg following lines replaced by jto with below for bug r25-12
C 140   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I3)
C 150   FORMAT (/ A)
C 160   FORMAT (A,1X,A)
140   FORMAT ('0',15X,'TRACKS FOR FILE ',A,' UNIT ',I2.2,' = ',I5)
150   FORMAT (A)
C end of changes
170   FORMAT ('+*** WARNING - ',A,' NOT SPECIFIED AND ',
     *   'WILL BE SET TO ',A,'.')
C
      END
