C MEMBER URWPTR
C-----------------------------------------------------------------------
C
      SUBROUTINE URWPTR (XTYPE,IPARRY,ISTAT)
C
C  THIS ROUTINE WRITES THE POINTER RECORDS TO THE FOR THE SPECIFIED
C  DATA TYPE TO THE NEW PREPROCESSOR DATA BASE.
C
C  ARGUMENT LIST:
C
C    NAME    TYPE   I/O   DIM   DESCRIPTION
C    ------  ----   ---   ---   -----------
C    XTYPE    A4     I     1    DATA TYPE
C    IPARRY   I*2    I     ?    POINTER ARRAY
C    ISTAT    I*4    O     1    STATUS CODE:
C                                 0=NORMAL RETURN
C                                 1=DATA TYPE NOT FOUND
C                                 2=SYSTEM ERROR
C
      CHARACTER*1 XCAR
      CHARACTER*4 XTYPE
C
      INTEGER*2 IPARRY(*)
C
      INCLUDE 'uiox'
      INCLUDE 'udebug'
      INCLUDE 'ucommon/uordrx'
      INCLUDE 'pdbcommon/pddtdr'
      INCLUDE 'urcommon/urpddt'
      INCLUDE 'urcommon/urcdta'
      INCLUDE 'urcommon/ursifc'
      INCLUDE 'urcommon/urunts'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/reorder/RCS/urwptr.f,v $
     . $',                                                             '
     .$Id: urwptr.f,v 1.3 2002/02/11 21:12:44 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'ENTER URWPTR'
         CALL SULINE (IOGDB,1)
         ENDIF
C
      ISTAT=0
C
C  FIND TYPE IN DATA TYPE DIRECTORY
      IAMORD=1
      IDX=IPDCKD(XTYPE)
      IF (IDX.EQ.0) THEN
         WRITE (LP,100) XTYPE
         CALL SUERRS (LP,2,-1)
         ISTAT=1
         GO TO 70
         ENDIF
C
C  COMPUTE NUMBER OF POSITIONS USED
      LUSED=JDDTDR(18,IDX)
      LRLUR2=LRLURD*2
      NPTREC=IUNRCD(LUSED,LRLUR2)
      IMAX=NPTREC*LRLUR2
      NUM=IMAX-LUSED
      IF (NUM.GT.0) THEN
C     INITIALIZE END OF POINTER ARRAY
         IBEG=LUSED+1
         DO 10 I=IBEG,IMAX
            IPARRY(I)=-9999
10          CONTINUE
         ENDIF
C
C  CHECK PP24 POINTER ARRAY FOR FOUR ZERO VALUES AND ONE NON-ZERO
      IF (XTYPE.EQ.'PP24') THEN
         NSET=0
         NPER=5
         NCHK=4
         IF (IPDDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         ' XTYPE=',XTYPE,
     *         ' LUSED=',LUSED,
     *         ' NPER=',NPER,
     *         ' NCHK=',NCHK,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         DO 30 IPOS=1,LUSED,NPER
            IBEG=IPOS
            IEND=IPOS+NCHK-1
            DO 20 NPOS=IBEG,IEND
               IF (IPDDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *               ' IPOS=',IPOS,
     *               ' NPOS=',NPOS,
     *               ' IPARRY(NPOS)=',IPARRY(NPOS),
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
               IF (IPARRY(NPOS).NE.0) GO TO 30
20             CONTINUE
            NPOS=IPOS+NCHK
            IF (IPARRY(NPOS).NE.0) THEN
               IF (IPDDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *               ' IPOS=',IPOS,
     *               ' NCHK=',NCHK,
     *               ' IPARRY(NPOS)=',IPARRY(NPOS),
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
               IPARRY(NPOS)=0
               IF (NSET.EQ.0) THEN
                  XCAR='0'
                  NLINES=2
                  ELSE
                     XCAR=' '
                     NLINES=1
                  ENDIF
               WRITE (LP,110) XCAR,NPOS,'PP24'
               CALL SULINE (LP,NLINES)
               NSET=NSET+1
               ENDIF
30          CONTINUE
         IF (NSET.GT.0) THEN
            WRITE (LP,130) NSET,'PP24'
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  CHECK PPVR POINTER ARRAY FOR MISSING SYMBOLS
      IF (XTYPE.EQ.'PPVR') THEN
         NSET=0
         NPER=4
         NCHK=4
         IF (IPDDB.GT.0) THEN
            WRITE (IOGDB,*)
     *         ' XTYPE=',XTYPE,
     *         ' LUSED=',LUSED,
     *         ' NPER=',NPER,
     *         ' NCHK=',NCHK,
     *         ' '
            CALL SULINE (IOGDB,1)
            ENDIF
         DO 60 IPOS=1,LUSED,NPER
            IBEG=IPOS
            IEND=IPOS+NCHK-1
            DO 40 NPOS=IBEG,IEND
               IF (IPDDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *               ' IPOS=',IPOS,
     *               ' NPOS=',NPOS,
     *               ' IPARRY(NPOS)=',IPARRY(NPOS),
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
               IVALUE=IPARRY(NPOS)
               IF (IABS(IVALUE).NE.9999) GO TO 60
40             CONTINUE
            DO 50 NPOS=IBEG,IEND
               IF (IPDDB.GT.0) THEN
                  WRITE (IOGDB,*)
     *               ' IPOS=',IPOS,
     *               ' NPOS=',NPOS,
     *               ' IPARRY(NPOS)=',IPARRY(NPOS),
     *               ' '
                  CALL SULINE (IOGDB,1)
                  ENDIF
               IPARRY(NPOS)=0
50             CONTINUE
            IF (NSET.EQ.0) THEN
               XCAR='0'
               NLINES=2
               ELSE
                  XCAR=' '
                  NLINES=1
               ENDIF
            WRITE (LP,120) XCAR,IBEG,IEND,'PPVR'
            CALL SULINE (LP,NLINES)
            NSET=NSET+1
60          CONTINUE
         IF (NSET.GT.0) THEN
            WRITE (LP,130) NSET,'PPVR'
            CALL SULINE (LP,2)
            ENDIF
         ENDIF
C
C  GET FILE NUMBER
      IFILE=JDDTDR(4,IDX)
C
C  WRITE POINTER ARRAY
      IUNIT=KURDDF(IFILE)
      IREC=JDDTDR(14,IDX)
      CALL WVLRCD (IUNIT,IREC,NPTREC,IPARRY,LRLURD,IERR)
      IF (IERR.GT.0) THEN
          WRITE (LP,140) IERR
          CALL SUERRS (LP,2,-1)
          ISTAT=2
          GO TO 70
          ENDIF
      CALL ULINE (LP,2)
      WRITE (LP,150) XTYPE
      GO TO 80
C
C  SET ERROR FLAG
70    IWURFL=1
C
80    IF (IPDTR.GT.0) THEN
         WRITE (IOGDB,*) 'EXIT URWPTR - ISTAT=',ISTAT
         CALL SULINE (IOGDB,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
100   FORMAT ('0*** ERROR - IN URWPTR - DATA TYPE ',A,' NOT DEFINED ',
     *   'IN THE NEW PREPROCESSOR DATA BASE.')
110   FORMAT (A,'*** NOTE - IN URWPTR - POSITION ',I5,
     *   ' IN ',A,' POINTER ARRAY SET TO ZERO.')
120   FORMAT (A,'*** NOTE - IN URWPTR - POSITIONS ',I5,' THROUGH ',I5,
     *   ' IN ',A,' POINTER ARRAY SET TO ZERO.')
130   FORMAT ('0*** NOTE - IN URWPTR - ',I5,' POSITIONS IN ',A,
     *   ' POINTER ARRAY SET TO ZERO.')
140   FORMAT ('0*** ERROR - IN URWPTR - SYSTEM ERROR. WVLRCD STATUS ',
     *   'CODE=',I2,'.')
150   FORMAT ('0*** NOTE - POINTER RECORDS FOR DATA TYPE ',A,
     *   ' SUCCESSFULLY COPIED.')
C
      END
