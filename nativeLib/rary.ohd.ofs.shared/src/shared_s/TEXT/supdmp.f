C MODULE SUPDMP
C-----------------------------------------------------------------------
C
C  ROUTINE TO PRINT DUMP OF PARAMETER ARRAY.
C
      SUBROUTINE SUPDMP (TYPE,ATYPE,NWORDS,LARRAY,ARRAY,IARRAY)
C
      CHARACTER*4 DTYPE,ATYPE
C
      DIMENSION ARRAY(LARRAY),IARRAY(LARRAY)
C
      INCLUDE 'uio'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'scommon/suoptx'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_s/RCS/supdmp.f,v $
     . $',                                                             '
     .$Id: supdmp.f,v 1.3 1999/07/06 15:34:18 page Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISDBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUPDMP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      DTYPE=ATYPE
      NVAL=NWORDS
      IF (NVAL.LT.1.OR.NVAL.GT.LARRAY) NVAL=LARRAY
C
      IF (ISLEFT(8).GT.0) CALL SUPAGE
C      
      WRITE (LP,70) TYPE,LARRAY,NWORDS,NVAL
      CALL SULINE (LP,2)
C
C  CHECK FOR DUMP FORMAT SET BY SETOPT COMMAND
      IF (IOPDMP.GT.0) THEN
         IF (IOPDMP.EQ.1) DTYPE='CHAR'
         IF (IOPDMP.EQ.2) DTYPE='REAL'
         IF (IOPDMP.EQ.3) DTYPE='BOTH'
         IF (IOPDMP.GT.3) DTYPE='BOTH'
         ENDIF
C
C  CHECK FOR VALID TYPE
      IF (DTYPE.EQ.'CHAR'.OR.
     *    DTYPE.EQ.'REAL'.OR.
     *    DTYPE.EQ.'BOTH'.OR.
     *    DTYPE.EQ.'INT'.OR.
     *    DTYPE.EQ.'INT2') GO TO 10
         WRITE (LP,60) DTYPE,'CHAR','INT','INT2','REAL','BOTH'
         CALL SUWRNS (LP,2,-1)
         DTYPE='BOTH'
C
C  DUMP IN REAL*4 FORMAT
10    IF (DTYPE.EQ.'REAL'.OR.DTYPE.EQ.'BOTH') THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,80)
         CALL SULINE (LP,1)
         WRITE (LP,110)
         CALL SULINE (LP,1)
         NPER=10
         NUM=NVAL
         IF (NUM.GT.NPER) NUM=NPER
         NUM1=1
         WRITE (LP,130) NUM1,(ARRAY(I),I=1,NUM)
         CALL SULINE (LP,1)
         IF (LARRAY.GT.NPER) THEN
            NTIME=(LARRAY-NPER)/NPER
            IF (MOD(LARRAY,NPER).NE.0) NTIME=NTIME+1
            NUM1=NPER+1
            NUM2=NPER*2
            DO 20 J=1,NTIME
               IF (NUM2.GT.LARRAY) NUM2=LARRAY
               WRITE (LP,130) NUM1,(ARRAY(I),I=NUM1,NUM2)
               CALL SULINE (LP,1)
               NUM1=NUM1+NPER
               NUM2=NUM2+NPER
20             CONTINUE
            ENDIF
         ENDIF
C
C  DUMP IN INTEGER*4 FORMAT
      IF (DTYPE.EQ.'INT') THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,90)
         CALL SULINE (LP,1)
         WRITE (LP,110)
         CALL SULINE (LP,1)
         NPER=10
         NUM=NVAL
         IF (NUM.GT.NPER) NUM=NPER
         NUM1=1
         WRITE (LP,140) NUM1,(ARRAY(I),I=1,NUM)
         CALL SULINE (LP,1)
         IF (LARRAY.GT.NPER) THEN
            NTIME=(LARRAY-NPER)/NPER
            IF (MOD(LARRAY,NPER).NE.0) NTIME=NTIME+1
            NUM1=NPER+1
            NUM2=NPER*2
            DO 30 J=1,NTIME
               IF (NUM2.GT.LARRAY) NUM2=LARRAY
               WRITE (LP,140) NUM1,(ARRAY(I),I=NUM1,NUM2)
               CALL SULINE (LP,1)
               NUM1=NUM1+NPER
               NUM2=NUM2+NPER
30             CONTINUE
            ENDIF
         ENDIF
C
C  DUMP IN INTEGER*2 FORMAT
      IF (DTYPE.EQ.'INT2') THEN
         CALL SUPDM2 (LARRAY*2,IARRAY,NVAL*2)
         ENDIF
C
C  DUMP IN CHARACTER FORMAT
      IF (DTYPE.EQ.'CHAR'.OR.DTYPE.EQ.'BOTH') THEN
         IF (ISLEFT(5).GT.0) CALL SUPAGE
         WRITE (LP,100)
         CALL SULINE (LP,1)
         WRITE (LP,120)
         CALL SULINE (LP,1)
         NPER=20
         NUM=NVAL
         IF (NUM.GT.NPER) NUM=NPER
         NUM1=1
         WRITE (LP,150) NUM1,(ARRAY(I),I=1,NUM)
         CALL SULINE (LP,1)
         IF (LARRAY.GT.NPER) THEN
            NTIME=(LARRAY-NPER)/NPER
            IF (MOD(LARRAY,NPER).NE.0) NTIME=NTIME+1
            NUM1=NPER+1
            NUM2=NPER*2
            DO 40 J=1,NTIME
               IF (NUM2.GT.LARRAY) NUM2=LARRAY
               WRITE (LP,150) NUM1,(ARRAY(I),I=NUM1,NUM2)
               CALL SULINE (LP,1)
               NUM1=NUM1+NPER
               NUM2=NUM2+NPER
40             CONTINUE
            ENDIF
         ENDIF
C
      WRITE (LP,160)
      CALL SULINE (LP,1)
C
      IF (ISDBUG.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUPDMP'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
60    FORMAT ('0*** WARNING - IN SUPDMP - ',A,' IS AN INVALID ARRAY ',
     *   'TYPE. VALID TYPES ARE : ',5(A,2X))
70    FORMAT ('0DUMP OF ',A,' PARAMETER ARRAY : ',
     *   'ARRAY LENGTH=',I5,' (FULL WORDS)',3X,
     *   'NUMBER OF WORDS REQUESTED=',I5,3X,
     *   'NUMBER OF WORDS DUMPED=',I5)
80    FORMAT (' *--> REAL*4    DUMP IN 10F8.2 FORMAT')
90    FORMAT (' *--> INTEGER*4 DUMP IN 10I8   FORMAT ')
100   FORMAT (' *--> CHARACTER DUMP IN 20A4   FORMAT')
110   FORMAT (' ',5X,10('-------+'))
120   FORMAT (' ',5X,20('---+'))
130   FORMAT (' ',I4,1X,10F8.2)
140   FORMAT (' ',I4,1X,10I8)
150   FORMAT (' ',I4,1X,20A4)
160   FORMAT (' *--> END OF DUMP OF PARAMETER ARRAY')
C
      END
