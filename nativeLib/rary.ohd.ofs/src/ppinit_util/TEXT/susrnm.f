C MODULE SUSRNM
C-----------------------------------------------------------------------
C
C  ROUTINE SUSRNM DECODES THE EXTERNAL FILE USERS NAMES PASSED THRU
C  THE PARM FIELD AND PRINTS A TABLE IF ANY OF THE NAMES DIFFER
C
      SUBROUTINE SUSRNM (IPRNT,PMFLD,IUSCLB,ISTAT)
C
C  THE INTERNAL AND EXTERNAL FILE NAMES ARE STORED IN THE ARRAY
C  USRNAM AS FOLLOWS:
C
C      SLOT    EXTERNAL     INTERNAL
C      ----    --------     --------
C        1     USERPARM     USERPARM
C        2     HCL          HCL
C        3     CLB          CLB
C        4     PDB          PDB
C        5     PPP          PDB
C        6     PRD          PRD
C        7     FC           FC
C        8     ESP          ESP
C
      DIMENSION ARRAY(200)
      CHARACTER*(*) PMFLD
      CHARACTER*8 TEMP
      CHARACTER*8 USER
      PARAMETER (MAXNAM=8)
      CHARACTER*8 USRNAM(MAXNAM,2)
      CHARACTER*8 XNA/'N/A'/
      PARAMETER (MOPTN=3)
      CHARACTER*8 OPTNS(MOPTN)
     *   /'DEBUG   ','TRACE   ','        '/
C
      INCLUDE 'uiox'
      INCLUDE 'ufreex'
      INCLUDE 'scommon/sudbgx'
      INCLUDE 'hclcommon/hdflts'
      INCLUDE 'pdbcommon/pdrrsc'
      INCLUDE 'pppcommon/ppxctl'
      INCLUDE 'prdcommon/pmaxdm'
      INCLUDE 'common/fcunit'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/ppinit_util/RCS/susrnm.f,v $
     . $',                                                             '
     .$Id: susrnm.f,v 1.3 2001/06/13 14:07:02 dws Exp $
     . $' /
C    ===================================================================
C
C
      IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'ENTER SUSRNM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C  SET DEBUG LEVEL
      LDEBUG=ISBUG('SYS ')
C
      ISTAT=0
C      
      DO 5 I=1,MAXNAM
         USRNAM(I,1)=' '
         USRNAM(I,2)=' '
5        CONTINUE
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DECODE PARM FIELD TO DETEMINE EXTERNAL USER NAMES
C
      IF (PMFLD.EQ.' ') THEN
         WRITE (LP,290)
         CALL SULINE (LP,2)
         GO TO 85
         ENDIF
      WRITE (LP,180) PMFLD(1:LENSTR(PMFLD))
      CALL SULINE (LP,2)
C
      LPMFLD=LENSTR(PMFLD)
      NWORDS=LPMFLD/4
      CALL UNPAKS (PMFLD,ICDBUF,NWORDS,LEN(ICDBUF),IERR)
      IF (LDEBUG.GT.0.AND.IERR.GT.0) THEN
         WRITE (IOSDBG,200) IERR
         CALL SULINE (IOSDBG,1)
         ENDIF
      CALL UFFIND (1,LPMFLD,IERR)
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,210)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      USER=XNA
      DO 10 I=1,MAXNAM
         USRNAM(I,1)=XNA
         USRNAM(I,2)=XNA
10       CONTINUE
C
      LFIELD=MAXNAM+1
      IF (LFIELD.GT.NFIELD) LFIELD=NFIELD
      DO 70 IFIELD=1,LFIELD
         IF (IFSTOP(IFIELD).GE.IFSTRT(IFIELD)) GO TO 20
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,220) IFIELD
               CALL SULINE (IOSDBG,1)
               ENDIF
            GO TO 70
20       NCHAR=IFSTOP(IFIELD)-IFSTRT(IFIELD)+1
         TEMP=XNA
         CALL UPACK1 (ICDBUF(IFSTRT(IFIELD):IFSTRT(IFIELD)),TEMP,NCHAR)
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,230) NFIELD,IFIELD,
     *         IFSTRT(IFIELD),IFSTOP(IFIELD),NCHAR,TEMP
            CALL SULINE (IOSDBG,1)
            ENDIF
         GO TO (50,60,60,60,60,60,60,60,60),IFIELD
            DO 30 NP=1,MOPTN
               CALL UNAMCP (TEMP,OPTNS(NP),IMATCH)
               IF (IMATCH.EQ.0) GO TO 40
30             CONTINUE
            WRITE (LP,240) TEMP
            CALL SULINE (LP,2)
            GO TO 70
40          IF (NP.EQ.1) LDEBUG=1
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,250)
               CALL SULINE (IOSDBG,1)
               ENDIF
            IF (NP.EQ.2) ISTRCE=1
            IF (LDEBUG.GT.0) THEN
               WRITE (IOSDBG,260)
               CALL SULINE (IOSDBG,1)
               ENDIF
            GO TO 70
50       USER=TEMP
         GO TO 70
60       USRNAM(IFIELD-1,1)=TEMP
70       CONTINUE
C
C  IF USER SPECIFIED, FILL USRNAM ARRAY WITH NAME
      IF (USER.NE.XNA) THEN
      DO 80 I=1,MAXNAM
         USRNAM(I,1)=USER
80       CONTINUE
         IF (LDEBUG.GT.0) THEN
            WRITE (IOSDBG,270) USER
            CALL SULINE (IOSDBG,1)
            ENDIF
         ENDIF
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IOSDBG,280) USER,(USRNAM(I,1),I=1,MAXNAM)
         CALL SULINE (IOSDBG,1)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  SET INDICATOR TO PRINT NOTES IF DATA BASE NOT ALLOCATED
85    INDMSG=-2
C
C  CHECK WHICH FILE ARE ALLOCATED
      IDUPRM=0
      IDHCL=0
      IDCLB=0
      IDPPD=0
      IDPPP=0
      IDPRD=0
      IDFC=0
      IDESP=0
      CALL SUDACK (INDMSG,IDUPRM,IDHCL,IDCLB,IDPPD,IDPPP,IDPRD,
     *   IDFC,IDESP,IERR)
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  DETERMINE INTERNAL FILE NAMES
C
      IRSTAT=0
C
C  USER PARAMETER DATA FILE
      IF (IDUPRM.EQ.1) CALL SUBSTR (HNAMRF(1),1,8,USRNAM(1,2),1)
C
C  HCL LOCAL DATA BASE
C
C  CALIBRATION DATA FILES
      IF (IUSCLB.EQ.1.AND.IDCLB.EQ.1) THEN
         NINDX=36
         CALL UREADT (NINDX,1,ARRAY,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUBSTR (ARRAY,1,8,USRNAM(3,2),1)
            ELSE
               IRSTAT=1
            ENDIF
         ENDIF
C
C  PREPROCESSOR DATA BASE
      IF (IDPPD.EQ.1) CALL SUBSTR (IPUSER(1),1,8,USRNAM(4,2),1)
C
C  PARAMETRIC DATA BASE
      IF (IDPRD.EQ.1) CALL SUBSTR (USERPP(1),1,8,USRNAM(5,2),1)
C
C  PROCESSED DATA BASE
      IF (IDPRD.EQ.1) CALL SUBSTR (USERPR(1),1,8,USRNAM(6,2),1)
C
C  FORECAST COMPONENT DATA BASE
      IF (IDFC.EQ.1) THEN
         CALL UREADT (KFCGD,1,ARRAY,IERR)
         IF (IERR.EQ.0) THEN
            CALL SUBSTR (ARRAY(4),1,4,USRNAM(7,2),1)
            ELSE
               IRSTAT=1
            ENDIF
         ENDIF
C
      IF (IRSTAT.GT.0) THEN
         WRITE (LP,410)
         CALL SULINE (LP,2)
         ENDIF
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
      IF (PMFLD.EQ.' ') GO TO 130
C
C  CHECK IF ANY USER NAMES DIFFER
C
      IDIFF=0
      INA=0
      NUM=MAXNAM-1
C
      DO 100 J=1,2
         DO 100 I=1,NUM
            IF (USRNAM(I,J).EQ.XNA) GO TO 90
            IF (USRNAM(I+1,J).EQ.XNA) GO TO 90
            IF (USRNAM(I,J).EQ.USRNAM(I+1,J)) GO TO 100
            IDIFF=1
            GO TO 100
90       INA=1
100      CONTINUE
C
      DO 120 I=1,MAXNAM
         IF (USRNAM(I,1).EQ.XNA) GO TO 120
            DO 110 J=1,MAXNAM
               IF (USRNAM(J,2).EQ.XNA) GO TO 110
               IF (USRNAM(I,1).EQ.USRNAM(J,2)) GO TO 110
               IDIFF=1
110            CONTINUE
120      CONTINUE
C
C  CHECK IF TABLE TO BE PRINTED EVEN IF USER NAMES NOT DIFFERENT
      IF (IPRNT.EQ.1) GO TO 130
C
      IF (IDIFF.EQ.0) GO TO 150
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
C  PRINT INTERNAL AND EXTERNAL FILE NAMES
130   IF (IDIFF.EQ.1) THEN
         WRITE (LP,300)
         CALL SULINE (LP,2)
         ENDIF
      WRITE (LP,310)
      CALL SULINE (LP,4)
      DO 140 I=1,MAXNAM
         IF (I.EQ.1) THEN
            WRITE (LP,320) 'USERPARM',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.2) THEN
            WRITE (LP,320) 'HCL     ',(USRNAM(I,J),J=1,2)
         IF (I.EQ.3.AND.IUSCLB.EQ.1) THEN             
            CALL SULINE (LP,1)
            ENDIF
            WRITE (LP,320) 'CLB     ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.4) THEN
            WRITE (LP,320) 'PDB     ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.5) THEN
            WRITE (LP,320) 'PPP     ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.6) THEN
            WRITE (LP,320) 'PRD     ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.7) THEN
            WRITE (LP,320) 'FC      ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
         IF (I.EQ.8) THEN
            WRITE (LP,320) 'ESP     ',(USRNAM(I,J),J=1,2)
            CALL SULINE (LP,1)
            ENDIF
140      CONTINUE
C
      IF (INA.EQ.1) THEN
         WRITE (LP,170)
         CALL SULINE (LP,2)
         WRITE (LP,400)
         CALL SULINE (LP,2)
         ENDIF
C
150   IF (ISTRCE.GT.0) THEN
         WRITE (IOSDBG,*) 'EXIT SUSRNM'
         CALL SULINE (IOSDBG,1)
         ENDIF
C
      RETURN
C
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C
170   FORMAT ('0')
180   FORMAT ('0*** NOTE - PARM FIELD : ',A)
200   FORMAT (' UNPAKS STATUS CODE = ',I2)
210   FORMAT (' UFREE CALLED')
220   FORMAT (' BLANK STRING FOUND IN FIELD ',I2)
230   FORMAT (' UPACK1 CALLED : NFIELD=',I2,3X,'IFIELD=',I2,3X,
     *   'IFSTRT=',I2,3X,'IFSTOP=',I2,3X,'NCHAR=',I2,3X,'TEMP=',A)
240    FORMAT ('0*** WARNING - IN SUSRNM - INVALID PARM FIELD OPTION ',
     *   ' : ',A)
250   FORMAT (' *** NOTE - TIMR OPTION SET')
260   FORMAT (' *** NOTE - TRACE OPTION SET')
270   FORMAT (' USRNAM ARRAY EXTERNAL NAMES FILLED WITH ',A)
280   FORMAT (' EXTERNAL FILE NAMES FROM PARM FIELD' /
     *   ' ',
     *   'USER=',A,2X,
     *   'UPRM=',A,2X,
     *   'UHCL=',A,2X,
     *   'UCLB=',A,2X,
     *   'UPDB=',A,2X,
     *   'UPPP=',A,2X,
     *   'UPRD=',A,2X,
     *   'UFC=',A,2X,
     *   'UESP=',A)
290   FORMAT ('0*** NOTE - IN SUSRNM - PARM FIELD IS EMPTY. INTERNAL ',
     *   'AND EXTERNAL FILE NAMES WILL NOT BE CHECKED.')
300   FORMAT ('0*** NOTE - ONE OR MORE OF THE INTERNAL AND EXTERNAL ',
     *   'FILE NAMES DIFFER. THE NAMES ARE AS FOLLOWS.')
310   FORMAT ('0',T19,'EXTERNAL',5X,'INTERNAL' /
     *     5X,'FILE',11X,'NAME',9X,'NAME' /
     *     3(5X,8('-')))
320   FORMAT (5X,A,2(5X,A))
400   FORMAT (5X,'N/A = DATA SETS NOT ALLOCATED OR INTERNAL ',
     *   'NAME CANNOT BE OBTAINED.')
410   FORMAT ('0*** WARNING - IN SUSRNM - ONE OR MORE DATA BASE NOT ',
     *   'SUCCESSFULLY READ. TABLE OF FILE NAMES MAY NOT BE COMPLETE.')
C
      END
