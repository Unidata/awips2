C MODULE NRDHCL
C-----------------------------------------------------------------------
C
C  ROUTINE TO GET RUN OPTIONS AND SYSTEM CONTROLS FROM HCL.
C
      SUBROUTINE NRDHCL
C
C----------------------------------------------------------
C     WRITTEN BY R SHEDD (HRL) 12/90
C----------------------------------------------------------
C
      CHARACTER*4 CODE
      CHARACTER*8 OLDOPN
C
      PARAMETER (LIARGS=60)
      DIMENSION IARGS(LIARGS)
C
      INCLUDE 'common/ionum'
      INCLUDE 'common/pudbug'
      INCLUDE 'common/fctime'
      INCLUDE 'common/fctim2'
      INCLUDE 'common/errdat'
      COMMON /HDFLTS/ IHDFLT(25)
      COMMON /NFUT/ IFUTR
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_mapx/RCS/nrdhcl.f,v $
     . $',                                                             '
     .$Id: nrdhcl.f,v 1.4 2002/02/11 20:43:39 dws Exp $
     . $' /
C    ===================================================================
C
C
      IOPNUM=-1
      CALL FSTWHR ('NRDHCL  ',IOPNUM,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'ENTER NRDHCL'
C
      INDERR=0
C
      IOUNT=0
      IF (IOUNT.EQ.1) THEN
C
C  GET VALUE FOR IPR IN COMMON IONUM
      CALL HPAST ('PRINT   ',IPR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,'PRINT   ')
      IF (IPR.EQ.6.OR.IPR.EQ.8.OR.IPR.EQ.9) THEN
         ELSE
            IPR=6
            WRITE (IPR,100) 'PRINT',IPR
            CALL WARN
         ENDIF
C
C  GET VALUE FOR IOERR IN COMMON ERRDAT
      CALL HPAST ('ERRORPR ',IOERR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,'ERRORPR ')
      IF (IOERR.EQ.6.OR.IOERR.EQ.8.OR.IOERR.EQ.9) THEN
         ELSE
            IOERR=9
            WRITE (IPR,100) 'ERRORPR',IOERR
            CALL WARN
         ENDIF
C
C  GET VALUES FOR COMMON PUDBUG
      CALL HPAST ('DEBUGPR ',IOPDBG,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,'DEBUGPR ')
      IF (IOPDBG.EQ.6.OR.IOPDBG.EQ.8.OR.IOPDBG.EQ.9) THEN
         ELSE
            IOPDBG=6
            WRITE (IPR,100) 'DEBUGPR',IOPDBG
            CALL WARN
         ENDIF
C
      ENDIF
C
      CALL HPAST ('PPTRACE ',IPTRCE,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,'PPTRACE ')
      CALL FTEKCK (IPTRCE,'PPTRACE ',0,IPTRCE,0,5)
      NDBUG=0
      IPALL=0
      IALL=0
      CALL HPASTA ('PPDEBUG ',LIARGS,IVAL,NIARGS,IARGS,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL FPHPWN (ISTAT,'PPDEBUG ')
         IF (ISTAT.NE.1) GO TO 20
         ENDIF
      NIARGS=NIARGS/3
      DO 10 I=1,NIARGS
         CALL HGTSTR (1,IARGS(1+(I-1)*3),CODE,LCODE,ISTAT)
         IF (ISTAT.NE.0) THEN
            WRITE (IPR,120) ISTAT
            CALL ERROR
            GO TO 10
            ENDIF
         IF (CODE.EQ.' ') GO TO 10
         IF (CODE.EQ.'ALL') THEN
            IPALL=1
            IALL=1
            GO TO 20
            ENDIF
         NDBUG=NDBUG+1
         CALL UMEMOV (CODE,PDBUG(NDBUG),1)
10       CONTINUE
C
C  GET VALUE FOR TECHNIQUE STARTRUN
20    CALL HPASTA ('STARTRUN',LIARGS,IVAL,NIARGS,IARGS,ISTAT)
      IF (ISTAT.NE.0) THEN
         CALL FPHPWN (ISTAT,'STARTRUN')
         WRITE (IPR,110) 'STARTRUN'
         INDERR=1
         ELSE
            IF (NIARGS.NE.7) THEN
               WRITE (IPR,110) 'STARTRUN'
               INDERR=1
               ENDIF
         ENDIF
      IDARUN=IARGS(1)
C
C  GET VALUE FOR TECHNIQUE LSTCMPDY
      CALL HPASTA ('LSTCMPDY',LIARGS,IVAL,NIARGS,IARGS,ISTAT)
      IF (ISTAT.NE.0) THEN
          CALL FPHPWN (ISTAT,'LSTCMPDY')
          WRITE (IPR,110) 'LSTCMPDY'
          INDERR=1
          ELSE
             IF (NIARGS.NE.7) THEN
                WRITE (IPR,110) 'LSTCMPDY'
                INDERR=1
                ENDIF
          ENDIF
      LDACPD=IARGS(1)
C
C  GET VALUE FOR TECHNIQUE ENDRUN
      CALL HPASTA ('ENDRUN  ',LIARGS,IVAL,NIARGS,IARGS,ISTAT)
      IF (ISTAT.NE.0) THEN
          CALL FPHPWN (ISTAT,'ENDRUN  ')
          WRITE (IPR,110) 'ENDRUN'
          INDERR=1
          ELSE
            IF (NIARGS.NE.7) THEN
               WRITE (IPR,110) 'ENDRUN'
               INDERR=1
               ENDIF
          ENDIF
      LDARUN=IARGS(1)
C
C  GET FCTIME VARIABLES
      NHOPDB=IHDFLT(24)
      LOCAL=IHDFLT(22)
      NLSTZ=IHDFLT(23)
C
C  GET VALUE FOR TECHNIQUE FUTPRECP
      CALL HPAST ('FUTPRECP',IFUTR,ISTAT)
      IF (ISTAT.NE.0) CALL FPHPWN (ISTAT,'FUTPRECP')
C
      IF (INDERR.EQ.1) CALL STOPFN ('MAPX    ')

      CALL FSTWHR (OLDOPN,IOLDOP,OLDOPN,IOLDOP)
C
      IF (IPTRCE.GE.1) WRITE (IOPDBG,*) 'EXIT NRDHCL'
C
      RETURN
C
100   FORMAT ('0**WARNING** VALUE OF TECHNIQUE ',A,
     &   ' MUST BE 6, 8 OR 9. ','IT HAS BEEN SET TO ',I2,'.')
110   FORMAT ('0**ERROR** VALUE OF TECHNIQUE ',A,
     &   ' NOT PROPERLY SPECIFIED.')
120   FORMAT ('0**ERROR** INVALID ARGUMENT FOR TECHNIQUE PPDEBUG. ',
     &   'HGTSTR STATUS CODE = ',I2)
C
      END
