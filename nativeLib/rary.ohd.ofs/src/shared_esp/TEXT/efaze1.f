C MEMBER EFAZE1
C  (from old member EEFAZE1)
C
      SUBROUTINE EFAZE1(MD,D)
C
C   THIS SUBROUTINE SETS UP RUN INFO PRIOR TO EXECUTING ONE
C   SEGMENT AT A TIME.
C
C   THIS SUBROUTINE WAS ORIGINALLY WRITTEN BY GERALD N DAY (HRL).
C
C
      INCLUDE 'common/fcrunc'
      INCLUDE 'common/ionum'
      INCLUDE 'common/fdbug'
      INCLUDE 'common/where'
      INCLUDE 'common/killcd'
      INCLUDE 'common/fcary'
      INCLUDE 'common/fcsegn'
      include 'common/egentr'
C
      DIMENSION SBNAME(2),OLDOPN(2),D(1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/shared_esp/RCS/efaze1.f,v $
     . $',                                                             '
     .$Id: efaze1.f,v 1.3 2000/12/19 16:14:28 jgofus Exp $
     . $' /
C    ===================================================================
C
CFAN2007
C     DR18520: ESP aborting run 
C     line 73: CALL FGETSG(ID,IRSGEX(1),X,1,X,1,X,1,IOPT,NOPARM,IER)
C     ID need defined. (It is OK in Fortran 77 if it is not defines)
C
      CHARACTER*8 ID           
C
CFAN2007
C
      DATA SBNAME/4HEFAZ,4HE1  /
C
      IOLDOP=IOPNUM
      IOPNUM=0
      DO 10 I=1,2
      OLDOPN(I)=OPNAME(I)
   10 OPNAME(I)=SBNAME(I)
C
      IF(ITRACE.GE.1) WRITE(IODBUG,900)
  900 FORMAT(1H0,17H** EFAZE1 ENTERED)
C
C   WRITE ESP RUN INFORMATION
C
cew moved this call down so that shift could be written out also.
c      CALL EWTINF
C
C   CHECK RUN TYPE AND FILL CB FCRUNC -
C   STORES LIST OF SEGMENTS IN ORDER OF EXECUTION.
C
      CALL FCORDR(ITYPRN,RUNID,IER,D,MD)
      IF(IER.EQ.0) GO TO 20
      WRITE(IPR,600) ITYPRN,RUNID
  600 FORMAT(1H0,10X,48H**ERROR** COMMON BLOCK /FCRUNC/ CANNOT BE FILLED
     1,14H FOR ITYPRN = ,I5,14H, AND RUNID = ,2A4)
      CALL KILLFN(8HESP     )
      GO TO 999
   20 CONTINUE
C
C   GET SEGMENT INFORMATION FOR FIRST SEGMENT TO BE
C   RUN INTO CB FCSEGN.
C
      IOPT=1
      NOPARM=1
      CALL FGETSG(ID,IRSGEX(1),X,1,X,1,X,1,IOPT,NOPARM,IER)
      IF(IER.EQ.0) GO TO 25
      WRITE(IPR,605) IDSEGN
  605 FORMAT(1H0,10X,48H**ERROR** COMMON BLOCK /FCSEGN/ CANNOT BE FILLED
     1   ,13H FOR SEGMENT ,2A4,1H.)
      CALL KILLFN(8HESP     )
      GO TO 999
   25 CONTINUE
C
C   GET DATES OF AVAILABLE CO INTO CB FCSEGC
C
c bypass this step if generating historical trace files
c
      if(igen.eq.0) then
      CALL FCDATE(IDSEGN,0)
      endif
C
C   1) SET CO DATE FOR START OF RUN,
C   2) CALCULATE START TIME OF RUN IN LST,
C   3) CALCULATE SHIFT NECESSARY TO MAKE RUN TIMES COMPATIBLE
C      WITH IDT,
C   4) CALCULATE IDARUN,
C   5) CONVERT WINDOWS TO LST AND SHIFT,
C   6) CHECK IF START OF WINDOW LT START OF RUN,
C   7) CHECK IF END OF WINDOW LT START OF WINDOW,
C   8) CALCULATE END OF RUN AS END OF LAST WINDOW,
C   9) CALCULATE LDARUN.
C
      CALL ETMCHK

cew write out ESP info
cew moved from above so that shift info could be written out
      CALL EWTINF
C
  999 CONTINUE
      IOPNUM=IOLDOP
      OPNAME(1)=OLDOPN(1)
      OPNAME(2)=OLDOPN(2)
      RETURN
      END
