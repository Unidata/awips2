C MODULE TRAN26
C-----------------------------------------------------------------------
C
C  ROUTINE TO TRANSFER ALL INPUT FOR RES-SNGL OPERATION TO UNIT 89.
C
      SUBROUTINE TRAN26 (ENDSTR,NENDSTR)
C
C  ARGUMENT LIST:
C    ENDSTR - CHARACTER STRING DENOTING END OF TRANSFER
C   NENDSTR - NUMBER OF WORDS IN ENDSTR
C
      CHARACTER*4 ENDSTR(NENDSTR)
      CHARACTER*32 FILNAM
      CHARACTER*80 STRNG
C
      INCLUDE 'uiox'
      common /CMFCINIT/ PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS
      INCLUDE 'upvrsx_types'
      INCLUDE 'common/fld26'
      INCLUDE 'common/read26'
C
      SAVE I89
      DATA I89/-1/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_res/RCS/tran26.f,v $
     . $',                                                             '
     .$Id: tran26.f,v 1.3 2001/06/13 09:51:26 mgm Exp $
     . $' /
C    ===================================================================
C

C     Setup the upvrsx common block
      call set_upvrsx(PGMVRN,PGMVRD,PGMNAM,MPGMRG,PGMCMP,PGMSYS)

C
      MUNI26=89
C
      IF (PGMNAM(1:4).EQ.'MCP3'.OR.PGMNAM(1:4).EQ.'OPT3') THEN
         IF (I89.EQ.-1) THEN
             FILNAM=''
             CALL UPOPEN (MUNI26,FILNAM,0,'F',IERR)
             I89=0
           ENDIF
         ENDIF
C
      REWIND MUNI26
C
      NCD26=0
C
10    READ (ICD,20,END=30) STRNG
      WRITE (MUNI26,20) STRNG
20    FORMAT (A)
      NCD26=NCD26+1
      ISAME=IUSAME(STRNG,ENDSTR,NENDSTR)
      IF (ISAME.NE.1) GO TO 10
      GO TO 40
C
C  NO CARD WITH 'ENDSTR' FOUND ON IT
30    CALL STER26 (21,1)
C
40    RETURN
C
      END
