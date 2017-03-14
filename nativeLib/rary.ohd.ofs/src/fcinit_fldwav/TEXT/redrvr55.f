      SUBROUTINE REDRVR55(SYSTM,RIVR,JN)

C
C MR 1954 - 09/2004 FLDWAV Multi-Scenario Enhancement
C
      CHARACTER*4 RIVR(20,JN),SYSTM(20)
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)
      COMMON/IONUM/IN,IPR,IPU

      CHARACTER*8 SNAME

      DATA SNAME/'REDRVR55'/
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/redrvr55.f,v $
     . $',                                                             '
     .$Id: redrvr55.f,v 1.3 2004/09/24 22:46:44 jgofus Exp $
     . $' /
C    ===================================================================
C

      CALL FPRBUG(SNAME,1,55,IBUG)
      READ(IN,'(20A4)') SYSTM
      IF(IBUG.EQ.1) WRITE(IODBUG,'(1X,20A4)') (SYSTM(K),K=1,20)
      DO 620 J=1,JN
        READ(IN,'(20A4)') (RIVR(K,J),K=1,20)
        IF(IBUG.EQ.1) WRITE(IODBUG,'(1X,20A4)') (RIVR(K,J),K=1,20)
  620 CONTINUE
      
	RETURN
	END
