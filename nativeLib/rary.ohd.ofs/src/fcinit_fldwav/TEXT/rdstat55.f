      SUBROUTINE RDSTAT55(PO,IPO,IUSEP,LEFTP,JN,IOBS,NBT,NGAGE,NGS,
     . NRCM1,NQCM,NCM,YQCM,KD,NODESC,IERR,K1,K4,K7,K8)

      CHARACTER*80 DESC

      COMMON/MXVAL55/MXNB,MXNGAG,MXNCM1,MXNCML,MXNQL,MXINBD,MXRCH,
     #               MXMGAT,MXNXLV,MXROUT,MXNBT,MXNSTR,MXSLC
      COMMON/IONUM/IN,IPR,IPU
      COMMON/FDBUG/IODBUG,ITRACE,IDBALL,NDEBUG,IDEBUG(20)

      INCLUDE 'common/ofs55'

      DIMENSION PO(*),IPO(*),NBT(K1),NGAGE(K1),NGS(K4,K1),KD(K1)
      DIMENSION NRCM1(K1),NQCM(K1),NCM(K7,K1),YQCM(K8,K7,K1)
      CHARACTER*8 SNAME
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/rdstat55.f,v $
     . $',                                                             '
     .$Id: rdstat55.f,v 1.4 2004/08/25 18:47:49 jgofus Exp $
     . $' /
C    ===================================================================
C
	DATA SNAME/ 'RDSTAT55' /

      CALL FPRBUG(SNAME,1,55,IBUG)

        LONSLC=IUSEP+1
        LONQSL=LONSLC+JN
        IUSEP=LONQSL+JN-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

      IF(IOBS.EQ.3) THEN
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,ERR=1000) (IPO(LONSLC+J-1),J=1,JN)
        IF(IBUG.EQ.1) WRITE(IODBUG,8100) (IPO(LONSLC+J-1),J=1,JN)
        READ(IN,'(A)',END=1000) DESC
        READ(IN,*,ERR=1000) (IPO(LONQSL+J-1),J=1,JN)
        IF(IBUG.EQ.1) WRITE(IODBUG,8100) (IPO(LONQSL+J-1),J=1,JN)
      ELSE
        DO 500 J=1,JN
          IPO(LONSLC+J-1)=IABS(NQCM(J))
          IPO(LONQSL+J-1)=0
          IF(NQCM(J).LT.0) IPO(LONQSL+J-1)=1
  500   CONTINUE
      ENDIF

      MXSLC=1
      DO 100 J=1,JN
        IF(IPO(LONSLC+J-1).GT.MXSLC) MXSLC=IPO(LONSLC+J-1)
  100 CONTINUE
C jgg changed the following to fix HSD bug r23-48
C jgg      PO(100)=MXSLC+0.01
      K29=MXSLC
      IF(K29.EQ.0) K29=1
      PO(100)=K29 + 0.01
C jgg end of changes

      MUL=MXSLC*MXNGAG*JN

      LOSLIC=IUSEP+1
      LOFRMO=LOSLIC+MUL
      LOFBIO=LOFRMO+MUL
      LORRMO=LOFBIO+MUL
      LORBIO=LORRMO+MUL
      IUSEP=LORBIO+MUL-1

      CALL CHECKP(IUSEP,LEFTP,NERR)
      IF(NERR.EQ.1) THEN
        IUSEP=0
        GO TO 5000
      ENDIF

C...enter slice info to adjust computed t.s. to match obs t.s.
C...       IOBS=2 => use manning n slices
C...       IOBS=3 => read in slices

      MUL1=MXSLC*MXNGAG
      IF(IOBS.EQ.2) THEN
        DO 1085 J=1,JN
          LSLIC=LOSLIC+MUL1*(J-1)-1
          LFRMO=LOFRMO+MUL1*(J-1)-1
          LFBIO=LOFBIO+MUL1*(J-1)-1
          LRRMO=LORRMO+MUL1*(J-1)-1
          LRBIO=LORBIO+MUL1*(J-1)-1
          NSLICE=IPO(LONSLC+J-1)
          NGAG=NGAGE(J)
          IF(NGS(NGAG,J).EQ.NBT(J).AND.J.EQ.1.AND.KD(1).LE.1) 
     .          NGAG=NGAG-1
          NRCM=NRCM1(J)
          DO 1080 I=1,NGAG
            NTT=NGS(I,J)

            DO 1070 K=1,NRCM
              IF(NTT.GT.NCM(K,J).AND.NRCM.GT.1) GO TO 1070
              DO 1065 L=1,NSLICE
                KM1=K-1
                IF(KM1.LT.1) KM1=1
                PO(LSLIC+L)=YQCM(L,KM1,J)
 1065         CONTINUE
              GO TO 1075
 1070       CONTINUE
 1075       READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LFRMO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LFBIO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LRRMO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LRBIO+K),K=1,NSLICE)
            LSLIC=LSLIC+MXSLC
            LFRMO=LFRMO+MXSLC
            LFBIO=LFBIO+MXSLC
            LRRMO=LRRMO+MXSLC
            LRBIO=LRBIO+MXSLC
 1080     CONTINUE
 1085   CONTINUE
      ELSE
        DO 1095 J=1,JN
          LSLIC=LOSLIC+MUL1*(J-1)-1
          LFRMO=LOFRMO+MUL1*(J-1)-1
          LFBIO=LOFBIO+MUL1*(J-1)-1
          LRRMO=LORRMO+MUL1*(J-1)-1
          LRBIO=LORBIO+MUL1*(J-1)-1
          NSLICE=IPO(LONSLC+J-1)
          NGAG=NGAGE(J)
          IF(NGS(NGAG,J).EQ.NBT(J)) NGAG=NGAG-1
          DO 1090 I=1,NGAG
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LSLIC+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LFRMO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LFBIO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LRRMO+K),K=1,NSLICE)
            READ(IN,'(A)',END=1000) DESC
            READ(IN,*,ERR=1000) (PO(LRBIO+K),K=1,NSLICE)
            LSLIC=LSLIC+MXSLC
            LFRMO=LFRMO+MXSLC
            LFBIO=LFBIO+MXSLC
            LRRMO=LRRMO+MXSLC
            LRBIO=LRBIO+MXSLC
 1090     CONTINUE
 1095   CONTINUE
      ENDIF

      IF(IBUG.EQ.0) GO TO 2025

      DO 2020 J=1,JN
        LSLIC=LOSLIC+MUL1*(J-1)-1
        LFRMO=LOFRMO+MUL1*(J-1)-1
        LFBIO=LOFBIO+MUL1*(J-1)-1
        LRRMO=LORRMO+MUL1*(J-1)-1
        LRBIO=LORBIO+MUL1*(J-1)-1
        NGAG=NGAGE(J)
        IF(NGS(NGAG,J).EQ.NBT(J)) NGAG=NGAG-1
        DO 2015 I=1,NGAG
          NSLICE=IPO(LONSLC+J-1)
          WRITE(IODBUG,2001) J,I
 2001     FORMAT(10X,'RIVER NO.',I5,5X,'GAGE NO.',I5)
          WRITE(IODBUG,2002) (PO(LSLIC+K),K=1,NSLICE)
 2002     FORMAT(12X,'SLICE: ',10F12.1)
          WRITE(IODBUG,2004) (PO(LFRMO+K),K=1,NSLICE)
 2004     FORMAT(12X,'FRMSO:  ',10F12.3)
          WRITE(IODBUG,2006) (PO(LFBIO+K),K=1,NSLICE)
 2006     FORMAT(12X,'FBIASO: ',10F12.3)
          WRITE(IODBUG,2008) (PO(LRRMO+K),K=1,NSLICE)
 2008     FORMAT(12X,'RRMSO:  ',10F12.3)
          WRITE(IODBUG,2010) (PO(LRBIO+K),K=1,NSLICE)
 2010     FORMAT(12X,'RBIASO: ',10F12.3)
          LSLIC=LSLIC+MXSLC
          LFRMO=LFRMO+MXSLC
          LFBIO=LFBIO+MXSLC
          LRRMO=LRRMO+MXSLC
          LRBIO=LRBIO+MXSLC
 2015   CONTINUE
 2020 CONTINUE

      GO TO 2025
 1000 WRITE(IPR,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     *STATISTICS PARAMETERS.'/)

 5000 IERR=1
 8100 FORMAT(1H ,10X,7I10)
 2025 RETURN
      END
