      SUBROUTINE REDRES55(K,J,SAR,HSAR,NODESC,IERR,K1,K16)
C
C  THIS SUBROUTINE READS RESERVOIR SURFACE AREA VS. ELEVATION CURVE
C  FOR USE IN RESERVOIR STORAGE ROUTING
C
      INCLUDE 'common/fdbug'
      INCLUDE 'common/ionum'
C
      CHARACTER*80 DESC
      CHARACTER*8  SNAME
      DIMENSION    SAR(8,K16,K1),HSAR(8,K16,K1)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcinit_fldwav/RCS/redres55.f,v $
     . $',                                                             '
     .$Id: redres55.f,v 1.3 2000/09/27 15:51:01 page Exp $
     . $' /
C    ===================================================================
C
      DATA  SNAME / 'REDRES55' /

      CALL FPRBUG(SNAME, 1, 55, IBUG)

C.......................................................................
C     SAR   --  SURFACE AREA OF RESERVOIR CORRESPONDING TO HSA
C     HSAR  --  ELEVATION AT WHICH RESEVOIR SURFACE IS REDEFINED
C.......................................................................
      IERR=0
      READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (SAR(L,K,J),L=1,8)
      IF(IBUG.EQ.0) GO TO 115
      IF(NODESC.EQ.0)THEN
        WRITE(IODBUG,5)
    5   FORMAT(//10X,
     .   'SAR  = SURFACE AREA OF RESERVOIR CORRESPONDING TO HSA'/10X,
     .   'HSAR = ELEVATION AT WHICH RESEVOIR SURFACE IS REDEFINED'/)
        WRITE(IODBUG,10) (SAR(L,K,J),L=1,8)
   10   FORMAT(/1X,'SAR : ',8F10.2)
      ELSE
        WRITE(IODBUG,110) K,J,(SAR(L,K,J),L=1,8)
  110   FORMAT(/3X,'SAR(L,',I2,',',I2,') L=1,8'/8F10.2)
      ENDIF

  115 READ(IN,'(A)',END=1000) DESC
      READ(IN,*) (HSAR(L,K,J),L=1,8)
      IF(IBUG.EQ.0) GO TO 9000
      IF(NODESC.EQ.0) THEN
        WRITE(IODBUG,20) (HSAR(L,K,J),L=1,8)
   20   FORMAT(1X,'HSAR: ',8F10.2)
      ELSE
        WRITE(IODBUG,120) K,J,(HSAR(L,K,J),L=1,8)
  120   FORMAT(/3X,'HSAR(L,',I2,',',I2,') L=1,8'/8F10.2)
      ENDIF
      GO TO 9000
 1000 WRITE(IPR,1010)
 1010 FORMAT(/5X,'**ERROR** END OF FILE ENCOUNTERED WHILE READING INPUT
     * FOR RESERVOIR INFO.'/)
      CALL ERROR
      IERR=1
 9000 RETURN
      END
