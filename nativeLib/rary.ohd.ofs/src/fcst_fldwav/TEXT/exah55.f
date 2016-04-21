C-----------------------------------------------------------------------
C          THIS SUBPROGRAM DETERMINES HL BY KNOWN ATL=A+Ao
C-----------------------------------------------------------------------
      SUBROUTINE EXAH55(J,NCS,L,ATL,HL,HS,BS,BSS,AS,ASS,K1,K2,K9)
      DIMENSION HS(K9,K2,K1),BS(K9,K2,K1),BSS(K9,K2,K1)
      DIMENSION AS(K9,K2,K1),ASS(K9,K2,K1),SNAME(2)

      INCLUDE 'common/fdbug'
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_fldwav/RCS/exah55.f,v $
     . $',                                                             '
     .$Id: exah55.f,v 1.1 1999/04/23 18:08:30 dws Exp $
     . $' /
C    ===================================================================
C

      DATA SNAME/4HEXAH,4H55  /
C
      CALL FPRBUG(SNAME,1,55,IBUG)

            AT1=AS(1,L,J)+ASS(1,L,J)
            ATN=AS(NCS,L,J)+ASS(NCS,L,J)
            BTN=BS(NCS,L,J)+BSS(NCS,L,J)

            IF (ATL.LE.AT1) THEN
            HL=HS(1,L,J)

            ELSE IF(ATL.GT.ATN) THEN
            DA=ATL-ATN
            DH12=HS(NCS,L,J)-HS(NCS-1,L,J)
            DBDH=(BS(NCS,L,J)-BS(NCS-1,L,J))/DH12 
            DB0H=(BSS(NCS,L,J)-BSS(NCS-1,L,J))/DH12
                      IF ((DBDH+DB0H).LE.0.0001) THEN
                      DH=DA/BTN
                      ELSE
                      TA=0.5*(DBDH+DB0H)
                      TB=BTN
                      TC=-1.0*DA
                      DH=(-TB+(TB*TB-4.0*TA*TC)**0.5)/(2.0*TA)
                      ENDIF
                      HL=HS(NCS,L,J)+DH
                      
             ELSE
             DO 410 I=1,NCS-1
             ATI=AS(I,L,J)+ASS(I,L,J)
410          IF(ATL.GE.ATI) JJ1=I
             DA=ATL-(AS(JJ1,L,J)+ASS(JJ1,L,J))
             DH12=HS(JJ1+1,L,J)-HS(JJ1,L,J)
             DBDH=(BS(JJ1+1,L,J)-BS(JJ1,L,J))/DH12
             DB0H=(BSS(JJ1+1,L,J)-BSS(JJ1,L,J))/DH12
                      IF ((DBDH+DB0H).LE.0.0001) THEN
                      DH=DA/(BS(JJ1,L,J)+BSS(JJ1,L,J))
                      ELSE
                      TA=0.5*(DBDH+DB0H)
                      TB=BS(JJ1,L,J)+BSS(JJ1,L,J)
                      TC=-1.0*DA
                      DH=(-TB+(TB*TB-4.0*TA*TC)**0.5)/(2.0*TA)
                      ENDIF
                      HL=HS(JJ1,L,J)+DH
             ENDIF         
      RETURN  
      END
