C MEMBER MPDIM
C----------------------------------------------------------------------
C
      SUBROUTINE MPDIM (D)
C
C  THIS ROUTINE DIMENSIONS EACH VARIABLE ARRAY BY MAKING IT
C  EQUIVALENT TO PART OF THE D ARRAY.  THE VARIABLES L1,L2, ETC., ARE
C  THE POINTERS FOR THE LOCATION OF THE FIRST ELEMENT OF EACH ARRAY.
C
      INCLUDE 'common/ionum'
      COMMON /DIM/ M1,M2,M3,M4,M5,M6
C
      DIMENSION D(M5)
C
      EQUIVALENCE (ICVAR1,LDEBUG)
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/calb/src/map/RCS/mpdim.f,v $
     . $',                                                             '
     .$Id: mpdim.f,v 1.3 2003/08/14 19:00:12 gzhou Exp $
     . $' /
C    ===================================================================
C
C
C
      LDEBUG=0
C
      IUSTOP=0
C
      IF (M5.GT.1) GO TO 10
         WRITE (IPR,*) 'M5=',M5
         CALL USTOP (IPR,IUSTOP)
C
C  COMPUTE THE REQUIRED ARRAY SIZE
10    M6=M3/12+1
C
C  MC IS THE MAXIMUM NUMBER OF OB TIME CHANGES
C  FACTORS
      MC=10
      IBASE = M1*28 + M2*8 + M3*6 + M1*M2*2 + M1*M3 + M2*M6*12
      IADD1 = M1*4 + M1*MC*5 + 6400
      IADD2 = M1*817
      IADD3 = M2*749
      ICK=0
      IF (IADD1.GE.IADD2.AND.IADD1.GT.IADD3) ICK=IBASE+IADD1
      IF (IADD2.GE.IADD1.AND.IADD2.GT.IADD3) ICK=IBASE+IADD2
      IF (IADD3.GE.IADD1.AND.IADD3.GT.IADD2) ICK=IBASE+IADD3
C
      IF (M5.GE.ICK) GO TO 20
         WRITE (IPR,30) M5,ICK,M1,M2,M3
         CALL USTOP (IPR,IUSTOP)
C
C  THE FOLLOWING POINTERS ARE FOR ARRAYS THAT MUST BE IN STORAGE
C  AT ALL TIMES
20    L1=1
      L2=L1+M1*7
      L3=L2+M2*5
      L4=L3+M2*3
      L5=L4+M1*3
      L6=L5+M3*3
      L7=L6+M1*M3
      L8=L7+M2*M6*12
      L9=L8+M1
      L10=L9+M1
      L11=L10+M1
      L12=L11+M1*M2*2
      L13=L12+M1
      L14=L13+M1
      L15=L14+M1
      L16=L15+M1*12
      L17=L16+M3
C
C  THE FOLLOWING POINTERS ARE FOR ARRAYS THAT ARE USED IN THE FIRST
C  PART OF THE PROGRAM AND CAN BE EQUIVALENCED.
      L18=L17+M3*2
      L19=L18+M1
      L20=L19+M1
      L21=L20+M1
      L22=L21+M1
      L23=L22+M1*MC
      L24=L23+M1*MC
      L25=L24+M1*MC
      L26=L25+M1*MC
      L27=L26+M1*MC
      L28=L27+6400
C
C  THE FOLLOWING POINTER IS FOR THE C ARRAY WHICH CAN BE EQUIVALENCED
C  TO ARRAYS NO LONGER NEEDED
      L29=L18
C
C  THE FOLLOWING POINTERS ARE FOR ARRAYS THAT ARE USED TO LIST THE
C  MAP DATA FROM THE DISK AND CAN BE EQUIVALENCED TO ARRAYS NO LONGER
C  NEEDED.
      L30=L18
      L31=L30+M2
      L32=L31+M2
      L33=L31+M3*3
      L34=L33+M2*744
C
      IF (LDEBUG.GT.0) THEN
         WRITE (IPR,40) L1,L2,L3,L4,L5,L6,L7,L8,L9,
     *     L10,L11,L12,L13,L14,L15,L16,L17,L18,L19,
     *     L20,L21,L22,L23,L24,L25,L26,L27,L28,L29,
     *     L30,L31,L32,L33,L34
         ENDIF
C
      CALL MAPMN (D(L1),D(L2),D(L3),
     *            D(L4),D(L5),D(L6),
     *            D(L7),
     *            D(L8),D(L9),
     *            D(L10),D(L11),
     *            D(L12),D(L13),D(L14),D(L15),
     *            D(L16),
     *            D(L17),D(L18),D(L19),D(L20),D(L21),
     *            D(L22),D(L23),D(L24),D(L25),D(L26),D(L27),
     *            D(L29),
     *            D(L30),D(L31),D(L32),D(L33),
     *            ICK,MC)
C
      RETURN
C
30    FORMAT ('0*** ERROR - THE SPECIFIED ARRAY SIZE (M5) ',I7,
     *   ' IS SMALLER THAN THE REQUIRED ARRAY SIZE (',I7,').' /
     *     T14,'NUMBER OF STATIONS = ',I3 /
     *     T14,'NUMBER OF SUBAREAS = ',I3 /
C Modified by gzhou -- r23-45	 
C     *     T14,'NUMBER OF MONTHS   = ',I3 )
     *     T14,'NUMBER OF MONTHS   = ',I4 )
40    FORMAT (' L1 - L32 : ' / 4(/(10I10)))
C
      END
