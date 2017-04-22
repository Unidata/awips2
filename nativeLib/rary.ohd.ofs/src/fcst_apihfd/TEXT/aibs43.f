c  =====================================================================
c  pgm:  aibs43 (len,line,andx,anati)
c
c  out: len    .... length of table
c  out: line   .... ai curve lines
c  out: andx   .... antecedent precipitation index
c  out: anati  .... antecedent index
c  =====================================================================
      subroutine aibs43 (len,line,andx,anati)
c.......................................................................
c  This routine initializes antecedent index array with summer values 
c  for the Buffalo area as used in the Northeast RFC api/ai relationship
c
c     len -  ENDING POSITION OF TABLE
c.......................................................................
c  Initially written by
c     Ken Mack   NERFC                              5/18/95
c.......................................................................
c
      include 'common/fdbug'
      include 'common/ionum'
c
      DIMENSION andx(15,5),anati(15,5)
      DIMENSION NLINE(5),INDX(15,5),IATI(15,5),LINE(5)
c  INDICIES FOR THE BUFFALO AREA
      DIMENSION I1(15),I2(15),I3(15),I4(15),I5(15)
      DIMENSION IR1(15),IR2(15),IR3(15),IR4(15),IR5(15)
      DIMENSION SUBNAM(2)
c
      EQUIVALENCE
     1  (I1(1),INDX(1,1)),  (I2(1),INDX(1,2)),  (I3(1),INDX(1,3)),
     2  (I4(1),INDX(1,4)),  (I5(1),INDX(1,5))

      EQUIVALENCE
     1  (IR1(1),IATI(1,1)),  (IR2(1),IATI(1,2)),  (IR3(1),IATI(1,3)),
     2  (IR4(1),IATI(1,4)),  (IR5(1),IATI(1,5))
C
C    ================================= RCS keyword statements ==========
      CHARACTER*68     RCSKW1,RCSKW2
      DATA             RCSKW1,RCSKW2 /                                 '
     .$Source: /fs/hseb/ob72/rfc/ofs/src/fcst_apihfd/RCS/aibs43.f,v $
     . $',                                                             '
     .$Id: aibs43.f,v 1.1 1996/03/21 15:51:14 page Exp $
     . $' /
C    ===================================================================
C

      DATA NLINE / 33, 40, 45, 55, 60/

c  API INDEX FOR THE BUFFALO AREA
      DATA I1 /0,50,70,90,120,190,290,375,500,0,0,0,0,0,0/
      DATA I2 /0,45,65,90,120,170,245,315,400,500,0,0,0,0,0/
      DATA I3 /0,45,75,105,125,170,215,270,360,500,0,0,0,0,0/
      DATA I4 /0,35,80,110,145,185,225,275,335,415,500,0,0,0,0/
      DATA I5 /0,36,85,125,150,180,210,235,270,320,380,500,0,0,0/
c  ANTECEDENT INDEX FOR THE BUFFALO AREA
      DATA IR1 /2910,2060,1825,1650,1490,1275,1095,1000,1000,0,0,0,0,0,
     10/
      DATA IR2 /3995,3015,2705,2405,2150,1850,1525,1325,1155,1010,0,0,0,
     10,0/
      DATA IR3 /5120,4095,3545,3110,2895,2510,2255,2010,1750,1440,0,0,0,
     10,0/
      DATA IR4 /6690,5760,4705,4110,3590,3130,2800,2510,2250,2000,1790,
     10,0,0,0/
      DATA IR5 /8000,7900,6230,5080,4515,3980,3555,3290,3000,2695,2415,
     11990,0,0,0/
c
      DATA SUBNAM /4HAIBS,4H43  /,NOP/43/,IVERS/1/
      CALL FPRBUG(SUBNAM,2,NOP,IBUG)
      len = 5
 1000 do 1020 i = 1,len
      line(i) = nline(i)
 1020 continue

      do 1030 i = 1,15
      do 1035 j = 1,len
      andx(i,j) = indx(i,j) / 100.
      anati(i,j) = iati(i,j)
 1035 continue
 1030 continue
c
      RETURN
      END
