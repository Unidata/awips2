//
// GMSXnav.java
//

/*
This source file is part of the edu.wisc.ssec.mcidas package and is
Copyright (C) 1998 - 2009 by Tom Whittaker, Tommy Jasmin, Tom Rink,
Don Murray, James Kelly, Bill Hibbard, Dave Glowacki, Curtis Rueden
and others.
 
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free
Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
MA 02111-1307, USA
*/

package edu.wisc.ssec.mcidas;

import java.io.BufferedInputStream;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.lang.Math;

/**
 * This class implements GMSX navigation.  The code was modified
 * from the original FORTRAN code (nvxgmsx.dlm) on the McIDAS system. It
 * only supports latitude/longitude to line/element transformations (LL)
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see edu.wisc.ssec.mcidas.AREAnav
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author Tommy Jasmin, University of Wisconsin, SSEC
 */

public class GMSXnav extends AREAnav

{

  private byte bParms[] = new byte[3200];
  private int navType = 0;
  private float subLat;
  private float subLon;
  private float [] resLin = new float[4];
  private float [] resEle = new float[4];
  private float [] rlic = new float[4];
  private float [] relmfc = new float[4];
  private float [] senssu = new float[4];
  private float [] rline = new float[4];
  private float [] relem = new float[4];
  private float [] vmis = new float[3];
  private float [][] elmis = new float[3][3];
  private double lineOffset;
  private double dtims = 0.0d;
  private double dspin = 0.0d;
  private double sitagt = 0.0d;
  private double sunalp = 0.0d;
  private double sundel = 0.0d;
  private double [] sat = new double[3];
  private double [] sp = new double[3];
  private double [] ss = new double[3];
  private double [][] orbt1 = new double[35][8];
  private double [][] atit = new double[10][10];

  // class constants
  private static final double cdr = Math.PI / 180.0d;
  private static final double crd = 180.0d / Math.PI;
  private static final double hpai = Math.PI / 2.0d;
  private static final double dpai = Math.PI * 2.0d;
  private static final double ea = 6378136.0d;
  private static final double ef = 1.0d / 298.257d;


  /**
   *
   * constructor: copy nav block to a byte array, eliminating text fields
   *
   * @param navBlock - the navigation block from the image file
   *
   */

  public GMSXnav(int[] navBlock)

  {

    int i;
    int j;
    int index = 0;
    byte [] tmpArr;

    // copy data to new array, taking out text at start of each block
    for (i = 0; i < 126; i++) {
      tmpArr = intToBytes(navBlock[i + 1]);
      for (j = 0; j < 4; j++) {
        bParms[index] = tmpArr[j];
        index++;
      }
    }

    int srcOffset = 128;

    for (int block = 0; block < 4; block++) {
      for (i = 0; i < 127; i++) {
        tmpArr = intToBytes(navBlock[i + srcOffset]);
        for (j = 0; j < 4; j++) {
          bParms[index] = tmpArr[j];
          index++;
        }
      }
      srcOffset += 128;
    }

    decOABlock(bParms, 0);

  } 


  /**
   *
   * toLinEle converts lat/lon to satellite line/element
   *
   * @param array of lat/long pairs. Where latlon[indexLat][]
   * are latitudes and latlon[indexLon][] are longitudes.
   *
   * @return linele[][] array of line/element pairs.  Where
   * linele[indexLine][] is a line and linele[indexEle][] is an element.       
   *
   */

  public synchronized float[][] toLinEle (float[][] latlon) 
  {

    int mode = 1;
    int count = latlon[0].length;
    float [] rtnPoint; 
    float[][] linele = new float[2][count];
    float line = 0.0f;
    float elem = 0.0f;
    float lon = 0.0f;

    for (int point = 0; point < count; point++) {

      // initialize value as not navigable
      linele[indexLine][point] = Float.NaN; 
      linele[indexEle][point] = Float.NaN; 

      if (Math.abs(latlon[indexLat][point]) > 90.0) {
        continue;
      }

      // normalize to -180 to 180
      lon = latlon[indexLon][point];
      if (lon > 180.f) lon = lon - 360.f;
      if (lon < -180.f) lon = lon + 360.f;
      if (Math.abs(latlon[indexLon][point]) > 180.f) continue;
      if (-lon > 90-subLon && -lon < 270-subLon) continue;
      
      rtnPoint = mgivsr (  
        mode, 
        (float) elem, 
        (float) line, 
        (float) lon, 
        (float) latlon[indexLat][point]
      );

      linele[indexLine][point] = rtnPoint[0];
      linele[indexEle][point] = rtnPoint[1];

    }

    // Return in 'File' coordinates
    return imageCoordToAreaCoord(linele, linele);

  }

  public synchronized double[][] toLinEle (double[][] latlon) 
  {

    int mode = 1;
    int count = latlon[0].length;
    float [] rtnPoint; 
    double[][] linele = new double[2][count];
    double line = 0.0d;
    double elem = 0.0d;
    double lon = 0.0f;

    for (int point = 0; point < count; point++) {

      // initialize value as not navigable
      linele[indexLine][point] = Double.NaN; 
      linele[indexEle][point] = Double.NaN; 

      if (Math.abs(latlon[indexLat][point]) > 90.0) {
        continue;
      }

      // normalize to -180 to 180
      lon = latlon[indexLon][point];
      if (lon > 180.0) lon = lon - 360.0;
      if (lon < -180.0) lon = lon + 360.0;
      if (Math.abs(latlon[indexLon][point]) > 180.0) continue;
      if (-lon > 90-subLon && -lon < 270-subLon) continue;
      
      rtnPoint = mgivsr (  
        mode, 
        (float) elem, 
        (float) line, 
        (float) lon, 
        (float) latlon[indexLat][point]
      );

      linele[indexLine][point] = rtnPoint[0];
      linele[indexEle][point] = rtnPoint[1];

    }

    // Return in 'File' coordinates
    return imageCoordToAreaCoord(linele, linele);

  }

  /**
   *
   * toLatLon converts satellite line/element to lat/lon
   *
   * @param linele[][] array of line/element pairs.  Where
   * linele[indexLine][] is a line and linele[indexEle][] is an element.       
   *
   * @return array of lat/lon pairs. Where latlon[indexLat][]
   * are latitudes and latlon[indexLon][] are longitudes.
   *
   */

  public synchronized float[][] toLatLon (float[][] linele) 
  {

    int mode = -1;
    int count = linele[0].length;
    float [] rtnPoint;
    float[][] latlon = new float[2][count];
    float lat  = 0.0f;
    float lon  = 0.0f;

    // transform file to image coordinates
    float[][] imgLinEle = areaCoordToImageCoord(linele);

    for (int point = 0; point < count; point++) {

      // initialize value as not navigable
      latlon[indexLat][point] = Float.NaN; 
      latlon[indexLon][point] = Float.NaN; 

      rtnPoint = mgivsr (  
        mode, 
        (float) imgLinEle[indexEle][point], 
        (float) imgLinEle[indexLine][point], 
        (float) lon, 
        (float) lat
      );

      // normalize to -180 to 180
      lon = rtnPoint[1];
      if (lon > 180) lon = lon - 360;
      if (lon < -180) lon = lon + 360;
      if (Math.abs(lon) > 180 ||  (-lon > 90-subLon && -lon < 270-subLon)) {
          rtnPoint[0] = Float.NaN;
          lon  = Float.NaN;
      }

      latlon[indexLat][point] = rtnPoint[0];
      latlon[indexLon][point] = lon;

    }

    return (latlon);

  }


  public synchronized double[][] toLatLon (double[][] linele) 
  {

    int mode = -1;
    int count = linele[0].length;
    float [] rtnPoint;
    double[][] latlon = new double[2][count];
    double lat  = 0.0d;
    double lon  = 0.0d;

    // transform file to image coordinates
    double[][] imgLinEle = areaCoordToImageCoord(linele);

    for (int point = 0; point < count; point++) {

      // initialize value as not navigable
      latlon[indexLat][point] = Double.NaN; 
      latlon[indexLon][point] = Double.NaN; 

      rtnPoint = mgivsr (  
        mode, 
        (float) imgLinEle[indexEle][point], 
        (float) imgLinEle[indexLine][point], 
        (float) lon, 
        (float) lat
      );

      // normalize to -180 to 180
      lon = rtnPoint[1];
      if (lon > 180.) lon = lon - 360.;
      if (lon < -180.) lon = lon + 360.;
      if (Math.abs(lon) > 180. ||  (-lon > 90.-subLon && -lon < 270.-subLon)) {
          rtnPoint[0] = Float.NaN;
          lon  = Float.NaN;
      }

      latlon[indexLat][point] = rtnPoint[0];
      latlon[indexLon][point] = lon;

    }

    return (latlon);

  }

  /**
   *
   * sv0100 converts 4 or 6 byte byte values to double
   *
   * @param iWord - size of word in bytes from input array
   * @param iPos - power of 10 to multiply result
   * @param b - byte array to extract input bytes from
   * @param bOffs - offset in byte array to begin conversion
   *
   * @return converted value
   *
   */

  private double sv0100 (
    int iWord,
    int iPos,
    byte[] b,
    int bOffs
  ) 

  {

    boolean negative = false; 
    int [] tmpInt = new int[6];
    double r8Dat = 0.0d;

    if (b[bOffs] < 0) {
      negative = true;
    }

    if (iWord == 4) {
      for (int i = 1; i < 4; i++) {
        if (b[i + bOffs] < 0) {
          tmpInt[i] = (b[i + bOffs] & 0x7F) + 128;
        } else {
          tmpInt[i] = b[i + bOffs];
        }
      }
      r8Dat = (
        ((double) (b[0 + bOffs] & 0x7F) * 16777216.0d) +
        ((double) (tmpInt[1]) * 65536.0d) +
        ((double) (tmpInt[2]) * 256.0d) +
         (double) (tmpInt[3])
      );
    }

    if (iWord == 6) {
      for (int i = 1; i < 6; i++) {
        if (b[i + bOffs] < 0) {
          tmpInt[i] = (b[i + bOffs] & 0x7F) + 128;
        } else {
          tmpInt[i] = b[i + bOffs];
        }
      }
      r8Dat = (
        ((double) (b[0 + bOffs] & 0x7F) * Math.pow(2.0d, 40)) +
        ((double) (tmpInt[1]) * Math.pow(2.0d, 32)) +
        ((double) (tmpInt[2]) * 16777216.0d) +
        ((double) (tmpInt[3]) * 65536.0d) +
        ((double) (tmpInt[4]) * 256.0d) +
        ((double) (tmpInt[5]))
      );
    }

    r8Dat = r8Dat / Math.pow(10.0d, iPos);
    if (negative) {
      r8Dat = -r8Dat;
    }

    return(r8Dat);

  }


  /**
   *
   * decOABlock: decode Orbit and Attitude data block
   *
   * @param b - input byte array to decode
   * @param form - long (1) or short (0)
   *
   */

  private void decOABlock (
    byte[] b,
    int form
  )

  {

    int i = 0;
    int j = 0;
    int offset;
    int tmpLoInt;
    int tmpHiInt;
    long tmpLong;
    float r4Dmy = 0.0f;
    double r8Dmy = 0.0d;

    dtims = sv0100(6,  8, b,   0);
    dspin = sv0100(6,  8, b, 240);
    resLin[0] = (float) sv0100(4,  8, b,  6);
    resLin[1] = (float) sv0100(4,  8, b, 10);
    resLin[2] = (float) sv0100(4,  8, b, 10);
    resLin[3] = (float) sv0100(4,  8, b, 10);
    resEle[0] = (float) sv0100(4, 10, b, 14);
    resEle[1] = (float) sv0100(4, 10, b, 18);
    resEle[2] = (float) sv0100(4, 10, b, 18);
    resEle[3] = (float) sv0100(4, 10, b, 18);
    rlic[0] = (float) sv0100(4,  4, b,  22);
    rlic[1] = (float) sv0100(4,  4, b,  26);
    rlic[2] = (float) sv0100(4,  4, b, 110);
    rlic[3] = (float) sv0100(4,  4, b, 114);
    relmfc[0] = (float) sv0100(4,  4, b,  30);
    relmfc[1] = (float) sv0100(4,  4, b,  34);
    relmfc[2] = (float) sv0100(4,  4, b, 118);
    relmfc[3] = (float) sv0100(4,  4, b, 122);
    senssu[0] = (float) sv0100(4,  0, b,  38);
    senssu[1] = (float) sv0100(4,  0, b,  42);
    senssu[2] = (float) sv0100(4,  0, b,  42);
    senssu[3] = (float) sv0100(4,  0, b,  42);
    rline[0] = (float) sv0100(4,  0, b,  46);
    rline[1] = (float) sv0100(4,  0, b,  50);
    rline[2] = (float) sv0100(4,  0, b,  50);
    rline[3] = (float) sv0100(4,  0, b,  50);
    relem[0] = (float) sv0100(4,  0, b,  54);
    relem[1] = (float) sv0100(4,  0, b,  58);
    relem[2] = (float) sv0100(4,  0, b,  58);
    relem[3] = (float) sv0100(4,  0, b,  58);
    vmis[0] = (float) sv0100(4, 10, b,  62);
    vmis[1] = (float) sv0100(4, 10, b,  66);
    vmis[2] = (float) sv0100(4, 10, b,  70);
    elmis[0][0] = (float) sv0100(4,  7, b,  74);
    elmis[1][0] = (float) sv0100(4, 10, b,  78);
    elmis[2][0] = (float) sv0100(4, 10, b,  82);
    elmis[0][1] = (float) sv0100(4, 10, b,  86);
    elmis[1][1] = (float) sv0100(4,  7, b,  90);
    elmis[2][1] = (float) sv0100(4, 10, b,  94);
    elmis[0][2] = (float) sv0100(4, 10, b,  98);
    elmis[1][2] = (float) sv0100(4, 10, b, 102);
    elmis[2][2] = (float) sv0100(4,  7, b, 106);
    subLon = (float) sv0100(6,  6, b, 198);
    subLat = (float) sv0100(6,  6, b, 204);

    for (i = 0; i < 10; i++) {
      // long form
      if (form == 1) {
        j = ((i - 1) * 64) + 256;
      }
      // short form
      if (form == 0) {
        j = ((i - 1) * 48) + 256;
      }
      atit[0][i] = sv0100(6,  8, b, j);
      atit[2][i] = sv0100(6,  8, b, j + 12);
      atit[3][i] = sv0100(6, 11, b, j + 18);
      atit[4][i] = sv0100(6,  8, b, j + 24);
      atit[5][i] = sv0100(6,  8, b, j + 30);
    }

    for (i = 0; i < 8; i++) {
      // long form
      if (form == 1) {
        j = ((i - 1) * 256) + 896;
      }
      // short form
      if (form == 0) {
        j = ((i - 1) * 200) + 736;
      }
      orbt1[ 0][i] = sv0100(6,  8, b, j +   0);
      orbt1[ 8][i] = sv0100(6,  6, b, j +  48);
      orbt1[ 9][i] = sv0100(6,  6, b, j +  54);
      orbt1[10][i] = sv0100(6,  6, b, j +  60);
      orbt1[14][i] = sv0100(6,  8, b, j +  84);
      orbt1[17][i] = sv0100(6,  8, b, j + 102);
      orbt1[18][i] = sv0100(6,  8, b, j + 108);
      orbt1[19][i] = sv0100(6, 12, b, j + 128);
      orbt1[20][i] = sv0100(6, 14, b, j + 134);
      orbt1[21][i] = sv0100(6, 14, b, j + 140);
      orbt1[22][i] = sv0100(6, 14, b, j + 146);
      orbt1[23][i] = sv0100(6, 12, b, j + 152);
      orbt1[24][i] = sv0100(6, 16, b, j + 158);
      orbt1[25][i] = sv0100(6, 12, b, j + 164);
      orbt1[26][i] = sv0100(6, 16, b, j + 170);
      orbt1[27][i] = sv0100(6, 12, b, j + 176);
    }

    return;

  }


  /**
   *
   * subLatLon descr
   *
   * @param 
   *
   * @return 
   *
   */

  private void subLatLon (float[] ll) 

  {

    return;

  }


  /**
   *
   * mgivsr does the actual conversion to/from lat/lon or line/elem
   *
   * @param iMode - conversion mode, to lat/lon or to line/elem
   * @param rPix - float pixel or element value
   * @param rLin - float line value
   * @param rLat - float latitude value
   * @param rLon - float longitude value
   *
   * @return array of two floating point values, lat/lon or line/elem pair
   *
   */

  private float [] mgivsr (
    int iMode,
    float rPix,
    float rLin,
    float rLon,
    float rLat
  ) 

  {

    int rc = 0;
    int lMode;
    float rstep;
    float rsamp;
    float rfcl;
    float rfcp;
    float sens;
    float rftl;
    float rftp;
    float ri;
    float rj;
    float rio;
    float [] point = new float[2];
    double bc;
    double bs;
    double beta = 0.0d;
    double dd;
    double dda;
    double ddb;
    double ddc;
    double def;
    double ee;
    double en;
    double dk;
    double dk1;
    double dk2;
    double dLat = 0.0d;
    double dLon = 0.0d;
    double pc, ps;
    double qc, qs;
    double rtim = 0.0d; 
    double tf;
    double tl = 0.0d;
    double tp = 0.0d;
    double [] stn1 = new double[3];
    double [] sl;
    double [] slv = new double[3];
    double [] sx;
    double [] sy;
    double [] sw1;
    double [] sw2;
    double [] sw3 = new double[3];

    point[0] = Float.NaN;
    point[1] = Float.NaN;

    // parameter checks
    if (Math.abs(iMode) > 4) {
      rc = 1;
      return (point);
    }
    if ((Math.abs(rLat) > 90) && (iMode > 0)) {
      rc = 2;
      return (point);
    }

    // vissr frame information set
    lMode = Math.abs(iMode) - 1;
    rstep = resLin[lMode];
    rsamp = resEle[lMode];
    rfcl = rlic[lMode];
    rfcp = relmfc[lMode];
    sens = senssu[lMode];
    rftl = (float) (rline[lMode] + 0.5);
    rftp = (float) (relem[lMode] + 0.5);

    // transformation, geographical -> VISSR
    if (iMode > 0) {

      dLat = (double) rLat * cdr;
      dLon = (double) rLon * cdr;
      ee = (2.0d * ef) - (ef * ef);
      en = ea / Math.sqrt(1.0d - (ee * Math.sin(dLat) * Math.sin(dLat)));
      stn1[0] = en * Math.cos(dLat) * Math.cos(dLon);
      stn1[1] = en * Math.cos(dLat) * Math.sin(dLon);
      stn1[2] = (en * (1.0d - ee)) * Math.sin(dLat);
      rio = (float) (rfcl - 
        Math.atan(Math.sin(dLat) / (6.610689 - Math.cos(dLat))) / rstep);
      rtim = dtims + (double) (rio / sens / 1440.0d) / dspin;

      loop: while (true) {

        beta = mg1100(rtim);
        sw1 = mg1220(sp, ss);
        sw2 = mg1220(sw1, sp);
        bc = Math.cos(beta);
        bs = Math.sin(beta);
        sw3[0] = (sw1[0] * bs) + (sw2[0] * bc);
        sw3[1] = (sw1[1] * bs) + (sw2[1] * bc);
        sw3[2] = (sw1[2] * bs) + (sw2[2] * bc);
        sx = mg1200(sw3);
        sy = mg1220(sp, sx);
        slv[0] = stn1[0] - sat[0];
        slv[1] = stn1[1] - sat[1];
        slv[2] = stn1[2] - sat[2];
        sl = mg1200(slv);
        sw2 = mg1210(sp, sl);
        sw3 = mg1210(sy, sw2);
        tp = mg1230(sy, sw2);
        tf = (sp[0] * sw3[0]) + (sp[1] * sw3[1]) + (sp[2] * sw3[2]);
        if (tf < 0.0d) {
          tp = -tp;
        }
        tl = mg1230(sp, sl);
        ri = (float) (hpai - tl) / rstep + rfcl - (vmis[1] / rstep);
        rj = (float) (tp / rsamp + rfcp + (vmis[2] / rsamp) -
          (hpai - tl) * Math.tan(vmis[0]) / rsamp);
        if (Math.abs(ri - rio) >= 1.0d) {
          rtim = (double) (Math.rint((ri - 1) / sens) + 
            (rj * rsamp) / dpai) / (dspin * 1440.0) + dtims;
          rio = ri;
          continue loop;
        }
        break loop;
      }

      point[0] = ri;
      point[1] = rj;
      rLin = ri;
      rPix = rj;
      if ((rLin < 0) || (rLin > rftl)) {
        point[0] = Float.NaN;
        point[1] = Float.NaN;
        rc = 4;
      }
      if ((rPix < 0) || (rPix > rftp)) {
        point[0] = Float.NaN;
        point[1] = Float.NaN;
        rc = 5;
      }

    }
      // transformation, VISSR -> geographical
    if (iMode < 0) {
      rtim = (double) (Math.rint((rLin - 1) / sens) + 
        (rPix * rsamp) / dpai) / (dspin * 1440.0) + dtims;
      beta = mg1100(rtim);
      sw1 = mg1220(sp, ss);
      sw2 = mg1220(sw1, sp);
      bc = Math.cos(beta);
      bs = Math.sin(beta);
      sw3[0] = (sw1[0] * bs) + (sw2[0] * bc);
      sw3[1] = (sw1[1] * bs) + (sw2[1] * bc);
      sw3[2] = (sw1[2] * bs) + (sw2[2] * bc);
      sx = mg1200(sw3);
      sy = mg1220(sp, sx);
      pc = Math.cos(rstep * (rLin - rfcl));
      ps = Math.sin(rstep * (rLin - rfcl));
      qc = Math.cos(rsamp * (rPix - rfcp));
      qs = Math.sin(rsamp * (rPix - rfcp));
      sw1[0] = (elmis[0][0] * pc) + (elmis[0][2] * ps);
      sw1[1] = (elmis[1][0] * pc) + (elmis[1][2] * ps);
      sw1[2] = (elmis[2][0] * pc) + (elmis[2][2] * ps);
      sw2[0] = (qc * sw1[0]) - (qs * sw1[1]);
      sw2[1] = (qs * sw1[0]) + (qc * sw1[1]);
      sw2[2] = sw1[2];
      sw3[0] = (sx[0] * sw2[0]) + (sy[0] * sw2[1]) + (sp[0] * sw2[2]);
      sw3[1] = (sx[1] * sw2[0]) + (sy[1] * sw2[1]) + (sp[1] * sw2[2]);
      sw3[2] = (sx[2] * sw2[0]) + (sy[2] * sw2[1]) + (sp[2] * sw2[2]);
      sl = mg1200(sw3);
      def = (1.0d - ef) * (1.0d - ef);
      dda = def * ((sl[0] * sl[0]) + (sl[1] * sl[1])) + (sl[2] * sl[2]);
      ddb = def * ((sat[0] * sl[0]) + (sat[1] * sl[1])) + (sat[2] * sl[2]);
      ddc = def * ((sat[0] * sat[0]) + (sat[1] * sat[1]) - (ea * ea)) + 
        (sat[2] * sat[2]);
      dd = (ddb * ddb) - (dda * ddc);
      if ((dd >= 0.0d) && (dda != 0.0d)) {
        dk1 = (-ddb + Math.sqrt(dd)) / dda;
        dk2 = (-ddb - Math.sqrt(dd)) / dda;
      } else {
        rc = 6;
        return (point);
      }
      if (Math.abs(dk1) < Math.abs(dk2)) {
        dk = dk1;
      } else {
        dk = dk2;
      }
      stn1[0] = sat[0] + (dk * sl[0]);
      stn1[1] = sat[1] + (dk * sl[1]);
      stn1[2] = sat[2] + (dk * sl[2]);
      dLat = Math.atan(stn1[2] / 
        (def * Math.sqrt((stn1[0] * stn1[0]) + (stn1[1] * stn1[1]))));
      if (stn1[0] != 0.0d) {
        dLon = Math.atan(stn1[1] / stn1[0]);
        if ((stn1[0] < 0.0d) && (stn1[1] >= 0.0d)) {
          dLon = dLon + Math.PI;
        }
        if ((stn1[0] < 0.0d) && (stn1[1] < 0.0d)) {
          dLon = dLon - Math.PI;
        }
      } else {
        if (stn1[1] > 0.0d) {
          dLon = hpai;
        } else {
          dLon = -hpai;
        }
      }
      point[0] = (float) (dLat * crd);
      point[1] = (float) (dLon * crd);
    }

    return (point);

  }


  /**
   *
   * mg1100 conversion routine of some sort
   *
   * @param rtim - ?
   *
   * @return converted value ?
   *
   */

  private double mg1100 (
    double rtim
  ) 

  {

    double beta = 0.0d;
    double delt = 0.0d;
    double attalp = 0.0d;
    double attdel = 0.0d;
    double wkcos = 0.0d;
    double wksin = 0.0d;
    double [] att1 = new double[3];
    double [] att2 = new double[3];
    double [] att3 = new double[3];
    double [][] npa = new double[3][3]; 

    for (int i = 0; i < 7; i++) {
      if ((rtim > orbt1[0][i]) && (rtim < orbt1[0][i+1])) {
        npa = mg1110(i, rtim, orbt1);
        break;
      }
    }

    for (int i = 0; i < 9; i++) {
      if ((rtim >= atit[0][i]) && (rtim < atit[0][i+1])) {
        delt = (rtim - atit[0][i]) / (atit[0][i+1] - atit[0][i]);
        attalp = atit[2][i] + (atit[2][i+1] - atit[2][i]) * delt;
        attdel = atit[3][i] + (atit[3][i+1] - atit[3][i]) * delt;
        beta   = atit[4][i] + (atit[4][i+1] - atit[4][i]) * delt;
        if ((atit[4][i+1] - atit[4][i]) > 0.0d) {
          beta = atit[4][i] + (atit[4][i+1] - atit[4][i] - 360.0d * cdr) * delt;
        }
        break;
      }
    }

    wkcos   = Math.cos(attdel);
    att1[0] = Math.sin(attdel);
    att1[1] = wkcos * (-Math.sin(attalp));
    att1[2] = wkcos * Math.cos(attalp);

    att2[0] = (
      (npa[0][0] * att1[0]) + 
      (npa[0][1] * att1[1]) + 
      (npa[0][2] * att1[2])
    );
    att2[1] = (
      (npa[1][0] * att1[0]) + 
      (npa[1][1] * att1[1]) + 
      (npa[1][2] * att1[2])
    );
    att2[2] = (
      (npa[2][0] * att1[0]) + 
      (npa[2][1] * att1[1]) + 
      (npa[2][2] * att1[2])
    );

    wksin = Math.sin(sitagt);
    wkcos = Math.cos(sitagt);

    att3[0] = ( wkcos * att2[0]) + (wksin * att2[1]);
    att3[1] = (-wksin * att2[0]) + (wkcos * att2[1]);
    att3[2] = att2[2];
    sp = mg1200(att3);

    wkcos   = Math.cos(sundel);
    ss[0] = wkcos * Math.cos(sunalp);
    ss[1] = wkcos * Math.sin(sunalp);
    ss[2] = Math.sin(sundel);

    return(beta);

  }


  /**
   *
   * mg1110 conversion routine of some sort
   *
   * @param i - ?
   * @param rtim - ?
   * @param orbta - ?
   *
   * @return 3 by 3 double array
   *
   */

  private double [][] mg1110 (
    int i,
    double rtim,
    double [][] orbta
  )

  {

    double [][] npa = new double[3][3];
    double delt;

    if (i != 7) {

      delt = (rtim - orbta[0][i]) / (orbta[0][i+1] - orbta[0][i]);
      sat[0] = orbta[ 8][i] + (orbta[ 8][i+1] - orbta[ 8][i]) * delt;
      sat[1] = orbta[ 9][i] + (orbta[ 9][i+1] - orbta[ 9][i]) * delt;
      sat[2] = orbta[10][i] + (orbta[10][i+1] - orbta[10][i]) * delt;

      sitagt = (orbta[14][i] + (orbta[14][i+1] - orbta[14][i]) * delt) * cdr;
      if ((orbta[14][i+1] - orbta[14][i]) < 0.0d) {
        sitagt = (orbta[14][i] + 
          (orbta[14][i+1] - orbta[14][i] + 360.0d) * delt) * cdr;
      }
      sunalp = (orbta[17][i] + (orbta[17][i+1] - orbta[17][i]) * delt) * cdr;
      if ((orbta[17][i+1] - orbta[17][i]) > 0.0d) {
        sunalp = (orbta[17][i] + 
          (orbta[17][i+1] - orbta[17][i] - 360.0d) * delt) * cdr;
      }
      sundel = (orbta[18][i] + (orbta[18][i+1] - orbta[18][i]) * delt) * cdr;
 
      npa[0][0] = orbta[19][i];
      npa[1][0] = orbta[20][i];
      npa[2][0] = orbta[21][i];
      npa[0][1] = orbta[22][i];
      npa[1][1] = orbta[23][i];
      npa[2][1] = orbta[24][i];
      npa[0][2] = orbta[25][i];
      npa[1][2] = orbta[26][i];
      npa[2][2] = orbta[27][i];

    }

    return(npa);

  }


  /**
   *
   * mg1200 conversion routine of some sort
   *
   * @param vec - ?
   *
   * @return double array size 3
   *
   */

  private double[] mg1200 (
    double [] vec
  ) 

  {

    double [] vecu = new double[3];
    double rv1;
    double rv2;

    rv1 = (vec[0] * vec[0]) + (vec[1] * vec[1]) + (vec[2] * vec[2]);

    if (rv1 == 0.0d) {
      return(vecu);
    }

    rv2 = Math.sqrt(rv1);
    vecu[0] = vec[0] / rv2;
    vecu[1] = vec[1] / rv2;
    vecu[2] = vec[2] / rv2;

    return(vecu);

  }


  /**
   *
   * mg1210 conversion routine of some sort
   *
   * @param va - ?
   * @param vb - ?
   *
   * @return double array size 3
   *
   */

  private double [] mg1210 (
    double [] va,
    double [] vb
  ) 

  {

    double [] vc = new double[3];

    vc[0] = (va[1] * vb[2]) - (va[2] * vb[1]);
    vc[1] = (va[2] * vb[0]) - (va[0] * vb[2]);
    vc[2] = (va[0] * vb[1]) - (va[1] * vb[0]);

    return(vc);

  }


  /**
   *
   * mg1220 conversion routine of some sort
   *
   * @param va - ?
   * @param vb - ?
   *
   * @return double array size 3
   *
   */

  private double [] mg1220 (
    double [] va,
    double [] vb
  ) 

  {

    double [] vc = new double[3];
    double [] vd = new double[3];

    vc[0] = (va[1] * vb[2]) - (va[2] * vb[1]);
    vc[1] = (va[2] * vb[0]) - (va[0] * vb[2]);
    vc[2] = (va[0] * vb[1]) - (va[1] * vb[0]);

    vd = mg1200(vc);

    return(vd);

  }


  /**
   *
   * mg1230 conversion routine of some sort
   *
   * @param va - ?
   * @param vb - ?
   *
   * @return double 
   *
   */

  private double mg1230 (
    double [] va,
    double [] vb
  ) 

  {

    double as1;
    double as2;
    double asita = 0.0d;

    as1 = (va[0] * vb[0]) + (va[1] * vb[1]) + (va[2] * vb[2]);
    as2 = (((va[0] * va[0]) + (va[1] * va[1]) + (va[2] * va[2])) *
           ((vb[0] * vb[0]) + (vb[1] * vb[1]) + (vb[2] * vb[2])));

    if (as2 == 0.0d) {
      return(asita);
    }
    //asita = Math.acos(Math.min(Math.max(as1,.0000001),1.0) / Math.sqrt(as2));
    double temp = as1/Math.sqrt(as2);
    if (temp > 1.0 && temp-1.0 < 0.000001) temp = 1;
    asita = Math.acos(temp);

    return(asita);

  }


  /**
   *
   * mg1240 conversion routine of some sort
   *
   * @param va - ?
   * @param vh - ?
   * @param vn - ?
   * @param dpai - double PI (Math.PI * 2)
   *
   * @return double
   *
   */

  private double mg1240 (
    double [] va,
    double [] vh,
    double [] vn,
    double dpai
  ) 

  {

    double azi;
    double dnai;
    double [] vb = new double[3];
    double [] vc = new double[3];
    double [] vd = new double[3];

    vb = mg1220(vn, vh);
    vc = mg1220(va, vh);
    azi = mg1230(vb, vc);
    vd = mg1220(vb, vc);

    dnai = (vd[0] * vh[0]) + (vd[1] * vh[1]) + (vd[2] * vh[2]);
    if (dnai > 0.0d) {
      azi = dpai - azi;
    }

    return(azi);

  }


  /**
   * intToBytes converts an int to an array of 4 bytes.
   *
   * @param  v       input value
   *
   * @return         the corresponding array of bytes
   *
   */

  static public byte[] intToBytes(int v) {

    byte[] b = new byte[4];
    int allbits = 255;

    for(int i = 0; i < 4; i++){
      b[3-i] = (byte)((v & (allbits << i * 8) ) >> i *8);
    }

    return b;

  }

  // main is used for unit testing

  public static void main(String[] args) {

    int [] navBlock = new int[800];
    int [] dirBlock = new int[64];
    DataInputStream dis = null;
    GMSXnav gmsx = null;
    String fileName = "GMSXAREA";

    System.out.println("unit test of class GMSXnav begin...");

    if (args.length > 0) fileName = args[0];

    // test assumes presence of test area called GMSXAREA
    try {
      dis = new DataInputStream (
        new BufferedInputStream(new FileInputStream(fileName), 2048)
      );
    } catch (Exception e) {
      System.out.println("error creating DataInputStream: " + e);
      System.exit(0);
    }

    // read and discard the directory
    System.out.println("reading in, discarding directory words...");
    try {
      for (int i = 0; i < 64; i++) {
        dirBlock[i] = dis.readInt();
      }
    } catch (IOException e) {
      System.out.println("error reading area file directory: " + e);
      System.exit(0);
    }

    // now read in the navigation data
    System.out.println("reading in navigation words...");
    try {
      for (int i = 0; i < navBlock.length; i++) {
        navBlock[i] = dis.readInt();
      }
    } catch (IOException e) {
      System.out.println("error reading area file navigation data: " + e);
      System.exit(0);
    }

    if (navBlock[0] != GMSX) {
      System.out.println("error: not a GMS navigation block.");
      System.exit(0);
    } 

    System.out.println("creating GMSXnav object...");
    gmsx = new GMSXnav(navBlock);
    gmsx.setImageStart(dirBlock[5], dirBlock[6]);
    System.out.println("####  ImageStart set to:"+dirBlock[5]+" "+dirBlock[6]);
    gmsx.setRes(dirBlock[11], dirBlock[12]);
    System.out.println("####  ImageRes set to:"+dirBlock[11]+" "+dirBlock[12]);
    gmsx.setStart(1,1);
    gmsx.setFlipLineCoordinates(dirBlock[8]); // invert Y axis coordinates

    System.out.println(" test of toLatLon...");
    System.out.println("  answer should be close to: -2.37, 133.31");

    double [][] linEle = new double [2][1];
    double [][] latLon = new double [2][1];
    linEle[gmsx.indexLine][0] = 471.0f;
    linEle[gmsx.indexEle][0] = 323.0f;

    latLon = gmsx.toLatLon(linEle);
    System.out.println("  answer is: " + latLon[gmsx.indexLat][0] + 
      ", " + latLon[gmsx.indexLon][0]);

    System.out.println(" test of toLinEle...");

    System.out.println("  answer should be close to: 480.0, 1.0");
    latLon[gmsx.indexLat][0] = -2.0f;
    latLon[gmsx.indexLon][0] = 118.0f;
    linEle = gmsx.toLinEle(latLon);
    System.out.println("  answer is: " + linEle[gmsx.indexLine][0] + 
      ", " + linEle[gmsx.indexEle][0]);

    System.out.println("  answer should be close to: 16.0, 628.0");
    latLon[gmsx.indexLat][0] = -24.0f;
    latLon[gmsx.indexLon][0] = 148.0f;
    linEle = gmsx.toLinEle(latLon);
    System.out.println("  answer is: " + linEle[gmsx.indexLine][0] + 
      ", " + linEle[gmsx.indexEle][0]);

    System.out.println("unit test of class GMSXnav end...");

  }
}
