//
// KALPnav.java
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

import java.util.*;
import java.io.*;

/**
 * The KALPnav class creates the ability to navigate KALP
 * image data.  It is a math copy of the McIDAS nvxgvar.dlm
 * code.
 *
 * When used with AreaFile class, set up like this:
 *
 * <pre><code>
 *  AreaFile af;
 *  try {
 *    af = new AreaFile("/home/user/mcidas/data/AREA0001");
 *  } catch (AreaFileException e) {
 *    System.out.println(e);
 *    return;
 *  }
 *  int[] dir;
 *  try { dir=af.getDir();
 *  } catch (AreaFileException e){
 *    System.out.println(e);
 *    return;
 *  }
 *  int[] nav;
 *  try { nav=af.getNav();
 *  } catch (AreaFileException e){
 *    System.out.println(e);
 *    return;
 *  }
 *  try { 
 *    GVARnav ng = new GVARnav(nav);  // XXXXnav is the specific implementation
 *  } catch (IllegalArgumentException excp) {
 *    System.out.println(excp);
 *    return;
 *  }
 *  ng.setImageStart(dir[5], dir[6]);
 *  ng.setRes(dir[11], dir[12]);
 *  ng.setMag(1,1);
 *  ng.setStart(0,0);
 *  ......................
 * </code></pre>
 *
 * @author Tom Whittaker
 * 
 */
public class KALPnav extends AREAnav {
  private int ic;
  private double h, a, rp, re, cdr, crd, lpsi2, deltax, deltay;
  private double sublat, sublon, cenlin, cenele, altitude;
  private double PI;

  private boolean isEastPositive = true;

  public KALPnav (int[] iparms) {
     this(1, iparms);
  }

  public KALPnav (int ifunc, int[] iparms) {

    if (ifunc != 1) ifunc = 1;

    if (iparms[0] != KALP ) {
        throw new IllegalArgumentException("Invalid navigation type" + iparms[0]);
    }
 
//        H=42150.766-6378.155
    altitude = (double)iparms[11] / 10000.0;
    h=altitude-6378.155;

    re=6378.155;
    a=1./297.;
    rp=re/(1.+a);
    PI=3.141592653;
    cdr=PI/180.;
    crd=180./PI;
    lpsi2=1.0;

//        DELTAX=18.03674/1408.
//        DELTAY=18.03674/1408.
    deltax = (double)iparms[12] / 1000000000.0;
    deltax = deltax * crd;
    deltay = deltax;

    sublat=(double)iparms[10]/10000.;
    sublon=(double)iparms[6]/10000.;

//        The center line and element are in full res coords *10
//        They are used in scan coords, so divide by 40.

    cenlin = (double)iparms[13]/40.;
    cenele = (double)iparms[14]/40.;
    if(iparms[13] == 0) {
       cenlin = 704.5;
       cenele = 704.5;
    }
  }

 
  public float[][] toLatLon(float[][] linele) { 

    double xele2, xlin2, x, y, xr, yr, rs, tanx, tany, val1, val2, yk;
    double vmu, cosrf, sinrf, xt, yt, zt, xfi, xla, teta;

    int number = linele[0].length;
    float[][] latlon = new float[2][number];
    float[][] imglinele = areaCoordToImageCoord(linele);

    for (int point=0; point<number; point++) {

      xele2 = imglinele[indexEle][point]/4.0;
      xlin2 = imglinele[indexLine][point]/4.0;
      x = cenele - xele2;
      y = cenlin - xlin2;
      xr = x;
      yr = y;

      x=xr*lpsi2*deltax*cdr;
      y=yr*lpsi2*deltay*cdr;
      rs=re+h;
      tanx=Math.tan(x);
      tany=Math.tan(y);
      val1=1.+tanx*tanx;
      val2=1.+(tany*tany)*((1.+a)*(1.+a));
      yk=rs/re;

      if((val1*val2) > ((yk*yk)/(yk*yk-1.0))) {
        latlon[indexLine][point] = Float.NaN;
        latlon[indexEle][point] = Float.NaN;
        continue;
      }

      vmu=(rs-(re*(Math.sqrt((yk*yk)-(yk*yk-1)*val1*val2))))/(val1*val2);
      cosrf=Math.cos(sublat*cdr);
      sinrf=Math.sin(sublat*cdr);
      xt=(rs*cosrf)+(vmu*(tanx*sinrf-cosrf));
      yt=(rs*sinrf)-(vmu*(tanx*cosrf+sinrf));
      zt=vmu*tany/Math.cos(x);
      teta=Math.asin(zt/rp);
      xfi=(Math.atan(((Math.tan(teta))*re)/rp))*crd;
      xla=-Math.atan(yt/xt)*crd;
//--- CHANGE LONGITUDE FOR CORRECT SUBPOINT
      xla=xla+sublon;
      if (isEastPositive) xla = -xla;
      latlon[indexLat][point] = (float) xfi;
      latlon[indexLon][point] = (float) xla;
    }
    return latlon;
  }

  public float[][] toLinEle(float [][] latlon) {


    double x1, y1, xfi, xla, rom, y, r1, r2, rs, reph, rpph;
    double coslo, sinlo, teta, xt, yt, zt, px, py, xr, yr ;

    int number = latlon[0].length;
    float[][] linele = new float[2][number];

    for (int point=0; point<number; point++) {

      x1 = (double) latlon[indexLat][point];
      y1 = (double) latlon[indexLon][point];
      if (!isEastPositive) y1 = -y1;

//--- CORRECT FOR SUBLON
      y1=y1+sublon;
      xfi=x1*cdr;
      xla=y1*cdr;
      rom=(re*rp)/Math.sqrt(rp*rp*Math.cos(xfi) *
            Math.cos(xfi)+re*re*Math.sin(xfi)*Math.sin(xfi));
      y=Math.sqrt(h*h+rom*rom-2*h*rom*Math.cos(xfi)*Math.cos(xla));
      r1=y*y+rom*rom;
      r2=h*h;
      if (r1 > r2) {
        linele[indexLine][point] = Float.NaN;
        linele[indexEle][point] = Float.NaN;

      } else {
        rs=re+h;
        reph=re;
        rpph=rp;
        coslo=Math.cos(sublat*cdr);
        sinlo=Math.sin(sublat*cdr);
        teta=Math.atan((rpph/reph)*Math.tan(xfi));
        xt=reph*Math.cos(teta)*Math.cos(xla);
        yt=reph*Math.cos(teta)*Math.sin(xla);
        zt=rpph*Math.sin(teta);
        px=Math.atan((coslo*(yt-rs*sinlo)-(xt-rs*coslo)*sinlo)/
         (sinlo*(yt-rs*sinlo)+(xt-rs*coslo)*coslo));
        py=Math.atan(zt*((Math.tan(px)*sinlo-coslo)/(xt-rs*coslo))*Math.cos(px));
        px=px*crd;
        py=py*crd;
        xr=px/(deltax*lpsi2);
        yr=py/(deltay*lpsi2);
        xr=cenele-xr;
        yr=cenlin-yr;
        xr=xr*4.0;
        yr=yr*4.0;
        linele[indexLine][point] = (float)yr;
        linele[indexEle][point] = (float)xr;
      }
    }
    return imageCoordToAreaCoord(linele, linele);
  }


  public double[][] toLinEle(double [][] latlon) {


    double x1, y1, xfi, xla, rom, y, r1, r2, rs, reph, rpph;
    double coslo, sinlo, teta, xt, yt, zt, px, py, xr, yr;

    int number = latlon[0].length;
    double[][] linele = new double[2][number];

    for (int point=0; point<number; point++) {

      x1 = latlon[indexLat][point];
      y1 = latlon[indexLon][point];
      if (!isEastPositive) y1 = -y1;

//--- CORRECT FOR SUBLON
      y1=y1+sublon;
      xfi=x1*cdr;
      xla=y1*cdr;
      rom=(re*rp)/Math.sqrt(rp*rp*Math.cos(xfi) *
            Math.cos(xfi)+re*re*Math.sin(xfi)*Math.sin(xfi));
      y=Math.sqrt(h*h+rom*rom-2*h*rom*Math.cos(xfi)*Math.cos(xla));
      r1=y*y+rom*rom;
      r2=h*h;
      if (r1 > r2) {
        linele[indexLine][point] = Float.NaN;
        linele[indexEle][point] = Float.NaN;
        continue;

      }
      rs=re+h;
      reph=re;
      rpph=rp;
      coslo=Math.cos(sublat*cdr);
      sinlo=Math.sin(sublat*cdr);
      teta=Math.atan((rpph/reph)*Math.tan(xfi));
      xt=reph*Math.cos(teta)*Math.cos(xla);
      yt=reph*Math.cos(teta)*Math.sin(xla);
      zt=rpph*Math.sin(teta);
      px=Math.atan((coslo*(yt-rs*sinlo)-(xt-rs*coslo)*sinlo)/
       (sinlo*(yt-rs*sinlo)+(xt-rs*coslo)*coslo));
      py=Math.atan(zt*((Math.tan(px)*sinlo-coslo)/(xt-rs*coslo))*Math.cos(px));
      px=px*crd;
      py=py*crd;
      xr=px/(deltax*lpsi2);
      yr=py/(deltay*lpsi2);
      xr=cenele-xr;
      yr=cenlin-yr;
      xr=xr*4.0;
      yr=yr*4.0;
      linele[indexLine][point] = yr;
      linele[indexEle][point] = xr;
    }
    return imageCoordToAreaCoord(linele, linele);
  }

  public double[][] toLatLon(double[][] linele) { 

    double xele2, xlin2, x, y, xr, yr, rs, tanx, tany, val1, val2, yk;
    double vmu, cosrf, sinrf, xt, yt, zt, xfi, xla, teta;

    int number = linele[0].length;
    double[][] latlon = new double[2][number];
    double[][] imglinele = areaCoordToImageCoord(linele);

    for (int point=0; point<number; point++) {

      xele2 = imglinele[indexEle][point]/4.0;
      xlin2 = imglinele[indexLine][point]/4.0;
      x = cenele - xele2;
      y = cenlin - xlin2;
      xr = x;
      yr = y;

      x=xr*lpsi2*deltax*cdr;
      y=yr*lpsi2*deltay*cdr;
      rs=re+h;
      tanx=Math.tan(x);
      tany=Math.tan(y);
      val1=1.+tanx*tanx;
      val2=1.+(tany*tany)*((1.+a)*(1.+a));
      yk=rs/re;

      if((val1*val2) > ((yk*yk)/(yk*yk-1.0))) {
        latlon[indexLine][point] = Float.NaN;
        latlon[indexEle][point] = Float.NaN;
        continue;
      }

      vmu=(rs-(re*(Math.sqrt((yk*yk)-(yk*yk-1)*val1*val2))))/(val1*val2);
      cosrf=Math.cos(sublat*cdr);
      sinrf=Math.sin(sublat*cdr);
      xt=(rs*cosrf)+(vmu*(tanx*sinrf-cosrf));
      yt=(rs*sinrf)-(vmu*(tanx*cosrf+sinrf));
      zt=vmu*tany/Math.cos(x);
      teta=Math.asin(zt/rp);
      xfi=(Math.atan(((Math.tan(teta))*re)/rp))*crd;
      xla=-Math.atan(yt/xt)*crd;
//--- CHANGE LONGITUDE FOR CORRECT SUBPOINT
      xla=xla+sublon;

      if (isEastPositive) xla = -xla;
      latlon[indexLat][point] = xfi;
      latlon[indexLon][point] = xla;
    }

    return latlon;
  }

  public static void main(String[] args) {

    int [] navBlock = new int[800];
    int [] dirBlock = new int[64];
    DataInputStream dis = null;
    KALPnav kalp = null;
    String fileName = "KALPAREA";

    System.out.println("unit test of class KALP begin...");

    if (args.length > 0) fileName = args[0];

    // test assumes presence of test area called KALPXAREA
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

    if (navBlock[0] != KALP) {
      System.out.println("error: not a KALP navigation block.");
      System.exit(0);
    } 

    double [][] linEle = new double [2][1];
    double [][] latLon = new double [2][1];

    System.out.println("creating KALPnav object...");
    kalp = new KALPnav(navBlock);
    kalp.setImageStart(dirBlock[5], dirBlock[6]);
    System.out.println("####  ImageStart set to:"+dirBlock[5]+" "+dirBlock[6]);
    kalp.setRes(dirBlock[11], dirBlock[12]);
    System.out.println("####  ImageRes set to:"+dirBlock[11]+" "+dirBlock[12]);
    kalp.setStart(0,0);

    //System.out.println("####  setFlipLineCoord called with "+dirBlock[8]);
    //kalp.setFlipLineCoordinates(dirBlock[8]); // invert Y axis coordinates

    System.out.println(" test of toLinEle...");

    latLon[kalp.indexLat][0] = 11.0f;
    latLon[kalp.indexLon][0] = 50.f;
    linEle = kalp.toLinEle(latLon);
    System.out.println("  answer is: " + linEle[kalp.indexLine][0] + 
      ", " + linEle[kalp.indexEle][0]);

    latLon[0][0] = Double.NaN;
    latLon = kalp.toLatLon(linEle);
    System.out.println("testing inverse of 11N/50E");
    System.out.println("  answer is: " + latLon[kalp.indexLat][0] + 
      ", " + latLon[kalp.indexLon][0]);

    System.out.println(" another test of toLinEle...");

    latLon[kalp.indexLat][0] = -20.0f;
    latLon[kalp.indexLon][0] = 100.f;
    linEle = kalp.toLinEle(latLon);
    System.out.println("  answer is: " + linEle[kalp.indexLine][0] + 
      ", " + linEle[kalp.indexEle][0]);

    latLon[0][0] = Double.NaN;
    latLon = kalp.toLatLon(linEle);
    System.out.println("testing inverse of 20S/100E");
    System.out.println("  answer is: " + latLon[kalp.indexLat][0] + 
      ", " + latLon[kalp.indexLon][0]);

    System.out.println("unit test of class KALPnav end...");

  }

}
