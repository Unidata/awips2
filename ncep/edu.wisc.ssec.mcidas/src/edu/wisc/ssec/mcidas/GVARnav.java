//
// GVARnav.java
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

/**
 * The GVARnav class creates the ability to navigate GVAR
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
 *  ng.setStart(1,1);
 *  ......................
 * </code></pre>
 *
 * @author Tom Whittaker
 * 
 */
public class GVARnav extends AREAnav
{

  private boolean isEastPositive = true;

  final double PI=3.141592653589793d;
  final double DEG=180.d/PI;
  final double RAD=PI/180.d; // degrees to radians conversion pi/180
  final double NOMORB=42164.365d; // nominal radial distance of satellite (km)
  final double AE=6378.137d; // earth equatorial radius (km)
  final double FER=1.d-(6356.7533d/AE); // earth flattening coeff = 1-(be/ae)
  final float AEBE2= (float) ( 1.d/Math.pow(1.d-FER,2d));
  final float AEBE3= AEBE2-1.f;
  final float AEBE4=(float) Math.pow(1.d-FER,4d)-1.f;

  private double xs[] = new double[3]; 
     // normalized s/c position in ecef coordinates
  private double bt[][] = new double[3][3]; 
     // ecef to instrument coordinates transformation
  private double q3; // used in subrtn lpoint
  private double pitch,roll,yaw; 
     // pitch,roll,yaw angles of instrument (rad)
  private float pma; // pitch misalignment of instrument (rad)
  private float rma; // roll misalignment of instrument (rad)

  private int incmax[] = {6136, 2805}; // number of increments per cycle

  private float elvmax[] = {0.220896f, 0.22089375f}; 
      // bounds in elevation (radians)
  private float scnmax[] = {0.24544f,0.2454375f}; 
      // bounds in scan angle (radians)
  private float elvinc[] = {8.e-6f, 17.5e-6f}; 
      // change in elevation angle per increment (rad)
  private float scninc[] = {16.e-6f, 35.e-6f}; 
      // change in scan angle per increment (radians)
  private float elvln[] = {28.e-6f, 280.e-6f}; 
      // elevation angle per detector line (radians)
  private float scnpx[] = {16.e-6f, 280.e-6f}; 
      // scan angle per pixel (radians)
  private float nsnom[] = {.220896f, .22089375f};
      // north-south center of instrument (=4.5 x incmax x elvinc

  private float ewnom[] = {.24544f, .2454375f};
      // east-west center of instrument (=2.5 x incmax x sncinc)

  final int STTYPE = 0; // position of satellite type
  final int IDNTFR = 1;
  final int IMCACT = 2; // position of imc active flag
  final int IYFLIP = 3; // position of yaw flip enabled flag
  final int REFLON = 5; // position of reference longitude
  final int REFDIS = 6; // position of reference distance from nominal
  final int REFLAT = 7; // position of reference latitude
  final int REFYAW = 8; // position of reference yaw
  final int RATROL = 9; // position of reference attitude roll
  final int RATPTC = 10; // position of reference attitude pitch
  final int RATYAW = 11; // position of reference attitude yaw
  final int ETIME  = 12; // position of epoch time
  final int EDTIME = 14; // location of delta from epoch time
  final int IMCROL = 15; // location of image motion compensation roll
  final int IMCPTC = 16; // location of image motion compensation pitch
  final int IMCYAW = 17; // location of image motion compensation yaw

  // ldr1-13: location of longitude delta from reference parameters
  final int LDR1   = 18; 
  final int LDR2   = 19;
  final int LDR3   = 20;
  final int LDR4   = 21;
  final int LDR5   = 22;
  final int LDR6   = 23;
  final int LDR7   = 24;
  final int LDR8   = 25;
  final int LDR9   = 26;
  final int LDR10  = 27;
  final int LDR11  = 28;
  final int LDR12  = 29;
  final int LDR13  = 30;

  // rddr1-11: location of radial distance delta from reference parameters
  final int RDDR1  = 31; 
  final int RDDR2  = 32;
  final int RDDR3  = 33;
  final int RDDR4  = 34;
  final int RDDR5  = 35;
  final int RDDR6  = 36;
  final int RDDR7  = 37;
  final int RDDR8  = 38;
  final int RDDR9  = 39;
  final int RDDR10 = 40;
  final int RDDR11 = 41;

  // dgl1-9: location of geocentric latitude delta parameters
  final int DGL1   = 42; 
  final int DGL2   = 43;
  final int DGL3   = 44;
  final int DGL4   = 45;
  final int DGL5   = 46;
  final int DGL6   = 47;
  final int DGL7   = 48;
  final int DGL8   = 49;
  final int DGL9   = 50;

  // doy1-9: location of orbit yaw delta parameters
  final int DOY1   = 51; 
  final int DOY2   = 52;
  final int DOY3   = 53;
  final int DOY4   = 54;
  final int DOY5   = 55;
  final int DOY6   = 56;
  final int DOY7   = 57;
  final int DOY8   = 58;
  final int DOY9   = 59;

  final int EXPTIM = 61; // exponential start time from epoch
  final int RAAWDS = 62; // location of start of roll attitude angle words
  final int PAAWDS = 129; // location of start of pitch attitude angle words
  final int YAAWDS = 184; // location of start of yaw attitude angle words
  final int RMAWDS = 257; // location of start of roll misalignment angle words
  final int PMAWDS = 312; // location of start of pitch misalignment angle words

  final int IMGDAY = 367; // position of image day value  (yyddd)
  final int IMGTM  = 368; // position of image time value (hhmmss)
  final int IMGSND = 369; // location of imager/sounder instrument flag

// the following four words were added 5-26-94 to comply w/ the new elug...
// numbering started at 380 because these same parameters are used
// in the nav message sent from the ingestor to evx, and we had to
// start somewhere after the 378 nav parameters

  final int IOFNC = 379;
  final int IOFEC = 380;
  final int IOFNI = 381;
  final int IOFEI = 382;

  final int MXCDSZ=5*128; // maximum directory block entry size

  final int OASIZE = 336; // size of gvar orbit and attitude set
  final int PCOEFS = 117; // start of pitch coefficients
  final int RMACFS = 227; // start of rma coefficients
  final int CUTOF1 = 115; // first dividing point in o&a set (128 word sets)
  final int CUTOF2 = 225; // second dividing point in o&a set (128 word sets)

  final int IMCFLG = 7; //  bit # in sscan for imc active flag
  final int FLPFLG = 15; //  bit # in sscan for yaw flip enabled flat
  private int iflip; // value of FLPFLG

  // now some variables that are shared among the methods...

  private double aec, ts, dr, lam, dlat, dyaw, phi;
  private double aebe2c, aebe3c, aebe4c, ferc;
  private int instr, itype;
  private double sublat, sublon;
  //private double rlat, rlon, gam, alf;
  private double[] subpoint;

  final int RELLST [][] = {  { 4,  10}, { 13,  63}, { 65,  94},
                     { 98, 100}, {103, 105}, {108, 110}, {113, 115},
                     {116, 118}, {120, 149}, {153, 155}, {158, 160},
                     {163, 165}, {168, 170}, {171, 173}, {175, 204},
                     {208, 210}, {213, 215}, {218, 220}, {223, 225},
                     {226, 228}, {230, 259}, {263, 265}, {268, 270},
                     {273, 275}, {278, 283}, {285, 314}, {318, 320},
                     {323, 325}, {328, 330}, {333, 335}, { -1,  -1}};



  /**
   * Set up for the real math work.  Must pass in the int array
   * of the GVAR nav 'codicil'.
   *
   * @param iparms the nav block from the image file
   * @throws IllegalArgumentException
   *           if the nav block is not a GVAR type.
   */
  public GVARnav (int[] iparms) 
      throws IllegalArgumentException
  {
    this(1, iparms);
  }

  /**
   * Set up for the real math work.  Must pass in the int array
   * of the GVAR nav 'codicil'.
   * @deprecated  Since ifunc must be 1, replaced with #GVARnav(int[] iparms).
   *              If ifunc != 1, ifunc is set to 1.
   *
   * @param  ifunc   the function to do (always 1 for now)
   * @param  iparms   the nav block from the image file
   * @throws IllegalArgumentException
   *           if the nav block is not a GVAR type.
   */
  public GVARnav (int ifunc, int[] iparms) 
      throws IllegalArgumentException
  {

    double te, psi, u, sinu, cosu;
    double sinoi, cosoi, slat, asc, sinasc, cosasc;
    double syaw, w, sw, cw, s2w, c2w;
    double sw1, cw1, sw3, cw3;
    double  secs, wa;
    double imgtim, epoch, timex, time50, r;
    double[][] b = new double [3][3];
    int lit,  year, day;
    int iftok, hour, minuts;
    int count, offset, loop;
    int time[] = new int[2];
    float rparms[] = new float[MXCDSZ];


// rellst two dimensional array of location of float in o&a set

// initialize the earth radius constants
    aec    = AE;
    ferc   = FER;
    aebe2c = (double) AEBE2;
    aebe3c = (double) AEBE3;
    aebe4c = (double) AEBE4;

    // Only type 1 supported
    if (ifunc != 1) ifunc = 1;

    // This is not used.  Left over from nvxgvar.dlm code for Cartesian
    // transformations
    if (ifunc != 1) {
      if (iparms[0] == LL ) itype = 1;
      if (iparms[0] == XY ) itype = 2;
      return ;
    }

    if (iparms[STTYPE] != GVAR ) 
        throw new IllegalArgumentException("Invalid navigation type" + 
                                            iparms[STTYPE]);

    itype = 1;

    // copy codicil into a float array
    for (loop = 0; loop<MXCDSZ; loop++) {
      rparms[loop] = Float.intBitsToFloat(iparms[loop] );
    }

    count = 0;
    rparms[IMGTM] = ( (float)iparms[IMGTM])/1000.f;
    while (RELLST[count][0] != -1) {

      offset = 1;
      if (RELLST[count][0] > CUTOF1) offset = 13;
      if (RELLST[count][0] > CUTOF2) offset = 31;

      for (loop = RELLST[count][0]; loop <= RELLST[count][1]; loop++) {
    if ( (loop == 13) || (loop == 60) || ( ((loop-7) % 55) == 0) 
           && (loop != 7) ) {
          rparms[loop+offset] = ( (float)iparms[loop+offset])/100.f;
        } else {
          rparms[loop+offset] = ( (float)iparms[loop+offset])/10000000.f;
        }
      }

      count ++;
    } // end while

    //see if this codicil is for imager or sounder
    instr = iparms[IMGSND];

    // ----------------------------------------------------------
    // new code from kath kelly for sounder nav - 10/27/??
    // because change to elug -- nadir postion is avabile in
    // signal and should be used to compute values instead
    // of making them hardwired...
     
    int nadnsc, nadnsi, nadewc, nadewi;
    nadnsc = iparms[IOFNC];
    nadnsi = iparms[IOFNI];
    nadewc = iparms[IOFEC];
    nadewi = iparms[IOFEI];

    if (nadnsc!=0 && nadnsi!=0 && nadewc!=0 && nadewi!=0) {
      if (instr == 1) {
        elvmax[0] = (nadnsc*incmax[0]+nadnsi)*elvinc[0];
      } else {
        elvmax[1]=( (9-nadnsc)*incmax[1]-nadnsi)*elvinc[1];
      }

      scnmax[instr-1] = (nadewc*incmax[instr-1]+nadewi)*scninc[instr-1];
    }
    // end of new code from kathy kelly
    // ----------------------------------------------------------

    // get contorl info from codicil

    year    = 1900 + iparms[IMGDAY] / 1000;
    day     = iparms[IMGDAY] - iparms[IMGDAY] / 1000 * 1000;
    hour    = (int)rparms[IMGTM] / 10000;
    minuts  = (int)rparms[IMGTM] / 100 - hour * 100;
    secs    = rparms[IMGTM] - (float)100*minuts - (float)10000*hour;

    // compute the actual time in minutes from Jan 1, 1950:

    int j = day + 1461*(year+4799)/4 - 3 * ((year + 4899) / 100) / 4 - 2465022;
    imgtim = (double)j * 1440.d + (double)hour*60.d + (double)minuts + (secs/60.d);

    // convert the BCD to binary integer

    int t0, t1, e0, e1, power10;
    t0 = 0;
    t1 = 0;
    power10 = 1;
    e0 = iparms[ETIME];
    e1 = iparms[ETIME+1];
    for (int i=0; i<8; i++) {
      t0 = t0 + ( e0 & 0xf ) * power10;
      t1 = t1 + ( e1 & 0xf ) * power10;
      e0 = e0 >>> 4;
      e1 = e1 >>> 4;
      power10 = power10 * 10;
    }

    int iaa, iab, iac, nbc, def;
    year = t0 / 10000;
    day = (int) ((t0 - (year * 10000)) * 0.1);
    iaa = t0 - (year * 10000);
    iab = (iaa - (day * 10)) * 10;
    nbc = t1 / 10000000;
    iac = t1 - (nbc * 10000000);
    def = t1 - iac;
    hour = iab + nbc;
    minuts = (int) (iac * 0.00001);
    double s = (t1 - (def + (minuts * 100000))) * 0.001;
    j = day + 1461*(year+4799)/4 - 3 * ((year + 4899) / 100) / 4 - 2465022;
    epoch = (double)j * 1440.d + (double)hour*60.d + (double)minuts + (s/60.d);

    int imc = 1;
    if ( (iparms[IMCACT] & (1 << IMCFLG)) != 0 ) imc = 0;
    iflip = 1;
    if ( (iparms[IYFLIP] & (1 << FLPFLG)) != 0) iflip = -1;

    // assign reference values to the subsatellite longitude and
    // latitude, the radial distance and the orbit yaw.
    lam = rparms[REFLON];
    dr  = rparms[REFDIS];
    phi = rparms[REFLAT];
    psi = rparms[REFYAW];

    subpoint = new double[2];
    subpoint[0] = rparms[REFLAT]/RAD;
    subpoint[1] = rparms[REFLON]/RAD;


    // assign reference values to the attitudes and misalignments
    roll  = rparms[RATROL];
    pitch = rparms[RATPTC];
    yaw   = rparms[RATYAW];
    rma   = 0.f;
    pma   = 0.f;

    // if imc is off, compute changes in the satellite orbit
    if (imc != 0) { 

    // set reference radial distance, latitude and orbit yaw to zero
      dr  = 0.;
      phi = 0.;
      psi = 0.;

    // compute time since epoch (in minutes)
      ts = imgtim - epoch;

    // computes orbit angle and the related trigonometric funktions.
    // earth rotational rate=.729115e-4 (RAD/s)
      w   = 0.729115e-4 * 60.0d * ts;
      sw  = Math.sin(w);
      cw  = Math.cos(w);
      sw1 = Math.sin(0.927*w);
      cw1 = Math.cos(0.927*w);
      s2w = Math.sin(2.*w);
      c2w = Math.cos(2.*w);
      sw3 = Math.sin(1.9268*w);
      cw3 = Math.cos(1.9268*w);

    // computes change in the imc longitude from the reference
      lam = lam + rparms[LDR1] + (rparms[LDR2] + rparms[LDR3]*w) * w
            + (rparms[LDR10]*sw1 + rparms[LDR11]*cw1 + rparms[LDR4]*sw
            + rparms[LDR5]*cw + rparms[LDR6]*s2w + rparms[LDR7]*c2w
            + rparms[LDR8]*sw3+rparms[LDR9]*cw3 + w*(rparms[LDR12]*sw
            + rparms[LDR13]*cw))*2.;

    // computes change in radial distance from the reference (km)
      dr = dr + rparms[RDDR1] + rparms[RDDR2]*cw + rparms[RDDR3]*sw
              + rparms[RDDR4]*c2w + rparms[RDDR5]*s2w + rparms[RDDR6]
              * cw3+rparms[RDDR7]*sw3 + rparms[RDDR8]*cw1
              + rparms[RDDR9]*sw1 + w*(rparms[RDDR10]*cw
              + rparms[RDDR11]*sw);

    // computes the sine of the change in the geocentric latitude
      dlat = rparms[DGL1] + rparms[DGL2]*cw + rparms[DGL3]*sw
              + rparms[DGL4]*c2w + rparms[DGL5]*s2w + w*(rparms[DGL6]*cw
              + rparms[DGL7]*sw) + rparms[DGL8]*cw1+rparms[DGL9]*sw1;

    // computes geocentric latitude by using an expansion for arcsine
      phi = phi + dlat * (1. + dlat * dlat / 6.);

    // computes sine of the change in the orbit yaw
      dyaw = rparms[DOY1] + rparms[DOY2]*sw + rparms[DOY3]*cw
               + rparms[DOY4]*s2w + rparms[DOY5]*c2w
               + w*(rparms[DOY6]*sw + rparms[DOY7]*cw)
               + rparms[DOY8]*sw1 + rparms[DOY9]*cw1;

    // computes the orbit yaw by using an expansion for arcsine.
      psi = psi + dyaw * (1. + dyaw * dyaw / 6.);

    }  // calculation of changes in the satellite orbit ends here


    // conversion of the imc longitude and orbit yaw to the subsatellite
    // longitude and the orbit inclination (ref: goes-pcc-tm-2473, inputs
    // required for earth location and gridding by sps, june 6, 1988)
    slat  = Math.sin(phi);
    syaw  = Math.sin(psi);
    sinoi = slat*slat + syaw*syaw;
    cosoi = Math.sqrt(1.-sinoi);
    sinoi = Math.sqrt(sinoi);

    if (slat == 0.0d && syaw == 0.0d) {
      u = 0.0d;
    } else {
      u = Math.atan2(slat,syaw);
    }

    sinu  = Math.sin(u);
    cosu  = Math.cos(u);

    // computes longitude of the ascending node
    asc    = lam-u;
    sinasc = Math.sin(asc);
    cosasc = Math.cos(asc);

    // computes the subsatellite geographic latitude
    sublat = Math.atan(aebe2c * Math.tan(phi));

    // computes the subsatellite longitude
    sublon = asc + Math.atan2(cosoi*sinu,cosu);

    // computes the spacecraft to earth fixed coordinates transformation
    // matrix:
    //     (vector in ecef coordinates) = b * (vector in s/c coordinates)

    b[0][1] = -sinasc*sinoi;
    b[1][1] =  cosasc*sinoi;
    b[2][1] = -cosoi;
    b[0][2] = -cosasc*cosu+sinasc*sinu*cosoi;
    b[1][2] = -sinasc*cosu-cosasc*sinu*cosoi;
    b[2][2] = -slat;
    b[0][0] = -cosasc*sinu-sinasc*cosu*cosoi;
    b[1][0] = -sinasc*sinu+cosasc*cosu*cosoi;
    b[2][0] =  cosu*sinoi;

    // computes the normalized spacecraft position vector in earth fixed
    // coordinates - xs.
    r     = (NOMORB+dr)/aec;
    xs[0] = -b[0][2]*r;
    xs[1] = -b[1][2]*r;
    xs[2] = -b[2][2]*r;

    // precomputes q3 (used in lpoint funciton (now in navToLatLon() )

    q3 = xs[0]*xs[0] + xs[1]*xs[1] + aebe2c * xs[2]*xs[2] - 1.0;

    // computes the attitudes and misalignments if imc is off
    if (imc != 0) {

    // computes the solar orbit angle
      wa = rparms[61-1] * ts;

    // computes the difference between current time, ts, and the
    // exponential time, iparms(62). note that both times are since epoch.
      te = ts - rparms[EXPTIM];

    // computes roll + roll misalignment
      roll = roll + gatt(RAAWDS,rparms,iparms,wa,te);

    // computes pitch + pitch misalignment
      pitch = pitch + gatt(PAAWDS,rparms,iparms,wa,te);

    // computes yaw
      yaw = yaw + gatt(YAAWDS,rparms,iparms,wa,te);

    // computes roll misalignment
      rma = (float) gatt(RMAWDS,rparms,iparms,wa,te);

    // computes pitch misalignment
      pma = (float) gatt(PMAWDS,rparms,iparms,wa,te);

    // apply the earth sensor compensation if needed
      roll   = roll + rparms[IMCROL];
      pitch  = pitch + rparms[IMCPTC];
      yaw    = yaw + rparms[IMCYAW];

    }  // end if (imc...)

    // computes the instrument to earth fixed coordinates transformation
    // matrix - bt
    inst2e(roll,pitch,yaw,b,bt);

    return; 

  }

  private double gatt
          (int k,float rparms[],int iparms[], double wa, double te) {

    // k =  starting position of a parameter subset in the real o&a set
    // rparms(mxcdxz) = input o&a parameter set
    // iparms(mxcdxz) = input o&a parameter set
    // doulble wa = input solar orbit angle in radians
    // doulbe te = input exponential time delay from epoch (minutes)

    // local variables

    double ir, jr, mr, att;

    // constant component
    att = rparms[k+2];

    //  computes the exponential term
    if (te >= 0) {
        att = att + rparms[k] * Math.exp(-te / rparms[k+1]);
    }

    // extracts the number of sinusoids
    ir = (double) iparms[k+3];
    int i  = (int) ir;

    // calculation of sinusoids
    for (int loop = 1; loop<=i; loop++) {
      att =att + rparms[k+2*loop+2] * Math.cos(wa*(double)loop +
                 rparms[k+2*loop+3]);
    }

    // pointer to the number of monomial sinusoids
    k = k + 34;

    // extacts number of monomial sinusoids
    ir  = (double) iparms[k];
    int kkk = iparms[k];
    int ll;

    // computes monomial sinusoids
    for (int l=1; l<=kkk; l++) {
        
      ll = k + 5 * l;

      // order of sinusoid
      jr =  (double) iparms[ll-4];

      // order of monomial sinusoid
      mr  = (double) iparms[ll-3];
      att = att + rparms[ll-2] * Math.pow((wa - rparms[ll]),mr) * 
                  Math.cos(jr*wa+rparms[ll-1]);
    }

    return (att);
  }




  private void inst2e(double r, double p, double y,double[][] a, double[][]at) {

// r =  roll angle in radians
// p = pitch angle in radians
// y = yaw angle in radians
// a(3,3) = spacecraft to ecef coordinates transformation matrix
// at(3,3)= instrument to ecef coordinates transformation matrix

    double[][] rpy = new double[3][3];
    int i, j;

// we compute instrument to body coordinates transformation
// matrix by using a small angle approximation of trigonometric
// funktions of the roll, pitch and yaw.
    rpy[0][0] = 1. - 0.5 * (p * p + y * y);
    rpy[0][1] = -y;
    rpy[0][2] = p;
    rpy[1][0] = y + p * r;
    rpy[1][1] = 1. - 0.5 * (y * y + r * r);
    rpy[1][2] = -r;
    rpy[2][0] = -p + r * y;
    rpy[2][1] = r + p * y;
    rpy[2][2] = 1. - 0.5 * (p * p + r * r);

// multiplication of matrices a and rpy
     
    for (i=0; i<3; i++) {
      for (j=0; j<3; j++) {
        at[i][j] = a[i][0] * rpy[0][j] + a[i][1] * rpy[1][j] + 
              a[i][2] * rpy[2][j];
      }
      }
  return;

  }



  /** return the lat,lon of the subpoint
  *
  * @return double[2] {lat, lon}
  *
  */
  
  public double[] getSubpoint() {
    return subpoint;
  }


  /** converts from satellite coordinates to latitude/longitude
   *
   * @param  linele[][]  array of line/element pairs.  Where 
   *                     linele[indexLine][] is a 'line' and 
   *                     linele[indexEle][] is an element. These are in 
   *                     'file' coordinates (not "image" coordinates.)
   *
   * @return latlon[][]  array of lat/lon pairs. Output array is 
   *                     latlon[indexLat][] of latitudes and 
   *                     latlon[indexLon][] of longitudes.
   *
   */
  public double[][] toLatLon(double[][] linele) { 

    double rl, rp;
    double ylat, ylon;
    double rlat, rlon, gam, alf;
    int number = linele[0].length;

    // alpha = elevation angle (rad)
    // zeta = scan angle (rad)
    double q1, q2, d, h, alpha, zeta, alpha0, zeta0, ff, doff;
    double[] g1 = new double[3];
    double[] g = new double[3];
    double[] u = new double[3];
    double sa, ca, da, dz, d1, cz;
    double[][] latlon = new double[2][number];

    //  transform line/pixel to geographic coordinates:
    double imglinele[][] = areaCoordToImageCoord(linele); 

    for (int point=0; point<number; point++) {

      //  set input line/pixel numbers
      rl = imglinele[indexLine][point];
      rp = imglinele[indexEle][point];

      //  if doing sounder nav, have to trick routines into thinking image is
      //  at res 1, because nav routines take sounder res into account
      if (instr == 2) {
        rl = (rl+9.)/10.;
        rp = (rp+9.)/10.;
      }
       
       //  compute elevation and scan angles (e,s) related to input
       //  line and pixel numbers

      if (instr == 1) {
        alpha0 = elvmax[0] - (rl - 4.5) * elvln[0];
      } else {
        alpha0 = elvmax[1] - (rl - 2.5) * elvln[1];
      }

      zeta0 = (rp - 1.0) * scnpx[instr-1] - scnmax[instr - 1];

      // compute sign of misalignment corrections and origin offset
      ff = (double) iflip;
      if (instr == 2) ff = -ff;
      doff = scnmax[instr - 1] - ewnom[instr - 1];;


      // add new second order origin offset correction
      alpha = alpha0- alpha0 * zeta0 * doff;
      zeta = zeta0+ 0.5f * alpha0 * alpha0 * doff;

      //  transform elevation and scan angles to geographic coordinates
      //  (this is the old 'lpoint' routine...

     // computes trigonometric funktions of the scan and elevation
     // angles corrected for the roll and pitch misalignments
      ca = Math.cos(alpha);
      sa = Math.sin(alpha);
      cz = Math.cos(zeta);
      da = alpha-pma*sa*(ff/cz+Math.tan(zeta))-rma*(1.0d-ca/cz);
      dz = zeta + ff * rma * sa;

     // corrected scan angle
      cz = Math.cos(dz);

     // computes pointing vector in instrument coordinates
      g[0] = Math.sin(dz);
      g[1] = -cz * Math.sin(da);
      g[2] = cz * Math.cos(da);

     // transforms the pointing vector to earth fixed coordinates
      g1[0] = bt[0][0] * g[0] + bt[0][1] * g[1] + bt[0][2] * g[2];
      g1[1] = bt[1][0] * g[0] + bt[1][1] * g[1] + bt[1][2] * g[2];
      g1[2] = bt[2][0] * g[0] + bt[2][1] * g[1] + bt[2][2] * g[2];

     // computes coefficients and solves a quadratic equation to
     // find the intersect of the pointing vector with the earth
     // surface
      q1 = g1[0]*g1[0] + g1[1]*g1[1] + aebe2c * g1[2]*g1[2];
      q2 = xs[0] * g1[0] + xs[1] * g1[1] + aebe2c * xs[2] * g1[2];
      d  = q2 * q2 - q1 * q3;
      if (Math.abs(d) < 1.d-9) {
         d=0.;
      }

     // if the discriminant of the equation, d, is negative, the
     // instrument points off the earth

      if (d >= 0.0) {
        d = Math.sqrt(d);

       // slant distance from the satellite to the earth point
        h = -(q2 + d) / q1;

       // cartesian coordinates of the earth point
        u[0] = xs[0] + h * g1[0];
        u[1] = xs[1] + h * g1[1];
        u[2] = xs[2] + h * g1[2];

       // sinus of geocentric latitude
        d1 = u[2] / Math.sqrt(u[0]*u[0] + u[1]*u[1] + u[2]*u[2]);

       // geographic (geodetic) coordinates of the point
        rlat = Math.atan(aebe2c * d1 / Math.sqrt(1. - d1 * d1));
        rlon = Math.atan2(u[1],u[0]);
      } else {
        latlon[indexLat][point] = Double.NaN;
        latlon[indexLon][point] = Double.NaN;
        continue;
      }

      rlat = rlat * DEG;
      rlon = rlon * DEG;

       //  put longitude into mcidas form
      if (!isEastPositive) rlon = -rlon;

       //  see if we have to convert to x y z coordinates
      if (itype == 2) {
        ylat = (double) rlat;
        ylon = (double) rlon;
        // llcart(ylat,ylon,xlat,xlon,z);
      } else {
        latlon[indexLat][point] = rlat;
        latlon[indexLon][point] = rlon;
      }

    } // end point for loop

    return latlon;

  }

  /** converts from satellite coordinates to latitude/longitude
   *
   * @param  linele[][]  array of line/element pairs.  Where 
   *                     linele[indexLine][] is a 'line' and 
   *                     linele[indexEle][] is an element. These are in 
   *                     'file' coordinates (not "image" coordinates.)
   *
   * @return latlon[][]  array of lat/lon pairs. Output array is 
   *                     latlon[indexLat][] of latitudes and 
   *                     latlon[indexLon][] of longitudes.
   *
   */
  public float[][] toLatLon(float[][] linele) { 

    double rl, rp;
    double ylat, ylon;
    double rlat, rlon, gam, alf;
    int number = linele[0].length;

    // alpha = elevation angle (rad)
    // zeta = scan angle (rad)
    double q1, q2, d, h, alpha, zeta, alpha0, zeta0, ff, doff;
    double[] g1 = new double[3];
    double[] g = new double[3];
    double[] u = new double[3];
    double sa, ca, da, dz, d1, cz;
    float[][] latlon = new float[2][number];

    //  transform line/pixel to geographic coordinates:
    float imglinele[][] = areaCoordToImageCoord(linele); 

    for (int point=0; point<number; point++) {

      //  set input line/pixel numbers
      rl = imglinele[indexLine][point];
      rp = imglinele[indexEle][point];

      //  if doing sounder nav, have to trick routines into thinking image is
      //  at res 1, because nav routines take sounder res into account
      if (instr == 2) {
        rl = (rl+9.)/10.;
        rp = (rp+9.)/10.;
      }
       
       //  compute elevation and scan angles (e,s) related to input
       //  line and pixel numbers

      if (instr == 1) {
        alpha0 = elvmax[0] - (rl - 4.5) * elvln[0];
      } else {
        alpha0 = elvmax[1] - (rl - 2.5) * elvln[1];
      }

      zeta0 = (rp - 1.0) * scnpx[instr-1] - scnmax[instr - 1];

      // compute sign of misalignment corrections and origin offset
      ff = (double) iflip;
      if (instr == 2) ff = -ff;
      doff = scnmax[instr - 1] - ewnom[instr - 1];;


      // add new second order origin offset correction
      alpha = alpha0- alpha0 * zeta0 * doff;
      zeta = zeta0+ 0.5f * alpha0 * alpha0 * doff;

      //  transform elevation and scan angles to geographic coordinates
      //  (this is the old 'lpoint' routine...

     // computes trigonometric funktions of the scan and elevation
     // angles corrected for the roll and pitch misalignments
      ca = Math.cos(alpha);
      sa = Math.sin(alpha);
      cz = Math.cos(zeta);
      da = alpha-pma*sa*(ff/cz+Math.tan(zeta))-rma*(1.0d-ca/cz);
      dz = zeta + ff * rma * sa;

     // corrected scan angle
      cz = Math.cos(dz);

     // computes pointing vector in instrument coordinates
      g[0] = Math.sin(dz);
      g[1] = -cz * Math.sin(da);
      g[2] = cz * Math.cos(da);

     // transforms the pointing vector to earth fixed coordinates
      g1[0] = bt[0][0] * g[0] + bt[0][1] * g[1] + bt[0][2] * g[2];
      g1[1] = bt[1][0] * g[0] + bt[1][1] * g[1] + bt[1][2] * g[2];
      g1[2] = bt[2][0] * g[0] + bt[2][1] * g[1] + bt[2][2] * g[2];

     // computes coefficients and solves a quadratic equation to
     // find the intersect of the pointing vector with the earth
     // surface
      q1 = g1[0]*g1[0] + g1[1]*g1[1] + aebe2c * g1[2]*g1[2];
      q2 = xs[0] * g1[0] + xs[1] * g1[1] + aebe2c * xs[2] * g1[2];
      d  = q2 * q2 - q1 * q3;
      if (Math.abs(d) < 1.d-9) {
         d=0.;
      }

     // if the discriminant of the equation, d, is negative, the
     // instrument points off the earth

      if (d >= 0.0) {
        d = Math.sqrt(d);

       // slant distance from the satellite to the earth point
        h = -(q2 + d) / q1;

       // cartesian coordinates of the earth point
        u[0] = xs[0] + h * g1[0];
        u[1] = xs[1] + h * g1[1];
        u[2] = xs[2] + h * g1[2];

       // sinus of geocentric latitude
        d1 = u[2] / Math.sqrt(u[0]*u[0] + u[1]*u[1] + u[2]*u[2]);

       // geographic (geodetic) coordinates of the point
        rlat = Math.atan(aebe2c * d1 / Math.sqrt(1. - d1 * d1));
        rlon = Math.atan2(u[1],u[0]);
      } else {
        latlon[indexLat][point] = Float.NaN;
        latlon[indexLon][point] = Float.NaN;
        continue;
      }

      rlat = rlat * DEG;
      rlon = rlon * DEG;

       //  put longitude into mcidas form
      if (!isEastPositive) rlon = -rlon;

       //  see if we have to convert to x y z coordinates
      if (itype == 2) {
        ylat = (double) rlat;
        ylon = (double) rlon;
        // llcart(ylat,ylon,xlat,xlon,z);
      } else {
        latlon[indexLat][point] = (float) rlat;
        latlon[indexLon][point] = (float) rlon;
      }

    } // end point for loop

    return latlon;

  }


  /**
   * toLinEle converts lat/long to satellite line/element
   *
   * @param  latlon[][]  array of lat/long pairs. Where latlon[indexLat][]
   *                     are latitudes and latlon[indexLon][] are longitudes.
   *
   * @return linele[][]  array of line/element pairs.  Where
   *                     linele[indexLine][] is a line and linele[indexEle][] 
   *                     is an element.  These are in 'file' coordinates
   *                     (not "image" coordinates);
   */
  public double[][] toLinEle(double[][] latlon) {

    double x, y;
    double tmplin, tmpele;
    double sing, slat, w1, w2, ff, doff, alpha1;
    double rlat, rlon, gam, alf;
    double [] f = new double[3];
    double [] ft = new double[3];
    double [] u = new double[3];
    int number = latlon[0].length;
    double[][] linele = new double[2][number];

    ff = (double) iflip;
    if (instr == 2) ff = -ff;
    doff = scnmax[instr-1] - ewnom[instr - 1];

    for (int point=0; point<number; point++) {

     // if in cartesian coords, transform to lat/lon
      if (itype == 2){
        x = latlon[indexLat][point];
        y = latlon[indexLon][point];
        // cartll(x,y,z,zlat,zlon);
      }

      if (Math.abs(latlon[indexLat][point]) > 90.) {
        linele[indexLine][point] = Double.NaN;
        linele[indexEle][point] = Double.NaN;
        continue;
      }

      rlat = (double)latlon[indexLat][point]*RAD;
      rlon = (double)latlon[indexLon][point]*RAD;
      if (!isEastPositive) rlon = -rlon;

     // transform lat/lon to elevation and scan angles
     // (used to be the gpoint routine...)

     // computes sinus of geographic (geodetic) latitude
      sing = Math.sin(rlat);
      w1   = aebe4c * sing * sing;

     // sinus of the geocentric latitude
      slat = ((0.375 * w1 - 0.5) * w1 + 1.) * sing / aebe2c;

     // computes local earth radius at specified point
      w2 = slat * slat;
      w1 = aebe3c * w2;
      w1 = (0.375 * w1 - 0.5) * w1 + 1.;

     // computes cartesian coordinates of the point
      u[2] = slat * w1;
      w2   = w1 * Math.sqrt(1. - w2);
      u[0] = w2 * Math.cos(rlon);
      u[1] = w2 * Math.sin(rlon);

     // pointing vector from satellite to the earth point
      f[0] = u[0] - xs[0];
      f[1] = u[1] - xs[1];
      f[2] = u[2] - xs[2];
      w2 = u[0] * f[0] + u[1] * f[1] + u[2] * f[2] * aebe2c;

     // verifies visibility of the point
     if (w2 <= 0.0) {
       // converts pointing vector to instrument coordinates
        ft[0] = bt[0][0] * f[0] + bt[1][0] * f[1] + bt[2][0] * f[2];
        ft[1] = bt[0][1] * f[0] + bt[1][1] * f[1] + bt[2][1] * f[2];
        ft[2] = bt[0][2] * f[0] + bt[1][2] * f[1] + bt[2][2] * f[2];

       // converts pointing vector to scan and elevation angles and
       // corrects for the roll and pitch misalignments
        gam  = Math.atan(ft[0] / Math.sqrt(ft[1]*ft[1] + ft[2]*ft[2] ) );
        alf  = -Math.atan( ft[1] / ft[2] );
        w1   = Math.sin(alf);
        w2   = Math.cos(gam);
        alpha1  = alf + rma * (1. - Math.cos(alf) / w2) + pma * w1 * 
                         (doff / w2 + Math.tan(gam));
        gam  = gam - ff * rma * w1;
        alf = alpha1 + alpha1 * gam * doff;
        gam = gam - 0.5f * alpha1 * alpha1 * doff;

      } else {
        // not visible...
        linele[indexLine][point] = Double.NaN;
        linele[indexEle][point] = Double.NaN;
        continue;
      }

   // convert elevation and scan angles to line/pixel coordinates

   // compute fractional line number

      tmplin = (elvmax[instr-1] - alf) / elvln[instr-1];
      if (instr == 1) {
        tmplin = tmplin + 4.5;
      } else {
        tmplin = tmplin + 2.5;
      }

   // compute fractional pixel number
      tmpele = (scnmax[instr-1] + gam) / scnpx[instr-1] + 1.;

   // convert internal 8 byte values to 4 bytes
      linele[indexLine][point] = tmplin;
      linele[indexEle][point] = tmpele;

   // if doing sounder nav, change lin & ele returned to res 10 values
      if (instr == 2) {
        linele[indexLine][point] = linele[indexLine][point]*10.f-9.f;
        linele[indexEle][point] = linele[indexEle][point]*10.f-9.f;
      }

    } // end for loop on points

    // Return in 'File' coordinates
    return imageCoordToAreaCoord(linele, linele);
  }

  /**
   * toLinEle converts lat/long to satellite line/element
   *
   * @param  latlon[][]  array of lat/long pairs. Where latlon[indexLat][]
   *                     are latitudes and latlon[indexLon][] are longitudes.
   *
   * @return linele[][]  array of line/element pairs.  Where
   *                     linele[indexLine][] is a line and linele[indexEle][] 
   *                     is an element.  These are in 'file' coordinates
   *                     (not "image" coordinates);
   */
  public float[][] toLinEle(float[][] latlon) {

    double x, y;
    double tmplin, tmpele;
    double sing, slat, w1, w2, ff, doff, alpha1;
    double rlat, rlon, gam, alf;
    double [] f = new double[3];
    double [] ft = new double[3];
    double [] u = new double[3];
    int number = latlon[0].length;
    float[][] linele = new float[2][number];

    ff = (double) iflip;
    if (instr == 2) ff = -ff;
    doff = scnmax[instr-1] - ewnom[instr - 1];

    for (int point=0; point<number; point++) {

     // if in cartesian coords, transform to lat/lon
      if (itype == 2){
        x = latlon[indexLat][point];
        y = latlon[indexLon][point];
        // cartll(x,y,z,zlat,zlon);
      }

      if (Math.abs(latlon[indexLat][point]) > 90.) {
        linele[indexLine][point] = Float.NaN;
        linele[indexEle][point] = Float.NaN;
        continue;
      }

      rlat = (double)latlon[indexLat][point]*RAD;
      rlon = (double)latlon[indexLon][point]*RAD;
      if (!isEastPositive) rlon = -rlon;

     // transform lat/lon to elevation and scan angles
     // (used to be the gpoint routine...)

     // computes sinus of geographic (geodetic) latitude
      sing = Math.sin(rlat);
      w1   = aebe4c * sing * sing;

     // sinus of the geocentric latitude
      slat = ((0.375 * w1 - 0.5) * w1 + 1.) * sing / aebe2c;

     // computes local earth radius at specified point
      w2 = slat * slat;
      w1 = aebe3c * w2;
      w1 = (0.375 * w1 - 0.5) * w1 + 1.;

     // computes cartesian coordinates of the point
      u[2] = slat * w1;
      w2   = w1 * Math.sqrt(1. - w2);
      u[0] = w2 * Math.cos(rlon);
      u[1] = w2 * Math.sin(rlon);

     // pointing vector from satellite to the earth point
      f[0] = u[0] - xs[0];
      f[1] = u[1] - xs[1];
      f[2] = u[2] - xs[2];
      w2 = u[0] * f[0] + u[1] * f[1] + u[2] * f[2] * aebe2c;

     // verifies visibility of the point
     if (w2 <= 0.0) {
       // converts pointing vector to instrument coordinates
        ft[0] = bt[0][0] * f[0] + bt[1][0] * f[1] + bt[2][0] * f[2];
        ft[1] = bt[0][1] * f[0] + bt[1][1] * f[1] + bt[2][1] * f[2];
        ft[2] = bt[0][2] * f[0] + bt[1][2] * f[1] + bt[2][2] * f[2];

       // converts pointing vector to scan and elevation angles and
       // corrects for the roll and pitch misalignments
        gam  = Math.atan(ft[0] / Math.sqrt(ft[1]*ft[1] + ft[2]*ft[2] ) );
        alf  = -Math.atan( ft[1] / ft[2] );
        w1   = Math.sin(alf);
        w2   = Math.cos(gam);
        alpha1  = alf + rma * (1. - Math.cos(alf) / w2) + pma * w1 * 
                         (doff / w2 + Math.tan(gam));
        gam  = gam - ff * rma * w1;
        alf = alpha1 + alpha1 * gam * doff;
        gam = gam - 0.5f * alpha1 * alpha1 * doff;

      } else {
        // not visible...
        linele[indexLine][point] = Float.NaN;
        linele[indexEle][point] = Float.NaN;
        continue;
      }

   // convert elevation and scan angles to line/pixel coordinates

   // compute fractional line number

      tmplin = (elvmax[instr-1] - alf) / elvln[instr-1];
      if (instr == 1) {
        tmplin = tmplin + 4.5;
      } else {
        tmplin = tmplin + 2.5;
      }

   // compute fractional pixel number
      tmpele = (scnmax[instr-1] + gam) / scnpx[instr-1] + 1.;

   // convert internal 8 byte values to 4 bytes
      linele[indexLine][point] = (float) tmplin;
      linele[indexEle][point] = (float) tmpele;

   // if doing sounder nav, change lin & ele returned to res 10 values
      if (instr == 2) {
        linele[indexLine][point] = linele[indexLine][point]*10.f-9.f;
        linele[indexEle][point] = linele[indexEle][point]*10.f-9.f;
      }

    } // end for loop on points

    // Return in 'File' coordinates
    return imageCoordToAreaCoord(linele, linele);
  }

}
