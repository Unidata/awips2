//
// GOESnav.java
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

/**
 * Navigation class for GOES (GOES D-H) type nav. This code was modified
 * from the original FORTRAN code (nvxgoes.dlm) on the McIDAS system. It
 * only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Don Murray
 */
public final class GOESnav extends AREAnav 
{

    private boolean isEastPositive = true;

    // NAVCOM variables
    private int navday;
    private int lintot;
    private double deglin;
    private int ieltot;
    private double degele;
    private double spinra;
    private int ietimy;
    private int ietimh;
    private double semima;
    private double oeccen;
    private double orbinc;
    private double perhel;
    private double asnode;
    private double nopcln;
    private double declin;
    private double rascen;
    private double piclin;
    private double prerat;
    private double predir;
    private double pitch;
    private double yaw;
    private double roll;
    private double skew;

    // BETCOM variables
    private int iajust;
    private int ibtcon;
    private int negbet;
    private int iseang;

    // VASCOM variables
    private double scan1;
    private double time1;
    private double scan2;
    private double time2;

    // NAVINI variables
    private double emega;
    private double ab;
    private double asq;
    private double bsq;
    private double r;
    private double rsq;
    private double rdpdg;
    private int numsen;
    private double totlin;
    private double radlin;
    private double totele;
    private double radele;
    private double picele;
    private double cpitch;
    private double cyaw;
    private double croll;
    private double pskew;
    private double rfact;
    private double roasin;
    private double tmpscl;
    private double b11;
    private double b12;
    private double b13;
    private double b21;
    private double b22;
    private double b23;
    private double b31;
    private double b32;
    private double b33;
    private double gamma;
    private double gamdot;
    private double rotm11;
    private double rotm13;
    private double rotm21;
    private double rotm23;
    private double rotm31;
    private double rotm33;
    private double pictim;
    private double xref;

    private int iold = 0;

    // variables needed for satvec
    private double tdife;
    private double xmmc;
    private double epsiln;
    private double srome2;
    private double pz;
    private double py;
    private double px;
    private double qz;
    private double qy;
    private double qx;

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the GOES nav 'codicil'.
     *
     * @param iarr  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a GOES type.
     */
    public GOESnav (int[] iarr) 
        throws IllegalArgumentException
    {

/* No longer needed.  Kept for consistency with nvxgoes.dlm
        if (ifunc != 1) 
        {
            if (iarr[0] == XY ) itype = 1;
            if (iarr[0] == LL ) itype = 2;
            return;
        }
*/

        if (iarr[0] != GOES ) 
            throw new IllegalArgumentException("Invalid navigation type" + 
                                                iarr[0]);
        
        int jday = iarr[1];
        int jtime = iarr[2];

        // INITIALIZE NAVCOM
        navday = jday%100000;
        
        for (int n = 6; n < 12; n++)
        {
            if (iarr[n] <= 0)
               throw new IllegalArgumentException("Invalid orbital parameters");
        }
        ietimy = icon1(iarr[4]);
        ietimh = 100*(iarr[5]/100) + Math.round(.6f*(iarr[5]%100));
        semima = (float) (iarr[6])/100.0;
        oeccen = (float) (iarr[7])/1000000.0;
        orbinc = (float) (iarr[8])/1000.0;
        double xmeana = (float) (iarr[9])/1000.0;
        perhel = (float) (iarr[10])/1000.0;
        asnode = (float) (iarr[11])/1000.0;
        if (iarr[4] == 0)
            throw new IllegalArgumentException("Invalid orbit type");

        //CALL EPOCH(IETIMY,IETIMH,SEMIMA,OECCEN,XMEANA);
        epoch(ietimy,ietimh,semima,oeccen,xmeana);

        declin = McIDASUtil.mcPackedIntegerToDouble(iarr[12]);
        rascen = McIDASUtil.mcPackedIntegerToDouble(iarr[13]);
        piclin = iarr[14];
        if (iarr[14] >= 1000000) piclin = piclin/10000.;
        if (iarr[12] == 0 && iarr[13] == 0 && iarr[14] == 0)
            throw new IllegalArgumentException(
                            "Invalid ascension/declination parameters");

        // ADDED 9/83 TO SUPPORT FRACTIONAL VALUES FOR PICLIN;
        if (iarr[15] == 0)
            throw new IllegalArgumentException("Invalid spin period");
        spinra = iarr[15]/1000.0;
        if(iarr[15] != 0 && spinra < 300.0) spinra = 60000.0/spinra;

        deglin = McIDASUtil.mcPackedIntegerToDouble(iarr[16]);
        lintot = iarr[17];
        degele = McIDASUtil.mcPackedIntegerToDouble(iarr[18]);
        ieltot = iarr[19];
        pitch  = McIDASUtil.mcPackedIntegerToDouble(iarr[20]);
        yaw    = McIDASUtil.mcPackedIntegerToDouble(iarr[21]);
        roll   = McIDASUtil.mcPackedIntegerToDouble(iarr[22]);
        skew   = iarr[28]/100000.0;
        if (iarr[28] == McIDASUtil.MCMISSING) skew = 0.;


        //-----INITIALIZE BETCOM
        iajust = iarr[24];
        iseang = iarr[27];
        ibtcon = 6289920;
        negbet = 3144960;

        //-----INITIALIZE NAVINI COMMON BLOCK
        emega = .26251617;
        ab = 40546851.22;
        asq = 40683833.48;
        bsq = 40410330.18;
        r = 6371.221;
        rsq = r*r;
        rdpdg = 1.745329252E-02;
        numsen = (lintot/100000)%100;
        if (numsen < 1) numsen = 1;
        totlin = numsen * (lintot%100000);
        radlin = rdpdg*deglin/(totlin-1.0);
        totele = ieltot;
        radele = rdpdg*degele/(totele-1.0);
        picele = (1.0+totele)/2.0;
        cpitch = rdpdg*pitch;
        cyaw = rdpdg*yaw;
        croll = rdpdg*roll;
        pskew = Math.atan2(skew, radlin/radele);
        double stp = Math.sin(cpitch);
        double ctp = Math.cos(cpitch);
        double sty = Math.sin(cyaw-pskew);
        double cty = Math.cos(cyaw-pskew);
        double str = Math.sin(croll);
        double ctr = Math.cos(croll);
        rotm11 = ctr*ctp;
        rotm13 = sty*str*ctp + cty*stp;
        rotm21 = -str;
        rotm23 = sty*ctr;
        rotm31 = -ctr*stp;
        rotm33 = cty*ctp - sty*str*stp;
        rfact = Math.pow(rotm31,2) + Math.pow(rotm33,2);
        roasin = Math.atan2(rotm31, rotm33);
        tmpscl = spinra/3600000.0;
        double dec = declin*rdpdg;
        double sindec = Math.sin(dec);
        double cosdec = Math.cos(dec);
        double ras = rascen*rdpdg;
        double sinras = Math.sin(ras);
        double cosras = Math.cos(ras);
        b11 = -sinras;
        b12 = cosras;
        b13 = 0.0;
        b21 = -sindec*cosras;
        b22 = -sindec*sinras;
        b23 = cosdec;
        b31 = cosdec*cosras;
        b32 = cosdec*sinras;
        b33 = sindec;

        //XREF=RAERAC(NAVDAY,0,0.0)*RDPDG
        double raha = 
            McIDASUtil.timdif(74001, 0, navday, 0)*1.00273791/4.0 + 100.26467;
        double rac = raha%360.0;
        if (rac < 0) rac = rac + 360.0;
        xref = rac*rdpdg;

        //-----TIME-SPECIFIC PARAMETERS (INCL GAMMA)
        pictim = McIDASUtil.mcPackedIntegerToDouble(jtime);
        gamma  = ((float) iarr[38])/100.;
        gamdot = ((float) iarr[39])/100.;

        //-----INITIALIZE VASCOM
        int iss = jday/100000;
        if ( (iss > 25 || iss == 12) && iarr[30] > 0)
        {
        //       THIS SECTION DOES VAS BIRDS AND GMS
        //       IT USES TIMES AND SCAN LINE FROM BETA RECORDS
            scan1 = (double) iarr[30];
            time1 = McIDASUtil.mcPackedIntegerToDouble(iarr[31]);
            scan2 = (double) iarr[34];
            time2 = McIDASUtil.mcPackedIntegerToDouble(iarr[35]);
        }
        else
        {
        //        THIS SECTION DOES THE OLD GOES BIRDS
            scan1 = 1.0;
            time1 = McIDASUtil.mcPackedIntegerToDouble(jtime);
            scan2 = (double) (lintot%100000);
            time2 = time1 + scan2*tmpscl;
        }
        iold = 0;
    }

    /** converts from satellite coordinates to latitude/longitude
     *
     * @param  linele[][]  array of line/element pairs.  Where 
     *                     linele[indexLine][] is a 'line' and 
     *                     linele[indexEle][] is an element. These are in 
     *                     'file' coordinates (not "image" coordinates.)
     *
     * @return latlon[][]  array of lat/long pairs. Output array is 
     *                     latlon[indexLat][] of latitudes and 
     *                     latlon[indexLon][] of longitudes.
     *
     */
    public float[][] toLatLon(float[][] linele) 
    {

        int ilin;
        double parlin;
        double framet;
        double samtim;
        double xlin;
        double xele;
        double ylin;
        double yele;
        double xcor;
        double ycor;
        double rot;
        double coslin;
        double sinlin;
        double cosele;
        double sinele;
        double eli;
        double emi;
        double eni;
        double temp;
        double elo;
        double emo;
        double eno;
        double basq;
        double onemsq;
        double aq;
        double bq;
        double cq;
        double rad;
        double s;
        double x;
        double y;
        double z;
        double ct;
        double st;
        double x1;
        double y1;

        int number = linele[0].length;
        float[][] latlon = new float[2][number];

        // Convert array to Image coordinates for computations
        float[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];
            ilin = Math.round( (float) xlin);
            parlin = (ilin - 1)/numsen + 1;
            framet = tmpscl*parlin;
            samtim = framet + pictim;
            double xyz[] = satvec(samtim);
            ylin = (xlin - piclin) * radlin;
            yele = (xele - picele + gamma + gamdot*samtim)*radele;
            xcor = b11*xyz[0] + b12*xyz[1] + b13*xyz[2];
            ycor = b21*xyz[0] + b22*xyz[1] + b23*xyz[2];
            rot = Math.atan2(ycor, xcor) + Math.PI;
            yele = yele - rot;
            coslin = Math.cos(ylin);
            sinlin = Math.sin(ylin);
            sinele = Math.sin(yele);
            cosele = Math.cos(yele);
            eli = rotm11*coslin - rotm13*sinlin;
            emi = rotm21*coslin - rotm23*sinlin;
            eni = rotm31*coslin - rotm33*sinlin;
            temp = eli;
            eli = cosele*eli + sinele*emi;
            emi = -sinele*temp + cosele*emi;
            elo = b11*eli + b21*emi + b31*eni;
            emo = b12*eli + b22*emi + b32*eni;
            eno = b13*eli + b23*emi + b33*eni;
            basq = bsq/asq;
            onemsq = 1.0 - basq;
            aq = basq + onemsq*Math.pow(eno,2);
            bq = 2.0 * ((elo*xyz[0] + emo*xyz[1])*basq + eno*xyz[2]);
            cq = (Math.pow(xyz[0],2) + Math.pow(xyz[1],2))*basq +
                            Math.pow(xyz[2],2) - bsq;
            rad = Math.pow(bq,2) - 4.0*aq*cq;
            if (rad < 1.0)
            {
                latlon[indexLat][point] = Float.NaN;
                latlon[indexLon][point] = Float.NaN;
            }
            else
            {
                s = -(bq + Math.sqrt(rad))/(2.0*aq);
                x = xyz[0] + elo*s;
                y = xyz[1] + emo*s;
                z = xyz[2] + eno*s;
                ct = Math.cos(emega*samtim+xref);
                st = Math.sin(emega*samtim+xref);
                x1 = ct*x + st*y;
                y1 = -st*x + ct*y;
                double ll[] = nxyzll(x1, y1, z);

                latlon[indexLat][point] = (float) ll[0];
                //  put longitude into East Positive (form)
                latlon[indexLon][point] = (isEastPositive) ? (float)-ll[1] : (float)ll[1];
            }
        } // end point for loop

        return latlon;

    }

    public double[][] toLatLon(double[][] linele) 
    {

        int ilin;
        double parlin;
        double framet;
        double samtim;
        double xlin;
        double xele;
        double ylin;
        double yele;
        double xcor;
        double ycor;
        double rot;
        double coslin;
        double sinlin;
        double cosele;
        double sinele;
        double eli;
        double emi;
        double eni;
        double temp;
        double elo;
        double emo;
        double eno;
        double basq;
        double onemsq;
        double aq;
        double bq;
        double cq;
        double rad;
        double s;
        double x;
        double y;
        double z;
        double ct;
        double st;
        double x1;
        double y1;

        int number = linele[0].length;
        double[][] latlon = new double[2][number];

        // Convert array to Image coordinates for computations
        double[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            xlin = imglinele[indexLine][point];
            xele = imglinele[indexEle][point];
            ilin = Math.round( (float) xlin);
            parlin = (ilin - 1)/numsen + 1;
            framet = tmpscl*parlin;
            samtim = framet + pictim;
            double xyz[] = satvec(samtim);
            ylin = (xlin - piclin) * radlin;
            yele = (xele - picele + gamma + gamdot*samtim)*radele;
            xcor = b11*xyz[0] + b12*xyz[1] + b13*xyz[2];
            ycor = b21*xyz[0] + b22*xyz[1] + b23*xyz[2];
            rot = Math.atan2(ycor, xcor) + Math.PI;
            yele = yele - rot;
            coslin = Math.cos(ylin);
            sinlin = Math.sin(ylin);
            sinele = Math.sin(yele);
            cosele = Math.cos(yele);
            eli = rotm11*coslin - rotm13*sinlin;
            emi = rotm21*coslin - rotm23*sinlin;
            eni = rotm31*coslin - rotm33*sinlin;
            temp = eli;
            eli = cosele*eli + sinele*emi;
            emi = -sinele*temp + cosele*emi;
            elo = b11*eli + b21*emi + b31*eni;
            emo = b12*eli + b22*emi + b32*eni;
            eno = b13*eli + b23*emi + b33*eni;
            basq = bsq/asq;
            onemsq = 1.0 - basq;
            aq = basq + onemsq*Math.pow(eno,2);
            bq = 2.0 * ((elo*xyz[0] + emo*xyz[1])*basq + eno*xyz[2]);
            cq = (Math.pow(xyz[0],2) + Math.pow(xyz[1],2))*basq +
                            Math.pow(xyz[2],2) - bsq;
            rad = Math.pow(bq,2) - 4.0*aq*cq;
            if (rad < 1.0)
            {
                latlon[indexLat][point] = Double.NaN;
                latlon[indexLon][point] = Double.NaN;
            }
            else
            {
                s = -(bq + Math.sqrt(rad))/(2.0*aq);
                x = xyz[0] + elo*s;
                y = xyz[1] + emo*s;
                z = xyz[2] + eno*s;
                ct = Math.cos(emega*samtim+xref);
                st = Math.sin(emega*samtim+xref);
                x1 = ct*x + st*y;
                y1 = -st*x + ct*y;
                double ll[] = nxyzll(x1, y1, z);

                latlon[indexLat][point] = ll[0];
                //  put longitude into East Positive (form)
                latlon[indexLon][point] = (isEastPositive) ? -ll[1] : ll[1];
            }
        } // end point for loop

        return latlon;

    }

    /**
     * toLinEle converts lat/long to satellite line/element
     *
     * @param  latlon[][] array of lat/long pairs. Where latlon[indexLat][]
     *                    are latitudes and latlon[indexLon][] are longitudes.
     *
     * @return linele[][] array of line/element pairs.  Where
     *                    linele[indexLine][] is a line and linele[indexEle][]
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public float[][] toLinEle(float[][] latlon) 
    {

        double xpar;
        double ypar;
        double zpar;
        double xlin;
        double xele;
        double xdum;
        double x1;
        double y1;
        double samtim;
        double ct;
        double st;
        double x;
        double y;
        double z;
        double xht;
        double parlin;
        double vcste1;
        double vcste2;
        double vcste3;
        double vcses1;
        double vcses2;
        double vcses3;
        double oldlin;
        double orbtim;
        double xsat;
        double ysat;
        double zsat;
        double xnorm;
        double ynorm;
        double znorm;
        double umv;
        double x3;
        double xyzsat[] = new double[3];

        int number = latlon[0].length;
        float[][] linele = new float[2][number];

        for (int point=0; point < number; point++) 
        {

            xpar = latlon[indexLat][point];
            // expects positive West Longitude.
            ypar = isEastPositive 
                     ? -latlon[indexLon][point]
                     :  latlon[indexLon][point];

            xlin = Double.NaN;
            xele = Double.NaN;

            if (Math.abs(xpar) <= 90.)
            {

                // initialize some variables
                oldlin = 910.; 
                orbtim = -99999.; 
                xsat = ysat = zsat = 0.0;
                x = y = z = 0.0;
                xht = znorm = 0.0;
                double xyz[] = nllxyz(xpar, ypar);
                x1 = xyz[0];
                y1 = xyz[1];
                z = xyz[2];
                xdum = 0.0;
                samtim = time1;

                for (int i = 0; i < 2; i++)
                {
                    if (Math.abs(samtim - orbtim) >= 0.0005)
                    {
                        xyzsat = satvec(samtim);
                        xsat = xyzsat[0];
                        ysat = xyzsat[1];
                        zsat = xyzsat[2];
                        orbtim = samtim;
                        xht = Math.sqrt(Math.pow(xyzsat[0],2) +
                                        Math.pow(xyzsat[1],2) +
                                        Math.pow(xyzsat[2],2));
                    }
                    ct = Math.cos(emega*samtim + xref);
                    st = Math.sin(emega*samtim + xref);
                    x = ct*x1 - st*y1;
                    y = st*x1 + ct*y1;
                    vcste1 = x - xsat;
                    vcste2 = y - ysat;
                    vcste3 = z - zsat;
                    vcses3 = b31*vcste1 + b32*vcste2 + b33*vcste3;
                    znorm = Math.sqrt(Math.pow(vcste1,2) +
                                             Math.pow(vcste2,2) +
                                             Math.pow(vcste3,2));
                    x3 = vcses3/znorm;
                    umv = Math.atan2(x3,Math.sqrt(rfact - Math.pow(x3,2))) - 
                            roasin;
                    xlin = piclin - umv/radlin;
                    parlin = (double) (xlin - 1.0)/numsen;
                    if (i == 0)
                    {
                        samtim = time2;
                        oldlin = xlin;
                    }
                }
                double scnnum = ( (double) (oldlin + xlin)/2.0 - 1.0)/numsen;
                double scnfrc = (scnnum - scan1)/(scan2 - scan1);
                xlin = oldlin + scnfrc*(xlin - oldlin);
                samtim = time1 + tmpscl*(scnnum - scan1);
                xyzsat = satvec(samtim);
                xsat = xyzsat[0];
                ysat = xyzsat[1];
                zsat = xyzsat[2];
                double cosa = x*xsat + y*ysat + z*zsat;
                double ctst = 0.0001*r*xht + rsq;
                if (cosa >= ctst) 
                {
                    double xsats1 = b11*xsat + b12*ysat + b13*zsat;
                    double ysats2 = b21*xsat + b22*ysat + b23*zsat;
                    ct = Math.cos(emega*samtim + xref);
                    st = Math.sin(emega*samtim + xref);
                    x = ct*x1 - st*y1;
                    y = st*x1 + ct*y1;
                    vcste1 = x - xsat;
                    vcste2 = y - ysat;
                    vcste3 = z - zsat;
                    vcses1 = b11*vcste1 + b12*vcste2 + b13*vcste3;
                    vcses2 = b21*vcste1 + b22*vcste2 + b23*vcste3;
                    vcses3 = b31*vcste1 + b32*vcste2 + b33*vcste3;
                    xnorm = Math.sqrt(Math.pow(znorm,2) - Math.pow(vcses3,2));
                    ynorm = Math.sqrt(Math.pow(xsats1,2) + Math.pow(ysats2,2));
                    znorm = Math.sqrt(Math.pow(vcste1,2) + 
                                      Math.pow(vcste2,2) + 
                                      Math.pow(vcste3,2));
                    x3 = vcses3/znorm;
                    umv = Math.atan2(x3,Math.sqrt(rfact - Math.pow(x3,2))) - 
                            roasin;
                    double slin = Math.sin(umv);
                    double clin = Math.cos(umv);
                    double u = rotm11*clin + rotm13*slin;
                    double v = rotm21*clin + rotm23*slin;
                    xele = picele + Math.asin(
                          (xsats1*vcses2 - ysats2*vcses1)/(xnorm*ynorm))/radele;
                    xele = xele + Math.atan2(v,u)/radele;
                    xele = xele-gamma-gamdot*samtim;
                }
            }
            linele[indexLine][point] = (float)xlin;
            linele[indexEle][point]  = (float)xele;

        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }


    public double[][] toLinEle(double[][] latlon) 
    {

        double xpar;
        double ypar;
        double zpar;
        double xlin;
        double xele;
        double xdum;
        double x1;
        double y1;
        double samtim;
        double ct;
        double st;
        double x;
        double y;
        double z;
        double xht;
        double parlin;
        double vcste1;
        double vcste2;
        double vcste3;
        double vcses1;
        double vcses2;
        double vcses3;
        double oldlin;
        double orbtim;
        double xsat;
        double ysat;
        double zsat;
        double xnorm;
        double ynorm;
        double znorm;
        double umv;
        double x3;
        double xyzsat[] = new double[3];

        int number = latlon[0].length;
        double[][] linele = new double[2][number];

        for (int point=0; point < number; point++) 
        {

            xpar = latlon[indexLat][point];
            // expects positive West Longitude.
            ypar = isEastPositive 
                     ? -latlon[indexLon][point]
                     :  latlon[indexLon][point];

            xlin = Double.NaN;
            xele = Double.NaN;

            if (Math.abs(xpar) <= 90.)
            {

                // initialize some variables
                oldlin = 910.; 
                orbtim = -99999.; 
                xsat = ysat = zsat = 0.0;
                x = y = z = 0.0;
                xht = znorm = 0.0;
                double xyz[] = nllxyz(xpar, ypar);
                x1 = xyz[0];
                y1 = xyz[1];
                z = xyz[2];
                xdum = 0.0;
                samtim = time1;

                for (int i = 0; i < 2; i++)
                {
                    if (Math.abs(samtim - orbtim) >= 0.0005)
                    {
                        xyzsat = satvec(samtim);
                        xsat = xyzsat[0];
                        ysat = xyzsat[1];
                        zsat = xyzsat[2];
                        orbtim = samtim;
                        xht = Math.sqrt(Math.pow(xyzsat[0],2) +
                                        Math.pow(xyzsat[1],2) +
                                        Math.pow(xyzsat[2],2));
                    }
                    ct = Math.cos(emega*samtim + xref);
                    st = Math.sin(emega*samtim + xref);
                    x = ct*x1 - st*y1;
                    y = st*x1 + ct*y1;
                    vcste1 = x - xsat;
                    vcste2 = y - ysat;
                    vcste3 = z - zsat;
                    vcses3 = b31*vcste1 + b32*vcste2 + b33*vcste3;
                    znorm = Math.sqrt(Math.pow(vcste1,2) +
                                             Math.pow(vcste2,2) +
                                             Math.pow(vcste3,2));
                    x3 = vcses3/znorm;
                    umv = Math.atan2(x3,Math.sqrt(rfact - Math.pow(x3,2))) - 
                            roasin;
                    xlin = piclin - umv/radlin;
                    parlin = (double) (xlin - 1.0)/numsen;
                    if (i == 0)
                    {
                        samtim = time2;
                        oldlin = xlin;
                    }
                }
                double scnnum = ( (double) (oldlin + xlin)/2.0 - 1.0)/numsen;
                double scnfrc = (scnnum - scan1)/(scan2 - scan1);
                xlin = oldlin + scnfrc*(xlin - oldlin);
                samtim = time1 + tmpscl*(scnnum - scan1);
                xyzsat = satvec(samtim);
                xsat = xyzsat[0];
                ysat = xyzsat[1];
                zsat = xyzsat[2];
                double cosa = x*xsat + y*ysat + z*zsat;
                double ctst = 0.0001*r*xht + rsq;
                if (cosa >= ctst) 
                {
                    double xsats1 = b11*xsat + b12*ysat + b13*zsat;
                    double ysats2 = b21*xsat + b22*ysat + b23*zsat;
                    ct = Math.cos(emega*samtim + xref);
                    st = Math.sin(emega*samtim + xref);
                    x = ct*x1 - st*y1;
                    y = st*x1 + ct*y1;
                    vcste1 = x - xsat;
                    vcste2 = y - ysat;
                    vcste3 = z - zsat;
                    vcses1 = b11*vcste1 + b12*vcste2 + b13*vcste3;
                    vcses2 = b21*vcste1 + b22*vcste2 + b23*vcste3;
                    vcses3 = b31*vcste1 + b32*vcste2 + b33*vcste3;
                    xnorm = Math.sqrt(Math.pow(znorm,2) - Math.pow(vcses3,2));
                    ynorm = Math.sqrt(Math.pow(xsats1,2) + Math.pow(ysats2,2));
                    znorm = Math.sqrt(Math.pow(vcste1,2) + 
                                      Math.pow(vcste2,2) + 
                                      Math.pow(vcste3,2));
                    x3 = vcses3/znorm;
                    umv = Math.atan2(x3,Math.sqrt(rfact - Math.pow(x3,2))) - 
                            roasin;
                    double slin = Math.sin(umv);
                    double clin = Math.cos(umv);
                    double u = rotm11*clin + rotm13*slin;
                    double v = rotm21*clin + rotm23*slin;
                    xele = picele + Math.asin(
                          (xsats1*vcses2 - ysats2*vcses1)/(xnorm*ynorm))/radele;
                    xele = xele + Math.atan2(v,u)/radele;
                    xele = xele-gamma-gamdot*samtim;
                }
            }
            linele[indexLine][point] = xlin;
            linele[indexEle][point]  = xele;

        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }

    private int icon1(int yymmdd)
    {
    /*
    C $ FUNCTION ICON1 (YYMMDD)
    C $ CONVERTS YYMMDD TO YEAR-DAY (YYDDD)
    C $ YYMMDD = (I) INPUT  DATE TO BE CONVERTED
    C $$ ICON1 = CONVERT, DATE
    */

        int num[] = {0,31,59,90,120,151,181,212,243,273,304,334};
        int year  = (yymmdd/10000)%100;
        int month = (yymmdd/100)%100;
        int day   = yymmdd%100;
        if (month < 0 || month > 12) month = 1;
        int julday = day + num[month - 1];
        if (year%4 == 0 && month > 2) julday = julday + 1;
        return (1000*year + julday);
    }

    private void epoch(int ietimy, int ietimh, double semima,
                       double oeccen, double xmeana)
    {
        double PI = 3.14159265;
        double RDPDG = PI/180.;
        double RE = 6378.388;
        double GRACON = 0.07436574;

        double axmmc = GRACON*Math.pow(Math.sqrt(RE/semima),3);
        double xmanom = RDPDG*xmeana;
        double time = (xmanom-oeccen*Math.sin(xmanom))/(60.0*axmmc);
        double time1 = McIDASUtil.mcPackedIntegerToDouble(ietimh);
        time = time1 - time;
        int iday = 0;
        if (time > 48.0)
        {
            time = time - 48.0;
            iday = 2;
        }
        else if (time > 24.0)
        {
            time = time - 24.0;
            iday = 1;
        }
        else if (time < -24.0)
        {
            time = time + 48.0;
            iday = -2;
        }
        else if (time < 0.0)
        {
            time = time + 24.0;
            iday = -1;
        }
        this.ietimh = McIDASUtil.mcDoubleToPackedInteger(time);
        if (iday != 0)
        {

            int jyear = (ietimy/1000)%100;
            // add 1000 so year will not go under zero
            jyear = jyear + 1000;
            int jday = ietimy%1000;
            jday = jday + iday;
            if (jday < 1)
            {
                jyear = jyear - 1;
                jday = leapyr(jyear) + jday;
            }
            else
            {
                int jtot = leapyr(jyear);
                if (jday > jtot)
                {
                    jyear = jyear + 1;
                    jday = jday + jtot;
                }
            }
            jyear = jyear%100;
            this.ietimy = 1000*jyear + jday;
        }  
    } // End EPOCH

    /* helper method for epoch */
    private int leapyr(int iy)
    {
        return 366-( (iy%4) + 3)/4;
    }

    /*
      SUBROUTINE NLLXYZ(XLAT,XLON,X,Y,Z)
      CONVERT LAT,LON TO EARTH CENTERED X,Y,Z
      (DALY, 1978)
      XLAT,XLON ARE IN DEGREES, WITH NORTH AND WEST POSITIVE
      X,Y,Z ARE GIVEN IN KM. THEY ARE THE COORDINATES IN A RECTANGULAR
         FRAME WITH ORIGIN AT THE EARTH CENTER, WHOSE POSITIVE
         X-AXIS PIERCES THE EQUATOR AT LON 0 DEG, WHOSE POSITIVE Y-AXIS
         PIERCES THE EQUATOR AT LON 90 DEG, AND WHOSE POSITIVE Z-AXIS
         INTERSECTS THE NORTH POLE.
    */
    private double[] nllxyz(double xlat, double xlon)
    {

        double ylat = rdpdg*xlat;
        ylat = Math.atan2(bsq*Math.sin(ylat), asq*Math.cos(ylat));
        double ylon = -rdpdg*xlon;
        double snlt = Math.sin(ylat);
        double cslt = Math.cos(ylat);
        double csln = Math.cos(ylon);
        double snln = Math.sin(ylon);
        double tnlt = Math.pow((snlt/cslt),2);
        double r = ab*Math.sqrt((1.0+tnlt)/(bsq+asq*tnlt));
        double x = r*cslt*csln;
        double y = r*cslt*snln;
        double z = r*snlt;
        return new double[] {x, y, z};
    }


    /*
      SUBROUTINE NXYZLL(X,Y,Z,XLAT,XLON)
      CONVERT EARTH-CENTERED X,Y,Z TO LAT & LON
      X,Y,Z ARE GIVEN IN KM. THEY ARE THE COORDINATES IN A RECTANGULAR
         COORDINATE SYSTEM WITH ORIGIN AT THE EARTH CENTER, WHOSE POS.
         X-AXIS PIERCES THE EQUATOR AT LON 0 DEG, WHOSE POSITIVE Y-AXIS
         PIERCES THE EQUATOR AT LON 90 DEG, AND WHOSE POSITIVE Z-AXIS
         INTERSECTS THE NORTH POLE.
      XLAT,XLON ARE IN DEGREES, WITH NORTH AND WEST POSITIVE
    */
    private double[] nxyzll(double x, double y, double z)
    {
        double xlat;
        double xlon;

        xlat = Double.NaN;
        xlon = Double.NaN;
        if (x != 0.0 && y != 0.0 && z != 0.0)
        {
            double a = Math.atan(z/Math.sqrt(x*x + y*y));
            xlat = Math.atan2(asq*Math.sin(a), bsq*Math.cos(a))/rdpdg;
            xlon = -Math.atan2(y,x)/rdpdg;
        }
        return new double[] {xlat, xlon};
    }

/*
C SATVEC PHILLI 0880 NAVLIB  COMPUTES EARTH SATELLITE AS FUNCTION OF TIM
C VECTOR EARTH-CENTER-TO-SAT (FUNC OF TIME)
*/
    private double[] satvec(double samtim)
    {
        if (iold != 1)
        {
            iold = 1;
            double PI = 3.14159265;
            double rdpdg = PI/180.0;
            double re = 6378.388;
            double gracon = .07436574;
            double solsid = 1.00273791;
            double sha = 100.26467;
            sha = rdpdg*sha;
            int irayd = 74001;
            int irahms = 0;
            double o = rdpdg*orbinc;
            double p = rdpdg*perhel;
            double a = rdpdg*asnode;
            double so = Math.sin(o);
            double co = Math.cos(o);
            double sp = Math.sin(p)*semima;
            double cp = Math.cos(p)*semima;
            double sa = Math.sin(a);
            double ca = Math.cos(a);
            px = cp*ca - sp*sa*co;
            py = cp*sa + sp*ca*co;
            pz = sp*so;
            qx = -sp*ca - cp*sa*co;
            qy = -sp*sa + cp*ca*co;
            qz = cp*so;
            srome2 = Math.sqrt(1.0 - oeccen) * Math.sqrt(1.0 + oeccen);
            xmmc = gracon*re*Math.sqrt(re/semima)/semima;
            int iey = (ietimy/1000)%100;
            int ied = ietimy%1000;
            int iefac = (iey-1)/4 + 1;
            double de = 365*(iey-1) + iefac + ied - 1;
            double te = 
                1440.0*de + 60.0*McIDASUtil.mcPackedIntegerToDouble(ietimh);
            int iray = irayd/1000;
            int irad = irayd%1000;
            int irafac = (iray-1)/4 + 1;
            double dra = 365*(iray-1) + irafac + irad -1;
            double tra = 
                1440.0*dra + 60.0*McIDASUtil.mcPackedIntegerToDouble(irahms);
            int inavy = (navday/1000)%100;
            int inavd = navday%1000;
            int infac = (inavy-1)/4 + 1;
            double dnav = 365*(inavy-1) + infac + inavd -1;
            tdife = dnav*1440. - te;
            double tdifra = dnav*1440. - tra;
            epsiln = 1.0E-8;
        }

        double timsam = samtim*60.0;
        double diftim = tdife + timsam;
        double xmanom = xmmc*diftim;
        double ecanm1 = xmanom;
        double ecanom = 0;
        int i = 0;
        for (i = 0; i < 20; i++)
        {
            ecanom = xmanom + oeccen*Math.sin(ecanm1);
            if (Math.abs(ecanom-ecanm1) < epsiln) break;
            ecanm1 = ecanom;
        }
        double xomega = Math.cos(ecanom) - oeccen;
        double yomega = srome2*Math.sin(ecanom);
        double z = xomega*pz + yomega*qz;
        double y = xomega*py + yomega*qy;
        double x = xomega*px + yomega*qx;
        return new double[] {x, y, z};
    }
}
