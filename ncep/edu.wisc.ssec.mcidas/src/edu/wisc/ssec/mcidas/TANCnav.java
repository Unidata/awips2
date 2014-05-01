//
// TANCnav.java
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
 * Navigation class for tangent cone (TANC) type nav. This code was 
 * modified from the original FORTRAN code (nvxtanc.dlm) on the McIDAS system. 
 * It only supports latitude/longitude to line/element transformations (LL) 
 * and vice/versa. Transform to 'XYZ' not implemented.
 * @see <A HREF="http://www.ssec.wisc.edu/mug/prog_man/prog_man.html">
 *      McIDAS Programmer's Manual</A>
 *
 * @author  Don Murray
 */
public final class TANCnav extends AREAnav 
{

    private boolean isEastPositive = true;

    int iwest;
    int ihem;
    double lin0;              // image line of pole
    double ele0;              // image element of pole
    double scale;             // km per unit image coordinate
    double lon0;              // standard longitude
    double lat0;              // standard latitude
    double coscl;             // cosine of standard colatitude
    double tancl;             // tangent of standard colatitude
    double tancl2;            // tangent of standard colatitude/2
    double mxtheta;           // limit of angle from std. lon on proj surface
    private static double Erad = 6371.2;     // earth radius

    /**
     * Set up for the real math work.  Must pass in the int array
     * of the TANC nav 'codicil'.
     *
     * @param iparms  the nav block from the image file
     * @throws IllegalArgumentException
     *           if the nav block is not a TANC type or the parameters are
     *           bogus
     */
    public TANCnav (int[] iparms) 
        throws IllegalArgumentException
    {

        if (iparms[0] != TANC ) 
            throw new IllegalArgumentException("Invalid navigation type" + 
                                                iparms[0]);
        lin0  = iparms[1]/10000.;
        ele0  = iparms[2]/10000.;
        scale = iparms[3]/10000.;
        lat0  = iparms[4]/10000.;
        lon0  = iparms[5]/10000.;
        if (scale <= 0 || lat0 <= -90. || lat0 >= 90. ||
            lat0 == 0 || lon0 <= -180. || lon0 >= 180.)
                throw new IllegalArgumentException("Invalid nav parameters");

        lon0 = -lon0 * DEGREES_TO_RADIANS;   // convert to radians
        double colat0;
        if (lat0 < 0)
            colat0 = Math.PI/2. + lat0*DEGREES_TO_RADIANS;
        else
            colat0 = Math.PI/2. - lat0*DEGREES_TO_RADIANS;

        coscl = Math.cos(colat0);
        tancl = Math.tan(colat0);
        tancl2 = Math.tan(colat0/2.);
        mxtheta = Math.PI*coscl;
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
    public double[][] toLatLon(double[][] linele) 
    {

        double d_lin;
        double d_ele;
        double lon;
        double lat;
        double radius;
        double theta_rh;

        int number = linele[0].length;
        double[][] latlon = new double[2][number];

        // Convert array to Image coordinates for computations
        double[][] imglinele = areaCoordToImageCoord(linele);

        for (int point=0; point < number; point++) 
        {
            d_lin = imglinele[indexLine][point] - lin0;
            d_ele = imglinele[indexEle][point] - ele0;

            if ( Math.abs(d_lin) < 0.01 && Math.abs(d_ele) < 0.01)
            {
                radius = 0.0;
                theta_rh = 0.0;
            }
            else
            {
                double dx = scale*(d_lin);
                double dy = scale*(d_ele);
                radius = Math.sqrt(dx*dx + dy*dy);
                theta_rh = Math.atan2(dy, dx);
            }

            // convert theta_rh to angle FROM standard longitude (theta)
            // maintaining theta positive from positive x-axis.
            double theta;
            if (lat0 < 0.)
            {
                theta = (theta_rh <= 0.)
                            ? Math.PI - Math.abs(theta_rh)
                            : -1.*(Math.PI - Math.abs(theta_rh));
            }
            else theta = theta_rh;

            // Apply range checking on theta to determine if point is navigable
            if (theta <= -mxtheta || theta > mxtheta)
            {
                latlon[indexLat][point] = Double.NaN;
                latlon[indexLon][point] = Double.NaN;
            }
            else
            {
                lon = lon0 + theta/coscl;
                if (lon <= -Math.PI) lon = lon + 2.*Math.PI;
                if (lon > Math.PI)   lon = lon - 2.*Math.PI;
                double colat = 
                    2.* Math.atan( 
                        tancl2*Math.pow(radius/(Erad*tancl),1./coscl));

                // convert to degrees
                lon = lon/DEGREES_TO_RADIANS;
                lat = 90. - colat/DEGREES_TO_RADIANS;
                latlon[indexLat][point] = (lat0 < 0) ? -1*lat : lat;
                latlon[indexLon][point] = lon;
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
     
     *                    is an element.  These are in 'file' coordinates
     *                    (not "image" coordinates);
     */
    public double[][] toLinEle(double[][] latlon) 
    {
        double lon;
        double lat;

        int number = latlon[0].length;
        double[][] linele = new double[2][number];

        for (int point=0; point < number; point++) 
        {
            lat = latlon[indexLat][point];
            lon = latlon[indexLon][point];
            if (lat <= -90. || lat >= 90. || lon <= -360. ||
                lon > 360.)
            {
                linele[indexLine][point] = Double.NaN;
                linele[indexEle][point]  = Double.NaN;
            }
            else
            {
                double colat = 
                   (lat0 < 0) 
                       ? Math.PI/2. + DEGREES_TO_RADIANS*lat
                       : Math.PI/2. - DEGREES_TO_RADIANS*lat;
                double in_lon = DEGREES_TO_RADIANS*lon;
                // map longitude into range -Pi to Pi
                if (in_lon <= -Math.PI) in_lon = in_lon + 2.*Math.PI;
                if (in_lon > Math.PI)   in_lon = in_lon - 2.*Math.PI;

              // Now trap opposite Pole. Though a physically possible latitude,
              // tan(colat/2) -> infinity there so it is not navigable
                if (colat == Math.PI)
                {
                    linele[indexLine][point] = Double.NaN;
                    linele[indexEle][point]  = Double.NaN;
                }
                else
                {
                    double radius = 
                        Erad * tancl * 
                            Math.pow(Math.tan(colat/2.)/tancl2, coscl);
                    double theta = in_lon-lon0;
                    if (theta <= -Math.PI) theta = theta + 2*Math.PI;
                    if (theta > Math.PI)   theta = theta - 2*Math.PI;
                    theta = coscl * theta;

               // Compute line and element, check for northern or southern
               // hemisphere projection cone.  Put north pole on top of frame,
               // south pole on bottom.  Maintain right-handed coordinate system
               // by measuring theta positive from the positive x-axis.
                    if (lat0 < 0) theta = Math.PI + theta;
                    linele[indexLine][point] = 
                        lin0 + radius*Math.cos(theta)/scale;
                    linele[indexEle][point]  = 
                        ele0 + radius*Math.sin(theta)/scale;
                }
            }
        } // end point loop

        // Return in 'File' coordinates
        return imageCoordToAreaCoord(linele, linele);
    }
}
