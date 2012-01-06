/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.common.dataplugin.fog.analysis;

import com.vividsolutions.jts.geom.Coordinate;

public class FogSatCalculator {

    public static double REARTH = 6356752.314;
    private double satlon = 0.0;
    private double lon = 0.0;
    private double lat = 0.0;
    public Coordinate location = new Coordinate();
    public Coordinate u_sat_vec = new Coordinate();
    public boolean calFlag = false;
    public float satHeight = 0.0f;

    public FogSatCalculator() {

    }

    public void setLocation(Coordinate location) {
        this.location = location;
    }
    
    public void setSatHeight(float satHeight) {
        this.satHeight = satHeight;
    }
    
    public void setSatLon(double satlon) {
        this.satlon = satlon;
    }
    
    /**
     * Gest the 3D vector for the satellite to the location
     * @return
     */
    public Coordinate satV3D()
    {
        if (!constructFlag()) {
            calculate();
        }
        return u_sat_vec;
    }

    // Name: ConstructFlag()
    // Description:
    //        Construct a flag to indicate whether the elev and azm needed to be 
    //        recalculated.
    //        If any of the component of the flag (_lat,_lon,_satlon) changes, 
    //        a recalculation is neeeded.
    // Input Arguments:
    //        None
    // Output Arguments:  
    //        return std::string: string flag to indicate what location
    //                            is currently used.
    // History:
    //      April 2004   Qin Zeng (GDMB/MDL)  -- created   
    //-----------------------------------------------------------------------------

    public boolean constructFlag() {
        return calFlag;
    }

    //-private---------------------------------------------------------------------
    //
    // TYPE: private member function 
    // Name: Calculate()  
    // Description:
    //        Calculate the azimuth and the elevation of the satellite based on 
    //        3D geometry relationship between the satellite and the local point.
    //        Assumptions are that Earth is a sphere and the satellite is right 
    //        above equator ie. latitude is 0.0 .
    //        Based on the spherical geometry knowledge.
    //        For more info:
    //        http://mathworld.wolfram.com/topics/Spheres.html
    // Input Arguments:
    //        None
    // Output Arguments:  
    //        None
    // History:
    //      April 2004   Qin Zeng (GDMB/MDL)  -- created
    //      Sep.  2004   Qi Zhu   (RSIS/MDL)  Replaced calling setupinfo->satelliteHeight()
    //                                           to get satellite height with using
    //                                           data member _satHeight.   
    //-----------------------------------------------------------------------------
    public void calculate()
    {
        double longdiffr = Math.toRadians(lon-satlon);
        double latr = Math.toRadians(lat);

        //SetupAccessor * setupinfo = SetupAccessor::getInstance();
        //float SAT_EARTH_DISTANCE =  setupinfo->satelliteHeight();
        float SAT_EARTH_DISTANCE = satHeight;

        float  r1 = (float) (1+SAT_EARTH_DISTANCE/(REARTH/1000)); 
        double v1 = r1*Math.cos(latr)* Math.cos(longdiffr)-1;
        double v2 = r1*Math.sqrt(1-Math.cos(latr)* Math.cos(latr)* Math.cos(longdiffr)* Math.cos(longdiffr));
        u_sat_vec.z = v1/Math.sqrt(v1*v1+v2*v2);
        float cos_elev = (float) (v2/Math.sqrt(v1*v1+v2*v2));

        if (latr == 0)
        {
            if (longdiffr > 0)
            {
                u_sat_vec.x = -cos_elev;
                u_sat_vec.y = 0;
            }
            else
            {
                u_sat_vec.x = cos_elev;
                u_sat_vec.y = 0;
            }
            calFlag = true;
            
            return;
        }

        float tan_azm = (float) (Math.tan(longdiffr)/Math.sin(latr));

        if (latr<0)  // north quadrant 
        {
            if (tan_azm > 0)
            {
                float cos_azm = (float) (1/(Math.sqrt(1+tan_azm*tan_azm)));
                u_sat_vec.y = cos_elev * cos_azm;
                float sin_azm = (float) Math.sqrt(1-cos_azm*cos_azm);
                u_sat_vec.x = cos_elev * sin_azm;
            }
            else
            {
                float cos_azm =(float) (-1/(Math.sqrt(1+tan_azm*tan_azm)));
                u_sat_vec.y = cos_elev * cos_azm;
                float sin_azm = (float) Math.sqrt(1-cos_azm*cos_azm);
                u_sat_vec.x = cos_elev * sin_azm;
            }
        }
        else  // either south quadrant;
        {
            if (tan_azm > 0)
            {
                float cos_azm = (float) (-1/(Math.sqrt(1+tan_azm*tan_azm)));
                u_sat_vec.y = cos_elev * cos_azm;
                float sin_azm = (float) (-1*(Math.sqrt(1-cos_azm*cos_azm)));
                u_sat_vec.x = cos_elev * sin_azm;
            }
            else
            {
                float cos_azm = (float) (1/(Math.sqrt(1+tan_azm*tan_azm)));
                u_sat_vec.y = cos_elev * cos_azm;
                float sin_azm = (float) (-1*(Math.sqrt(1-cos_azm*cos_azm)));
                u_sat_vec.x = cos_elev * sin_azm;
            }

        }
        calFlag = true;
    }

}
