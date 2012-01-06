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

import java.util.Calendar;

import com.raytheon.uf.common.time.DataTime;
import com.vividsolutions.jts.geom.Coordinate;

public class FogSolarCalculator {
    
    public boolean visNeeded = true;
    public boolean calFlag = false;
    public DataTime dataTime;
    public Coordinate location;
    public float sinSolarElevation = 0;
    public float solarDeclinationAngle = 0;
    public float cos_azm = 0.0f;
    public float cos_zenith = 0.0f;
    public float sha = 0.0f;
    public Coordinate u_s_vec = new Coordinate();
    
    // time realted stuff
    int minutes = 0;
    int seconds = 0;
    int hour = 0;
    int day = 0;
    int month = 0;
    int year = 0;
    int yday = 0;
    
    public FogSolarCalculator() {
        
    }
    
    /**
     * Sets the time related stuff
     * @param dataTime
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
        
        minutes = dataTime.getRefTimeAsCalendar().get(Calendar.MINUTE);
        seconds = dataTime.getRefTimeAsCalendar().get(Calendar.SECOND);
        hour = dataTime.getRefTimeAsCalendar().get(Calendar.HOUR_OF_DAY);
        day = dataTime.getRefTimeAsCalendar().get(Calendar.DAY_OF_MONTH);
        month = dataTime.getRefTimeAsCalendar().get(Calendar.MONTH)-1;
        year = dataTime.getRefTimeAsCalendar().get(Calendar.YEAR)-1900;
        yday = dataTime.getRefTimeAsCalendar().get(Calendar.DAY_OF_YEAR);
    }
    
    /**
     * Set the location coordinate
     * @param location
     */
    public void setLocation(Coordinate location) {
        this.location = location;
    }
        
    /**
     * Is the vis satellite record needed
     * @return
     */
    public boolean visNeeded() {
        return visNeeded;
    }
       
    /**
     * Indicates a need for recalculation
     * @return
     */
    public boolean constructFlag() {
        return calFlag;
    }
    
    
    //-public---------------------------------------------------------------------
    //
    // TYPE: Public member function 
    // Name: float Sin_SolarElevation()  
    // Description:
    //         Check if a recalculation is needed.
    //         If so, recalculate it.
    //         Then return sin value of the solar elevation
    // Input Arguments:
    //           None
    // Output Arguments:  
    //           return float: COSINE value of zenith
    // History:
    //         April 2004   Qin Zeng (GDMB/MDL)  -- created   
    //-----------------------------------------------------------------------------

    public float getSinSolarElevation() {
        if (!constructFlag()) {
            calculate();
        }
        
        return cos_zenith;
    }
    
    //-public---------------------------------------------------------------------
    //
    // TYPE: Public member function 
    // Name: SolarV3D()  
    // Description:
    //         Check if a recalculation is needed.
    //         If so, recalculate it.
    //         Then return the EARTH-SUN vector from local point
    // Input Arguments:
    //           None
    // Output Arguments:  
    //           return Vector3D:the EARTH-SUN vector from local point
        
    // History:
    //         April 2004   Qin Zeng (GDMB/MDL)  -- created   
    //         Dec 13, 2009 D Hladky   converted to Java
    //-----------------------------------------------------------------------------

    public Coordinate solarV3D() {
        if (!constructFlag()) {
            calculate();
        }
        
        return u_s_vec;
    }
    
    //-private---------------------------------------------------------------------
    //
    // TYPE: private member function 
    // Name: Calculate()  
    // Description:
    //           Calculate the azimuth and the elevation of the Sun from the view of 
    //           a local point on the Earth
    //           This calculation is based on a NOAA website's  document. This is the
    //           lower accuracy version.
    //Input: None
    //Output:None
    //
    //           Note:
    //           http://www.srrb.noaa.gov/hightlights/sunrise/calcdetails.html
    //           So no explanation for the astronomical magic numbers.
    // History:
    //         April 2004   Qin Zeng (GDMB/MDL)  -- created  
    //         Dec 2009     D Hladky  ported to Java AWIPS II
    //-----------------------------------------------------------------------------

    public void calculate() {
        
        float latr = (float) Math.toRadians(location.y);
        float lonr = (float) Math.toRadians(location.x);
        int time_zone;
        
        if (lonr > 0) {
            time_zone = (int) ((location.x -7.5)/15 + 1);
        }
        else {
            time_zone = (int) ((location.x + 7.5) /15 - 1);
        }
                
        float gamma = (float) (2 * Math.PI / 365 * (yday + (hour - 12)/24.0 ));
        // calculate equation time
        float eqtime = (float) (229.18 * (0.000075 + 0.001868* Math.cos(gamma)
                       -0.032077*Math.sin(gamma) -0.014615 * Math.cos(2* gamma)
               - 0.040849*Math.sin(2*gamma)));
        
        // calculate solar declination angle
        solarDeclinationAngle = (float) (0.006918 -0.399912* Math.cos(gamma) + 0.070257* Math.sin(gamma)
                - 0.006758*Math.cos(2* gamma) + 0.000907*Math.sin(2*gamma)
            -0.002697*Math.cos(3*gamma)+ 0.00148*Math.sin(3*gamma));
        
        float time_offset = (float) (eqtime +4*location.x - 60 *time_zone);
        float tst = (float) (hour *60 + minutes + seconds/60.0 + time_offset);
        
        sha = (float) Math.toRadians((tst/4.0)-180);
        
        float sin_radlat = (float) Math.sin(latr);
        float sin_decl   = (float) Math.sin(solarDeclinationAngle);
        float cos_radlat = (float) Math.cos(latr);
        float cos_decl   = (float) Math.cos(solarDeclinationAngle);
        float cos_sha    = (float) Math.cos(sha);
        
        cos_zenith = sin_radlat* sin_decl + cos_radlat * cos_decl * cos_sha;
        
        if (cos_zenith > 1) {
            cos_zenith = 1;
        }
        if (cos_zenith < -1) {
            cos_zenith = -1;
        }
        
        u_s_vec.z = cos_zenith;
        
        float sin_zenith = (float) Math.sqrt(1-cos_zenith * cos_zenith);
        
        //  Calculate azimuth of the sun
        if (sin_zenith != 0) {
            cos_azm = Math.abs((sin_radlat* cos_zenith - sin_decl) 
                             / (cos_radlat * sin_zenith));
        }
        else {
            cos_azm = 0;
        }

        if (cos_azm > 1)  cos_azm = 1;
        if (cos_azm < -1) cos_azm =-1;
        
        if (solarDeclinationAngle == latr) cos_azm =0;
        if (solarDeclinationAngle < latr)
        cos_azm = -cos_azm;
        
        float sin_azm;  
        
        if ( sha > 0) {
            sin_azm = (float) -Math.sqrt(1-cos_azm*cos_azm);
        }
        else {
            sin_azm = (float) Math.sqrt(1-cos_azm*cos_azm);
        }
        
        u_s_vec.x = sin_zenith * sin_azm;
        u_s_vec.y = sin_zenith * cos_azm;

        calFlag = true;
    }
}
