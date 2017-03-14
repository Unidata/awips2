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

import com.raytheon.uf.common.dataplugin.fog.FogRecord;
import com.vividsolutions.jts.geom.Coordinate;

public class FogMonitorUtils {
    
    //-free function---------------------------------------------------------------
    // NAME: CosAngleBetween
    //
    // TYPE: free function
    //  
    // Descript:
    //            Given two 3D vectors, this function calculate the COSINE value of 
    //            the angle between the vectors.
    //Input Arguments:
    //                vector1: one of the 3D vector
    //                vector2: the other 3D vector
    //                isUnit : if the vector is Unit Vector ie. |vector|=1 
    //Output Argutments:
    //        Return:
    //        float: COSINE value of the angle between the two vectors. 
    // History:
    //      May  2004    Qin Zeng (GDMB/MDL)  -- created  
    //      Dec 2009     D Hladky               translated to Java
    //-----------------------------------------------------------------------------

    public static float CosAngleBetween(Coordinate vector1, Coordinate vector2) {

        float norm = 1;
        float norm1;
        float norm2;

        norm1 = (float) Math.sqrt(vector1.x*vector1.x
                + vector1.y*vector1.y
                + vector1.z*vector1.z);
        norm2 = (float) Math.sqrt(vector2.x*vector2.x
                + vector2.y*vector2.y
                + vector2.z*vector2.z);
        norm = norm1 * norm2;

        return (float)((vector1.x*vector2.x + vector1.y*vector2.y + vector1.z*vector2.z)/norm);
    }


    //-free function---------------------------------------------------------------
    // NAME: CosAzimuth
    // TYPE: free function
    //  
    // Descript:
    //            Given a 3D vectors, this function calculate the COSINE value of 
    //            the Azimuth angle of the vector .
    //            Here we define Azimuth angle as the angle starting from the north
    //            direction.
    // Input Arguments:
    //            vector1: the 3D vector
    // Output Arguments:          
    //       Return:
    //                float: COSINE value of the azimuth angle . 
    // History:
    //      May  2004    Qin Zeng (GDMB/MDL)  -- created   
    //-----------------------------------------------------------------------------
    public static float CosAzimuth(Coordinate vector1) {

        float normXY = (float) Math.sqrt(vector1.x*vector1.x + vector1.y*vector1.y);
        return (float) (vector1.y/normXY);
    }


    //-free function---------------------------------------------------------------
    // NAME: SinAzimuth
    // TYPE: free function
    //  
    // Descript:
    //            Given a 3D vectors, this function calculate the SINE value of 
    //            the Azimuth angle of the vector .
    //            Here we define Azimuth angle as the angle starting from the north
    //            direction.
    // Input Arguments:
    //            vector1: the 3D vector
    // Output Arguments:          
    // Return:
    //                float: SINE value of the azimuth angle . 
    // History:
    //      May  2004    Qin Zeng (GDMB/MDL)  -- created   
    //-----------------------------------------------------------------------------

    public static float SinAzimuth(Coordinate vector1) {

        float normXY = (float) Math.sqrt(vector1.x*vector1.x + vector1.y*vector1.y);
        return (float) (vector1.x/normXY);
    }


    //-free function---------------------------------------------------------------
    // NAME: CosZentih
    // TYPE: free function
    //  
    // Descript:
    //            Given a 3D vectors, this function calculate the COSINE value of 
    //            the Zenith angle of the vector .
    // Input Arguments:
    //            vector1: the 3D vector
    //            isUnit : if the vector is Unit Vector ie. |vector|=1 
    // Output Arguments:
    //            Return:
    //                float: COSINE value of the Zenith angle . 
    // History:
    //      May  2004    Qin Zeng (GDMB/MDL)  -- created   
    //-----------------------------------------------------------------------------

    public static float CosZenith(Coordinate vector1) {

        float norm = (float) Math.sqrt(vector1.x*vector1.x
                + vector1.y*vector1.y
                + vector1.z*vector1.z);
        vector1.z=vector1.z/norm;

        return (float) vector1.z;
    }

    // the same as CosZenith
    public static float SinElev(Coordinate vector1) {
        return CosZenith(vector1);
    }


    public static float CosElev(Coordinate vector1) {
        // Elevation angle ranges from -90 to +90
        // so CosElev is always greater than or equal to 0;
        return (float) Math.sqrt(1-SinElev(vector1)*SinElev(vector1));
    }
    
    
    //---------------------------------------------------------------------------
    //Name: count2temp 
    //Type: public member function
    //
    //Description:
    //          Retrieve the temperature based on channel and brightness
    //          Reference:
    //          http://laps.fsl.noaa.gov/birk/awipsgoes/goesinfo.html
    //          "AWIPS-GOES Data Utilization Page"
    //
    //Input  Argument: 
    //          count_value: count value which represent brightness
    //                       ranging from 0 to 255
    //          channel:     Which channel to be calculated. (IR3_9 or IR10_7)     
    //Output Argument: 
    //         Return the temperature according the count and channel.
    //History:
    //           March 2004 ------ Qin Zeng(GDMB/MDL) created
    //           Converted to Java D  Hladky 12 Dec 09
    //--------------------------------------------------------------------------
  public static float count2temp(int count_value, FogRecord.CHANNEL channel)
  {
      if (count_value == 255) {
          return 0;
      }
      
      if (channel == FogRecord.CHANNEL.IR3_9) {
          if (count_value > 216) {
              return 0;
          }
          else if(count_value > 183) {
              return (float)421.7- count_value;
          }
          else {
              return (float)(660.4- count_value)/2;
          }
      }
      
      else if (channel == FogRecord.CHANNEL.IR10_7) {
          if (count_value > 180) {
              return (float)420.0 - count_value;
          }
          else if (count_value > 0) {
              return (float) ((660.0 - count_value)/2.0);
          }
      }
      // default
      return (float) -999.0;  
  }

}
