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
package com.raytheon.uf.edex.decodertools.core;

import java.util.HashMap;


/**
 *
 *
 *
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20071226            384 jkorman     Initial Coding.
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class PlatformLocationProxy
{
    private static HashMap<String,BasePoint> myLOCATIONS = new HashMap<String,BasePoint>();
    
    /**
     * 
     * @param aType
     */
    private PlatformLocationProxy() {
        // nothing
    }

    public static BasePoint lookup(String navaid,String [] aNetworkTypeList) {
        BasePoint retValue = null;
        if (myLOCATIONS == null) { // normal operation

        }
        else {
            // development/unittest option
            if(myLOCATIONS.containsKey(navaid)) {
                retValue = myLOCATIONS.get(navaid);
            }
        }
               
        return retValue;
    } // lookup()
    
    /**
     * Set the internal waypoint table for testing. Note that this method may
     * be used  to "clear" the test table by passing a null reference.
     * @param navaidTable A HashMap containing a Waypoint name to LatLonPoint
     * mapping.
     */
    public static void setTable(HashMap<String,BasePoint> aLocationMap) {
        myLOCATIONS = aLocationMap;
    } // setTable()
    
}
