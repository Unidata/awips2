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
package com.raytheon.viz.grid.gridcache;

import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;

/**
 * Time node of the cache tree.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 13, 2009 3579       mpduff      Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public class TimeNode extends CacheNode {
    private long referenceTime;
    
    private Map<Integer, DataCacheCube> dataCubeMap = new HashMap<Integer, DataCacheCube>();

    /**
     * @return the referenceTime
     */
    public long getReferenceTime() {
        return referenceTime;
    }

    /**
     * @param referenceTime the referenceTime to set
     */
    public void setReferenceTime(long referenceTime) {
        this.referenceTime = referenceTime;
    }
    
    /**
     * Add data cube to the time node.
     * @param cube
     */
    public void addDataCube(DataCacheCube cube) {
        referenceTime = cube.getMetadata().getRefTime();
        dataCubeMap.put(cube.getMetadata().getFcstHr(), cube);
    }
    
    /**
     * Get the forecast times for each cube.
     * @return
     */
    public int[] getFcstTimes() {
        Set<Integer> rtimes = dataCubeMap.keySet();
        int[] timeList = new int[rtimes.size()];

        // create a list to sort the times
        Iterator<Integer> iter = rtimes.iterator();
        
        int index = 0;
        while (iter.hasNext()) {
            timeList[index] = iter.next();
            index++;
        }
        
        Arrays.sort(timeList);
        
        return timeList;
    }
    
    /**
     * Get a data cube.
     * 
     * @param fcstTime
     *      The forecast time of the cube
     * @return
     *      The cube
     */
    public DataCacheCube getCube(int fcstTime) {
        return dataCubeMap.get(fcstTime);
    }
    
    /**
     * Remove a data cube.
     * 
     * @param fcstTime
     *      Forecast time of the cube to remove
     */
    public void removeCube(int fcstTime) {
        dataCubeMap.remove(fcstTime);
    }

    /* (non-Javadoc)
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        StringBuilder buf = new StringBuilder();
        
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        c.setTimeInMillis(getReferenceTime());
        
        buf.append("**************");
        buf.append("TimeNode\n");
        buf.append("Ref time = " + c.get((Calendar.MONTH) + 1) + "/" + c.get(Calendar.DAY_OF_MONTH) + " ");
        buf.append(c.get(Calendar.HOUR_OF_DAY) + "\n");
        buf.append("Fcst times = ");
        int[] ft = getFcstTimes();
        for (long l: ft) {
            buf.append(l + " ");
        }
        buf.append("\n");
        
        return buf.toString();
    }
}
