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
package com.raytheon.viz.radar.frame;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * A cache of the most recently viewed frame time. The cache is keyed off the
 * volume scan number and the primary elevation angle and will return the most
 * recently viewed frame time for the key.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * May 13, 2015  4461     bsteffen    Initial creation
 * Sep 30, 2015  4902     bsteffen    Cache all visited frames.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class RadarFrameCache {

    /**
     * Each entry in this map is ordered so that the most recently used are
     * first.
     */
    private Map<Key, List<RadarDataTime>> cache = new HashMap<>();

    /**
     * This should be called whenever a frame is displayed to update the cache
     */
    public synchronized void setLastFrameTime(DataTime time) {
        if (time instanceof RadarDataTime) {
            RadarDataTime rtime = (RadarDataTime) time;
            Key key = new Key(rtime);
            List<RadarDataTime> list = cache.get(key);
            if(list == null){
                list = new ArrayList<>(1);
                cache.put(key, list);
            }
            list.remove(rtime);
            list.add(0, rtime);
        }
    }

    /**
     * Get the most recent displayed frame from the . If no time is in the cache
     * then the time is returned.
     */
    public synchronized RadarDataTime getLastFrameTime(
            Collection<RadarDataTime> options) {
        Key key = new Key(options.iterator().next());
        List<RadarDataTime> list = cache.get(key);
        if (list == null) {
            return null;
        }
        for (RadarDataTime time : list) {
            if (options.contains(time)) {
                return time;
            }
        }
        return null;
    }

    public synchronized void clear() {
        cache.clear();
    }

    private static class Key {

        private final int volumeScanNumber;

        private final double primaryElevationAngle;

        private final int hashCode;

        public Key(RadarDataTime time) {
            this.volumeScanNumber = time.getVolumeScanNumber();
            this.primaryElevationAngle = time.getLevelValue();
            long temp = Double.doubleToLongBits(primaryElevationAngle);
            int hashCode = 31 + (int) (temp ^ (temp >>> 32));
            hashCode = 31 * hashCode + volumeScanNumber;
            this.hashCode = hashCode;
        }

        @Override
        public int hashCode() {
            return hashCode;
        }

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null)
                return false;
            if (getClass() != obj.getClass())
                return false;
            Key other = (Key) obj;
            if (Double.doubleToLongBits(primaryElevationAngle) != Double
                    .doubleToLongBits(other.primaryElevationAngle))
                return false;
            if (volumeScanNumber != other.volumeScanNumber)
                return false;
            return true;
        }

    }
}
