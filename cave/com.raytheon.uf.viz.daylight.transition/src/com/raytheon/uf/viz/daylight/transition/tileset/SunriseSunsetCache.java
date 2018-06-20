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
package com.raytheon.uf.viz.daylight.transition.tileset;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import org.geotools.coverage.grid.GridCoordinates2D;
import org.geotools.coverage.grid.GridEnvelope2D;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.referencing.CRS;
import org.geotools.referencing.crs.DefaultGeographicCRS;
import org.opengis.geometry.DirectPosition;
import org.opengis.referencing.operation.MathTransform;

import com.raytheon.uf.common.numeric.buffer.ShortBufferWrapper;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.viz.awipstools.common.SunriseSunsetCalculator;

/**
 * 
 * A cache of sunrise/sunset times to speed up retrieving multiple frames of
 * satellite data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SunriseSunsetCache {

    private static final Map<Key, Reference<SunriseSunsetCache>> cache = new HashMap<>();

    private final ShortBufferWrapper sunRise;

    private final ShortBufferWrapper sunSet;

    private SunriseSunsetCache(GridGeometry2D gg, Calendar time)
            throws Exception {
        MathTransform mt = CRS.findMathTransform(
                gg.getCoordinateReferenceSystem(), DefaultGeographicCRS.WGS84);
        GridEnvelope2D range = gg.getGridRange2D();
        int width = range.width;
        int height = range.height;
        sunRise = new ShortBufferWrapper(width, height);
        sunSet = new ShortBufferWrapper(width, height);
        int year = time.get(Calendar.YEAR);
        int month = time.get(Calendar.MONTH) + 1;
        int day = time.get(Calendar.DAY_OF_MONTH);
        for (int i = 0; i < width; i += 1) {
            for (int j = 0; j < height; j += 1) {
                GridCoordinates2D gc = new GridCoordinates2D(range.x + i,
                        range.y + j);
                DirectPosition dp = gg.gridToWorld(gc);
                mt.transform(dp, dp);
                double longitude = dp.getOrdinate(0);
                double latitude = dp.getOrdinate(1);
                SunriseSunsetCalculator ssc = new SunriseSunsetCalculator();
                ssc.calculate(longitude, latitude, "GMT", year, month, day);
                int sunRise = ssc.getSunriseHour() * TimeUtil.MINUTES_PER_HOUR
                        + ssc.getSunriseMinute();
                int sunSet = ssc.getSunsetHour() * TimeUtil.MINUTES_PER_HOUR
                        + ssc.getSunsetMinute();
                this.sunRise.setDataValue(sunRise, i, j);
                this.sunSet.setDataValue(sunSet, i, j);
            }
        }
    }

    public boolean isDay(int x, int y, Calendar time, int sunDelta) {
        int minutes = time.get(Calendar.HOUR_OF_DAY)
                * TimeUtil.MINUTES_PER_HOUR
                + time.get(Calendar.MINUTE);
        int sunRise = (int) this.sunRise.getDataValue(x, y);
        int sunSet = (int) this.sunSet.getDataValue(x, y);
        sunRise += sunDelta;
        if (sunRise > TimeUtil.MINUTES_PER_DAY) {
            sunRise -= TimeUtil.MINUTES_PER_DAY;
        }
        sunSet -= sunDelta;
        if (sunSet < 0) {
            sunSet += TimeUtil.MINUTES_PER_DAY;
        }
        if (sunRise < sunSet && (minutes < sunRise || minutes > sunSet)) {
            return false;
        } else if (sunSet < sunRise && (minutes > sunSet && minutes < sunRise)) {
            return false;
        } else {
            return true;
        }
    }

    public static synchronized SunriseSunsetCache getCache(GridGeometry2D gg,
            Calendar time) throws Exception {
        Key k = new Key(gg, time);
        SunriseSunsetCache c = null;
        Reference<SunriseSunsetCache> r = cache.get(k);
        if (r != null) {
            c = r.get();
        }
        if (c == null) {
            c = new SunriseSunsetCache(gg, time);
            cache.put(k, new SoftReference<>(c));
        }
        return c;

    }

    private static class Key {

        public final GridGeometry2D gg;

        public final int year;

        public final int month;

        public final int day;

        public Key(GridGeometry2D gg, Calendar time) {
            this.gg = gg;
            year = time.get(Calendar.YEAR);
            month = time.get(Calendar.MONTH) + 1;
            day = time.get(Calendar.DAY_OF_MONTH);
        }

        @Override
        public int hashCode() {
            final int prime = 31;
            int result = 1;
            result = prime * result + day;
            result = prime * result + ((gg == null) ? 0 : gg.hashCode());
            result = prime * result + month;
            result = prime * result + year;
            return result;
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
            if (day != other.day)
                return false;
            if (gg == null) {
                if (other.gg != null)
                    return false;
            } else if (!gg.equals(other.gg))
                return false;
            if (month != other.month)
                return false;
            if (year != other.year)
                return false;
            return true;
        }

    }
}
