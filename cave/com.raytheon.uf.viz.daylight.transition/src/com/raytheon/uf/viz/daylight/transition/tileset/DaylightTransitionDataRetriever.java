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

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.ShortBuffer;
import java.util.Calendar;

import org.geotools.coverage.grid.GridGeometry2D;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.viz.satellite.tileset.SatDataRetriever;

/**
 * 
 * An extension of the {@link SatDataRetriever} that blocks out data when there
 * is no sunlight.
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
public class DaylightTransitionDataRetriever extends SatDataRetriever {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DaylightTransitionDataRetriever.class);

    private final GridGeometry2D tileGeometry;

    private final int sunDelta;

    private final double nightTimeFillValue;

    public DaylightTransitionDataRetriever(SatelliteRecord record, int level,
            GridGeometry2D tileGeometry, int sunDelta,
            double nightTimeFillValue) {
        super(record, level, tileGeometry.getGridRange2D());
        this.tileGeometry = tileGeometry;
        this.sunDelta = sunDelta;
        this.nightTimeFillValue = nightTimeFillValue;
    }

    @Override
    public ColorMapData getColorMapData() {
        ColorMapData data = super.getColorMapData();
        Calendar time = record.getDataTime().getRefTimeAsCalendar();
        try {
            SunriseSunsetCache cache = SunriseSunsetCache.getCache(
                    tileGeometry, time);
            Buffer buffer = data.getBuffer();
            for (int i = 0; i < datasetBounds.width; i += 1) {
                for (int j = 0; j < datasetBounds.height; j += 1) {
                    if (!cache.isDay(i, j, time, sunDelta)) {
                        int index = j * datasetBounds.width + i;
                        if (buffer instanceof ByteBuffer) {
                            ((ByteBuffer) buffer).put(index,
                                    (byte) nightTimeFillValue);
                        } else if (buffer instanceof ShortBuffer) {
                            ((ShortBuffer) buffer).put(index,
                                    (short) nightTimeFillValue);
                        }
                    }
                }
            }
        } catch (Exception e) {
            statusHandler.error("Unable to determine daylight transition", e);
        }
        return data;

    }

}
