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

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.viz.core.tile.Tile;
import com.raytheon.viz.satellite.rsc.SatResource;
import com.raytheon.viz.satellite.tileset.SatTileSetRenderable;

/**
 * 
 * An extension of the {@link SatTileSetRenderable} that uses a
 * {@link DaylightTransitionDataRetriever} in order to block out data when there
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
public class DaylightTransitionTileSetRenderable extends SatTileSetRenderable {

    private final SatelliteRecord record;

    private final int sunDelta;

    public DaylightTransitionTileSetRenderable(SatResource resource,
            SatelliteRecord record, int sunDelta) {
        super(resource, record);
        this.record = record;
        this.sunDelta = sunDelta;
    }

    @Override
    protected ColorMapData retrieveRecordData(Tile tile) {
        double noDataValue = colormapping.getColorMapParameters()
                .getNoDataValue();
        return new DaylightTransitionDataRetriever(record, tile.tileLevel,
                tile.tileGeometry, sunDelta, noDataValue).getColorMapData();
    }

}
