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
package com.raytheon.viz.satellite.tileset;

import com.raytheon.uf.common.colormap.image.ColorMapData;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.tile.RecordTileSetRenderable;
import com.raytheon.uf.viz.core.tile.Tile;

/**
 * Satellite tile set renderable, uses {@link SatDataRetriever} for {@link Tile}
 * data retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 19, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SatTileSetRenderable extends RecordTileSetRenderable {

    private final AbstractVizResource<?, ?> resource;

    /**
     * Create satellite tile set renderable
     * 
     * @param resource
     * @param record
     * @param signed
     */
    public SatTileSetRenderable(AbstractVizResource<?, ?> resource,
            SatelliteRecord record) {
        // Total levels = Number of interpolation levels + base level
        super(resource, record, record.getGridGeometry(), record
                .getInterpolationLevels() + 1);
        this.resource = resource;
    }

    @Override
    protected ColorMapData retrieveRecordData(Tile tile) {
        return new SatDataRetriever((SatelliteRecord) record, tile.tileLevel,
                tile.getRectangle()).getColorMapData();
    }

    @Override
    protected void issueRefresh(IGraphicsTarget target) {
        super.issueRefresh(target);
        resource.issueRefresh();
    }

    public SatelliteRecord getSatelliteRecord() {
        return (SatelliteRecord) record;
    }
}
