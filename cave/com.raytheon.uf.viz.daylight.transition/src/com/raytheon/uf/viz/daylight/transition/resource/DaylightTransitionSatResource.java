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
package com.raytheon.uf.viz.daylight.transition.resource;

import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.satellite.SatelliteRecord;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IRenderable;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.daylight.transition.tileset.DaylightTransitionTileSetRenderable;
import com.raytheon.viz.satellite.rsc.SatResource;
import com.raytheon.viz.satellite.tileset.SatRenderable;
import com.raytheon.viz.satellite.tileset.SatTileSetRenderable;

/**
 * 
 * An extension of the {@link SatResource} that substitutes a
 * {@link DaylightTransitionTileSetRenderable} in order to block out data when
 * there is no sunlight.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 28, 2015  4633     bsteffen    Initial creation
 * Oct 12, 2015  4937     bsteffen    Move SatRenderable to new class, extend it here.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DaylightTransitionSatResource extends SatResource {

    private final DaylightTransitionSatResourceData resourceData;

    public DaylightTransitionSatResource(
            DaylightTransitionSatResourceData data, LoadProperties props) {
        super(data, props);
        this.resourceData = data;
    }


    @Override
    protected IRenderable constructRenderable(DataTime time,
            List<PluginDataObject> records) throws VizException {
        SatRenderable renderable = new DaylightTransitionSatRenderable(time);
        updateRenderable(renderable, records.toArray(new PluginDataObject[0]));
        renderable.project();
        return renderable;
    }

    private class DaylightTransitionSatRenderable extends SatRenderable {

        public DaylightTransitionSatRenderable(DataTime renderableTime) {
            super(DaylightTransitionSatResource.this, renderableTime);
        }

        @Override
        protected SatTileSetRenderable createTileSet(SatelliteRecord record) {
            SatTileSetRenderable tileSet = new DaylightTransitionTileSetRenderable(
                    DaylightTransitionSatResource.this, record,
                    resourceData.getSunDelta());
            tileSet.project(descriptor.getGridGeometry());
            return tileSet;
        }
    }

}
