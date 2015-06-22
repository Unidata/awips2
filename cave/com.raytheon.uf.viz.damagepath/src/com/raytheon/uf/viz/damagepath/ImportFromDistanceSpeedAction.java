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
package com.raytheon.uf.viz.damagepath;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.EditableCapability;
import com.raytheon.viz.awipstools.ui.layer.DistanceSpeedLayer;
import com.raytheon.viz.ui.cmenu.AbstractRightClickAction;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Action to create a damage path from a DistanceSpeedLayer.
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 23, 2015 3977       nabowle     Initial creation
 * Jun 01, 2015 3975       dgilling    Update for DamageLayer changes for
 *                                     multiple polygon support.
 * Jun 18, 2015 4354       dgilling    Update isEnabled to consider editable
 *                                     capability.
 * Jun 19, 2015 3977       nabowle     Specify Tornado Path.
 *
 * </pre>
 *
 * @author nabowle
 * @version 1.0
 */

public class ImportFromDistanceSpeedAction extends AbstractRightClickAction {

    protected static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ImportFromDistanceSpeedAction.class);

    public ImportFromDistanceSpeedAction() {
        super("New Tornado Path from Distance Speed Tool");
    }

    @Override
    public void run() {
        VizApp.runSync(new Runnable() {
            @Override
            public void run() {
                DamagePathLayer<?> layer = (DamagePathLayer<?>) getSelectedRsc();
                DistanceSpeedLayer dsLayer = findImportLayer(layer);

                // The Distance Speed tool has not been loaded.
                if (dsLayer == null) {
                    return;
                }

                Polygon polygon = DamagePathUtils.estimateTornadoDamagePath(dsLayer);

                if (polygon != null) {
                    layer.addPolygon(polygon.getExteriorRing().getCoordinates());
                }
            }
        });
    }

    /**
     * Returns true iff super.isEnabled() is true and the DistanceSpeed tool is
     * loaded, false otherwise.
     */
    @Override
    public boolean isEnabled() {
        AbstractVizResource<?, ?> rsc = getSelectedRsc();
        boolean enabled = rsc.getCapability(EditableCapability.class)
                .isEditable();
        if (enabled) {
            if (rsc != null) {
                enabled = findImportLayer(rsc) != null;
            }
        }
        return enabled;
    }

    /**
     * Finds the DistanceSpeedLayer.
     *
     * @param rsc
     *            The current resource
     * @return The found DistanceSpeedLayer, or null if the tool is not loaded.
     */
    private DistanceSpeedLayer findImportLayer(AbstractVizResource<?, ?> rsc) {
        ResourceList resources = rsc.getDescriptor().getResourceList();
        for (ResourcePair rp : resources) {
            if (rp.getResource() instanceof DistanceSpeedLayer) {
                return (DistanceSpeedLayer) rp.getResource();
            }
        }
        return null;
    }
}
