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

import java.io.File;

import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.drawing.polygon.PolygonLayer;
import com.raytheon.uf.viz.drawing.polygon.PolygonUtil;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * A layer for displaying and customizing a weather event's damage path.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 19, 2015  3974      njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DamagePathLayer<T extends DamagePathResourceData> extends
        PolygonLayer<T> {

    protected static final String NAME = "Damage Path";

    public DamagePathLayer(T resourceData, LoadProperties loadProperties) {
        super(resourceData, loadProperties);

        /*
         * TODO default load from localization file that contains geoJSON
         * version of the damage track
         */

    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        if (polygon == null) {
            polygon = PolygonUtil.makeDefaultPolygon(getResourceContainer()
                    .getActiveDisplayPane().getRenderableDisplay());
        }

        /*
         * TODO potentially need to register to listen to changes to a damage
         * track file at the site level to support showing the damage track on
         * multiple workstations. That will get complicated if we are planning
         * on supporting more than one damage path at a time though.
         */
    }

    @Override
    public String getName() {
        return NAME;
    }

    protected Polygon loadLocalizationPolygon(LocalizationFile file) {
        /*
         * TODO load from a localization file
         */
        return null;
    }

    protected Polygon loadStormTrackPolygon() {
        /*
         * TODO get storm track file, use mathematical algorithm to derive
         * damage track
         */
        return null;
    }

    protected Polygon loadFilesystemPolygon(File file) {
        /*
         * TODO load a polygon that was selected from a file chooser dialog from
         * an import menu
         */
        return null;
    }

    protected Polygon loadMousePolygon() {
        /*
         * TODO create a new InputAdapter that takes highest priority and blocks
         * other inputs. left clicking adds vertices, right click indicates the
         * last point on the polygon, then connect the first and last point.
         * 
         * afterwards, remove and dispose of that input adapter
         */
        return null;
    }

    @Override
    public void resetPolygon(Coordinate[] coords) {
        super.resetPolygon(coords);

        /*
         * TODO need to save the polygon to a site level localization file
         */
    }

}
