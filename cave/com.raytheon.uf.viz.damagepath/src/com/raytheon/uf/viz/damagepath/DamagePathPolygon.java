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

import java.util.Collections;
import java.util.Map;

import com.raytheon.uf.viz.drawing.polygon.DrawablePolygon;
import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Polygon;

/**
 * Extension of {@code DrawablePolygon} to support GeoJSON properties.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2015  #4354     dgilling     Initial creation
 * Jun 30, 2015  #4354     dgilling     Force setProperties to trigger a save.
 * 
 * </pre>
 * 
 * @author dgilling
 * @version 1.0
 */

public class DamagePathPolygon extends DrawablePolygon {

    private static final Map<String, String> DEFAULT_PROPS = Collections
            .emptyMap();

    private Map<String, String> properties;

    public DamagePathPolygon(DamagePathLayer<?> layer) {
        super(layer);
        this.properties = DEFAULT_PROPS;
    }

    public DamagePathPolygon(Polygon polygon, DamagePathLayer<?> layer) {
        this(polygon, DEFAULT_PROPS, layer);
    }

    public DamagePathPolygon(Polygon polygon, Map<String, String> properties,
            DamagePathLayer<?> layer) {
        super(polygon, layer);
        this.properties = properties;
    }

    public DamagePathPolygon(Coordinate[] coords, DamagePathLayer<?> layer) {
        super(coords, layer);
        this.properties = DEFAULT_PROPS;
    }

    @Override
    public void resetPolygon(DrawablePolygon newPolygon) {
        super.resetPolygon(newPolygon);

        if (newPolygon instanceof DamagePathPolygon) {
            DamagePathPolygon newDamagePath = (DamagePathPolygon) newPolygon;
            properties = newDamagePath.getProperties();
        }
    }

    public Map<String, String> getProperties() {
        return properties;
    }

    public void setProperties(Map<String, String> properties) {
        this.properties = properties;
        ((DamagePathLayer<?>) polygonLayer).scheduleSaveJob();
    }
}
