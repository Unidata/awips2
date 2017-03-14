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
package com.raytheon.uf.common.dataplugin.warning.gis;

import java.util.HashMap;
import java.util.Map;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 15, 2011            rjpeter     Initial creation
 * Jul 21, 2016 DR 18159   Qinglu Lin  Added copyAttributes().
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class GeospatialData implements Cloneable {
    @DynamicSerializeElement
    public Map<String, Object> attributes;

    @DynamicSerializeElement
    public Geometry geometry;

    public transient GeospatialData parent;

    public transient PreparedGeometry prepGeom;

    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public void setAttributes(Map<String, Object> attributes) {
        this.attributes = attributes;
    }

    public GeospatialData getParent() {
        return parent;
    }

    public void setParent(GeospatialData parent) {
        this.parent = parent;
    }

    public Geometry getGeometry() {
        return geometry;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }

    public PreparedGeometry getPrepGeom() {
        return prepGeom;
    }

    public void setPrepGeom(PreparedGeometry prepGeom) {
        this.prepGeom = prepGeom;
    }

    public GeospatialData copyAttributes() {
        GeospatialData gsd;
        try {
            gsd = (GeospatialData)super.clone();
        } catch (CloneNotSupportedException e) {
            throw new RuntimeException("Error invoking super.clone().", e);
        }
        gsd.attributes = new HashMap<>(attributes);
        return gsd;
    }
}
