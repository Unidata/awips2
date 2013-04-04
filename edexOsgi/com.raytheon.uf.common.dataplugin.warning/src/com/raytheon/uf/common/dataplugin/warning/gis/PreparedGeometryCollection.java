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

import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.prep.PreparedGeometry;
import com.vividsolutions.jts.geom.prep.PreparedGeometryFactory;

/**
 * {@link PreparedGeometry} implementation that can handle
 * {@link GeometryCollection} objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 28, 2013            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class PreparedGeometryCollection implements PreparedGeometry {

    private Geometry geometry;

    private PreparedGeometry[] prepared;

    public PreparedGeometryCollection(Geometry geometry) {
        this.geometry = geometry;
        int numGeoms = geometry.getNumGeometries();
        if (geometry.getClass() == GeometryCollection.class) {
            prepared = new PreparedGeometry[numGeoms];
            for (int i = 0; i < numGeoms; ++i) {
                prepared[i] = PreparedGeometryFactory.prepare(geometry
                        .getGeometryN(i));
            }
        } else {
            prepared = new PreparedGeometry[] { PreparedGeometryFactory
                    .prepare(geometry) };
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.vividsolutions.jts.geom.prep.PreparedGeometry#getGeometry()
     */
    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#contains(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean contains(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.contains(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#containsProperly(com
     * .vividsolutions.jts.geom.Geometry)
     */
    @Override
    public boolean containsProperly(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.containsProperly(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.vividsolutions.jts.geom.prep.PreparedGeometry#coveredBy(com.
     * vividsolutions.jts.geom.Geometry)
     */
    @Override
    public boolean coveredBy(Geometry geom) {
        boolean coveredBy = true;
        for (PreparedGeometry pg : prepared) {
            if (pg.coveredBy(geom) == false) {
                coveredBy = false;
                break;
            }
        }
        return coveredBy;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#covers(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean covers(Geometry geom) {
        throw new UnsupportedOperationException(
                "PreparedGeometryCollection does not support PreparedGeometry.covers");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#crosses(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean crosses(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.crosses(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#disjoint(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean disjoint(Geometry geom) {
        boolean disjoint = true;
        for (PreparedGeometry pg : prepared) {
            if (pg.disjoint(geom) == false) {
                disjoint = false;
                break;
            }
        }
        return disjoint;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.vividsolutions.jts.geom.prep.PreparedGeometry#intersects(com.
     * vividsolutions.jts.geom.Geometry)
     */
    @Override
    public boolean intersects(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.intersects(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#overlaps(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean overlaps(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.overlaps(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#touches(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean touches(Geometry geom) {
        for (PreparedGeometry pg : prepared) {
            if (pg.touches(geom)) {
                return true;
            }
        }
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.vividsolutions.jts.geom.prep.PreparedGeometry#within(com.vividsolutions
     * .jts.geom.Geometry)
     */
    @Override
    public boolean within(Geometry geom) {
        boolean within = true;
        for (PreparedGeometry pg : prepared) {
            if (pg.within(geom) == false) {
                within = false;
                break;
            }
        }
        return within;
    }

}
