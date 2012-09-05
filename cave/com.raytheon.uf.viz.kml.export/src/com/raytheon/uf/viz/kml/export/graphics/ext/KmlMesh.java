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
package com.raytheon.uf.viz.kml.export.graphics.ext;

import org.geotools.coverage.grid.GeneralGridGeometry;
import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IMesh;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Basically just a container for a grid geometry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 1, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class KmlMesh implements IMesh {

    private final GridGeometry2D imageGeometry;

    private Envelope latLonEnvelope;

    public KmlMesh(GridGeometry2D imageGeometry) {
        this.imageGeometry = imageGeometry;
    }

    public GridGeometry2D getImageGeometry() {
        return imageGeometry;
    }

    @Override
    public void dispose() {

    }

    @Override
    public boolean intersects(IExtent extent) {
        return false;
    }

    @Override
    public KmlMesh reproject(GeneralGridGeometry targetGeometry)
            throws VizException {
        return clone(targetGeometry);
    }

    @Override
    public KmlMesh clone(GeneralGridGeometry targetGeometry)
            throws VizException {
        return this;
    }

    public Envelope getLatLonEnvelope() throws TransformException {
        if (latLonEnvelope == null) {
            ReferencedEnvelope env = new ReferencedEnvelope(
                    imageGeometry.getEnvelope());
            try {
                latLonEnvelope = env.transform(MapUtil.LATLON_PROJECTION, true,
                        500);
            } catch (FactoryException e) {
                throw new TransformException(e.getLocalizedMessage(), e);
            }
        }
        return latLonEnvelope;
    }

}
