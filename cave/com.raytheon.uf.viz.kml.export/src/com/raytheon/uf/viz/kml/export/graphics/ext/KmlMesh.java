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
import org.opengis.coverage.grid.GridGeometry;
import org.opengis.geometry.Envelope;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGridMesh;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * Basically just a container for a grid geometry.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- -----------------------------
 * Jun 01, 2012  704      bsteffen  Initial creation
 * Feb 21, 2014  2817     bsteffen  Remove Deprecated reproject.
 * Apr 06, 2016  5400     bsteffen  Implement IGridMesh
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class KmlMesh implements IGridMesh {

    private final GridGeometry2D imageGeometry;

    private final GridGeometry2D targetGeometry;

    private Envelope latLonEnvelope;

    public KmlMesh(GridGeometry2D imageGeometry, GridGeometry2D targetGeometry) {
        this.imageGeometry = imageGeometry;
        this.targetGeometry = targetGeometry;
    }

    public GridGeometry2D getImageGeometry() {
        return imageGeometry;
    }

    @Override
    public void dispose() {
        /*
         * Nothing to do because no resources are out of reach of the garbage
         * collector.
         */
    }

    @Override
    public boolean intersects(IExtent extent) {
        return false;
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

    @Override
    public GridGeometry getSourceGeometry() {
        return imageGeometry;
    }

    @Override
    public GridGeometry getTargetGeometry() {
        return targetGeometry;
    }

}
