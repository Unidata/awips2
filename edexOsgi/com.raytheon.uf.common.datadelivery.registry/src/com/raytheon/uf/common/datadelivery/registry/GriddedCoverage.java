package com.raytheon.uf.common.datadelivery.registry;

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

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import org.geotools.coverage.grid.GridGeometry2D;
import org.geotools.geometry.jts.ReferencedEnvelope;
import org.opengis.referencing.operation.TransformException;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.gridcoverage.exception.GridCoverageException;
import com.raytheon.uf.common.gridcoverage.subgrid.SubGrid;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Coverage for Grid XML
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2011    191      dhladky     Initial creation
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * Dec 10, 2012   1259      bsteffen    Switch Data Delivery from LatLon to referenced envelopes.
 *
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class GriddedCoverage extends Coverage implements Serializable {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(GriddedCoverage.class);

    private static final long serialVersionUID = 1458544578457L;

    public GriddedCoverage() {

    }

    public GriddedCoverage(Coverage v) {

    }

    @XmlElement
    @DynamicSerializeElement
    private GridCoverage gridCoverage;

    @XmlAttribute
    @DynamicSerializeElement
    private String gridName;

    @XmlAttribute
    @DynamicSerializeElement
    private String modelName;

    /**
     * Cache of the coverage describing the requested grid, derived from
     * requestEnvelope and gridCoverage.
     */
    private transient GridCoverage requestGridCoverage;

    /**
     * Cache of the subgrid describing the requested grid, derived from
     * requestEnvelope and gridCoverage.
     */
    private transient SubGrid requestSubGrid;

    public GridCoverage getGridCoverage() {
        return gridCoverage;
    }

    public void setGridCoverage(GridCoverage gridCoverage) {
        this.gridCoverage = gridCoverage;
        GridGeometry2D gridGeom = gridCoverage.getGridGeometry();
        ReferencedEnvelope envelope = new ReferencedEnvelope(
                gridGeom.getEnvelope2D());
        double dx = envelope.getWidth() / gridGeom.getGridRange2D().width;
        double dy = envelope.getHeight() / gridGeom.getGridRange2D().height;
        // shrink the envelope by half a grid cell so it is covering cell
        // center and not cell corner. Otherwise worldwide grids can extend
        // up to 90.5 which just seems odd.
        envelope.expandBy(dx / -2, dy / -2);
        setEnvelope(envelope);
    }

    @Override
    public void setRequestEnvelope(ReferencedEnvelope requestEnvelope) {
        // Overridden so we can make sure to reset the cache fields.
        requestGridCoverage = null;
        requestSubGrid = null;
        super.setRequestEnvelope(requestEnvelope);
    }

    /**
     * Calculate the subgrid coverage that should be used based off the current
     * requestEnvelope.
     * 
     * @param requestEnvelope
     * @return
     */
    public GridCoverage getRequestGridCoverage() {
        if (requestGridCoverage == null) {
            requestGridCoverage = getRequestGridCoverage(getRequestEnvelope(),
                    true);
            if (requestGridCoverage == null) {
                requestGridCoverage = gridCoverage;
                requestGridCoverage.setName(gridName);
                requestSubGrid = new SubGrid();
                requestSubGrid.setUpperLeftX(0);
                requestSubGrid.setUpperLeftY(0);
                requestSubGrid.setNX(gridCoverage.getNx());
                requestSubGrid.setNY(gridCoverage.getNy());
                Coordinate ll = EnvelopeUtils.getLowerLeftLatLon(getEnvelope());
                requestSubGrid.setLowerLeftLat(ll.y);
                requestSubGrid.setLowerLeftLon(ll.x);
                Coordinate ur = EnvelopeUtils
                        .getUpperRightLatLon(getEnvelope());
                requestSubGrid.setUpperRightLat(ur.y);
                requestSubGrid.setUpperRightLon(ur.x);
            }
        }
        return requestGridCoverage;
    }

    /**
     * Calculate the subgrid coverage that would be used if the given
     * requestEnvelope is applied to this GriddedCoverage. This will return null
     * if the envelope cannot be used or if it the same as the coverage
     * envelope.
     * 
     * @param requestEnvelope
     * @return
     */
    public GridCoverage getRequestGridCoverage(
            ReferencedEnvelope requestEnvelope) {
        return getRequestGridCoverage(requestEnvelope, false);
    }

    private GridCoverage getRequestGridCoverage(
            ReferencedEnvelope requestEnvelope, boolean setSubGrid) {
        try {
            if (requestEnvelope == null
                    || requestEnvelope.equals(getEnvelope())) {
                return null;
            }
            ReferencedEnvelope subGridEnvelope = MapUtil.reprojectAndIntersect(
                    requestEnvelope, getEnvelope());

            Coordinate upperRight = EnvelopeUtils
                    .getUpperRightLatLon(subGridEnvelope);
            Coordinate lowerLeft = EnvelopeUtils
                    .getLowerLeftLatLon(subGridEnvelope);

            // geotools math transforms are not perfectly accurate. This
            // can become a problem with points near the edges, causing
            // them to appear outside the valid range of the grid. This
            // will normalize the numbers to 9 decimal places in LatLon
            // space which is an accuracy within a mm.
            double roundingFactor = 1000000000.0;
            upperRight.x = ((long) (upperRight.x * roundingFactor))
                    / roundingFactor;
            upperRight.y = ((long) (upperRight.y * roundingFactor))
                    / roundingFactor;
            lowerLeft.x = ((long) (lowerLeft.x * roundingFactor))
                    / roundingFactor;
            lowerLeft.y = ((long) (lowerLeft.y * roundingFactor))
                    / roundingFactor;

            // Now we have the right corners in the right space.
            SubGrid subGrid = new SubGrid();
            subGrid.setLowerLeftLat(lowerLeft.y);
            subGrid.setLowerLeftLon(lowerLeft.x);
            subGrid.setUpperRightLat(upperRight.y);
            subGrid.setUpperRightLon(upperRight.x);

            // Perform the subgrid
            GridCoverage subGC = gridCoverage.trim(subGrid);
            subGC.setName(gridName);
            subGC.initialize();
            if (setSubGrid) {
                requestSubGrid = subGrid;
            }
            return subGC;
        } catch (TransformException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        } catch (GridCoverageException e) {
            statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(), e);
        }
        return null;
    }

    public SubGrid getRequestSubGrid() {
        if (requestSubGrid == null) {
            getRequestGridCoverage();
        }
        return requestSubGrid;
    }

    public void setGridName(String gridName) {
        this.gridName = gridName;
    }

    public String getGridName() {
        return gridName;
    }

    /**
     * @return the modelName
     */
    public String getModelName() {
        return modelName;
    }

    /**
     * @param modelName
     *            the modelName to set
     */
    public void setModelName(String modelName) {
        this.modelName = modelName;
    }

}
