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
package com.raytheon.uf.viz.core.maps.rsc;

import java.util.List;

import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.maps.rsc.AbstractDbMapResourceData.ColumnDefinition;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.LabelableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.MagnificationCapability;
import com.vividsolutions.jts.geom.Envelope;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public abstract class AbstractDbMapResource<T extends AbstractDbMapResourceData, D extends IMapDescriptor>
        extends AbstractMapResource<T, D> implements IResourceDataChanged {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DbMapResource.class);

    protected static final double EXPANSION_FACTOR = 0.25;

    protected IFont font;

    protected PixelExtent lastExtent;

    protected PixelExtent projExtent;

    private List<String> labelFields;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractDbMapResource(T resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
    }

    @Override
    protected void disposeInternal() {
        resourceData.removeChangeListener(this);

        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.IResourceDataChanged#resourceChanged(com
     * .raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType,
     * java.lang.Object)
     */
    @Override
    public void resourceChanged(ChangeType type, Object object) {
        if (type == ChangeType.CAPABILITY) {
            if (object instanceof MagnificationCapability) {
                if (font != null) {
                    font.dispose();
                    font = null;
                }
            }
        }
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#setDescriptor(com.raytheon
     * .uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public void setDescriptor(D descriptor) {
        super.setDescriptor(descriptor);

        projExtent = new PixelExtent(descriptor.getGridGeometry()
                .getGridRange());

        lastExtent = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem crs) throws VizException {
        super.project(crs);

        projExtent = new PixelExtent(descriptor.getGridGeometry()
                .getGridRange());

        lastExtent = null;
    }

    /**
     * @return the labelFields
     */
    public List<String> getLabelFields() {
        if (this.labelFields == null) {
            try {
                this.labelFields = DbMapQueryFactory.getMapQuery(
                        resourceData.getTable(), resourceData.getGeomField())
                        .getColumnNamesWithoutGeometries();
                ColumnDefinition[] columns = resourceData.getColumns();
                if (columns != null) {
                    for (ColumnDefinition col : columns) {
                        labelFields.add(col.getName());
                    }
                }
            } catch (VizException e) {
                statusHandler.handle(Priority.PROBLEM,
                        "Error querying available label fields", e);
            }
        }
        return this.labelFields;
    }

    /**
     * @param screenExtent
     * @return
     */
    protected PixelExtent getExpandedExtent(PixelExtent screenExtent) {
        PixelExtent expandedExtent = screenExtent.clone();
        expandedExtent.getEnvelope().expandBy(
                expandedExtent.getWidth() * EXPANSION_FACTOR,
                expandedExtent.getHeight() * EXPANSION_FACTOR);

        return clipToProjExtent(expandedExtent);
    }

    protected PixelExtent clipToProjExtent(PixelExtent extent) {
        Envelope e = extent.getEnvelope()
                .intersection(projExtent.getEnvelope());
        PixelExtent clipped = new PixelExtent(e.getMinX(), e.getMaxX(),
                e.getMinY(), e.getMaxY());
        return clipped;
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        getCapability(LabelableCapability.class).setAvailableLabelFields(
                getLabelFields().toArray(new String[0]));
    }

    /*
     * (non-Javadoc)
     * 
     * @see java.lang.Object#toString()
     */
    @Override
    public String toString() {
        return this.resourceData.toString();
    }
}
