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
package com.raytheon.viz.dataaccess.rsc.grid;

import java.nio.Buffer;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteBufferWrapper;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource;

/**
 * Renders a generic grid image based on grid data that is retrieved using the
 * Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 8, 2013            bkowal     Initial creation
 * Jan 31, 2013 #1555     bkowal     Refactor
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class GenericGridResource extends
        AbstractDataAccessResource<GenericGridResourceData> {

    private static final String GRID_COLORMAP = "Grid/gridded data";

    private static final String GENERIC_LEGEND_TEXT = "Generic Grid ";

    private GriddedImageDisplay2 griddedImageDisplay;

    protected GenericGridResource(GenericGridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, GENERIC_LEGEND_TEXT);
    }

    /**
     * Initializes the data buffer, the geometry, and prepares the color map.
     * Since this is generic grid data, the color map will always be set to
     * "Grid/gridded data"
     * 
     * @throws VizException
     */
    @Override
    protected void prepareData(IGraphicsTarget target) throws VizException {
        IGridData gridData = this.resourceData.getFirstDataElement();

        GridGeometry2D gridGeometry = gridData.getGridGeometry();

        // Extract the raw data
        ByteBufferWrapper byteBufferWrapper = new ByteBufferWrapper(
                gridGeometry);
        byteBufferWrapper = gridData.populateData(byteBufferWrapper);
        Buffer buffer = byteBufferWrapper.getBuffer();

        // Prepare the Color Map
        SingleLevel singleLevel = null;
        if (gridData.getLevel() != null) {
            // TODO: convert level to single level
        }
        ColorMapParameters colorMapParameters = ColorMapParameterFactory.build(
                buffer.array(), gridData.getParameter(), gridData.getUnit(),
                singleLevel);

        colorMapParameters.setColorMapName(GRID_COLORMAP);
        colorMapParameters.setColorMap(ColorMapLoader
                .loadColorMap(colorMapParameters.getColorMapName()));
        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);
        this.griddedImageDisplay = new GriddedImageDisplay2(buffer,
                gridGeometry, this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#
     * buildLegendTextInternal()
     */
    @Override
    protected String buildLegendTextInternal() {
        StringBuilder stringBuilder = new StringBuilder();

        IGridData gridData = this.resourceData.getFirstDataElement();
        if (gridData.getParameter() != null) {
            stringBuilder.append(gridData.getParameter());
            stringBuilder.append(_SPACE_);
        }
        if (gridData.getLevel() != null) {
            stringBuilder.append(gridData.getLevel());
            stringBuilder.append(_SPACE_);
        }

        return stringBuilder.toString();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#disposeResource
     * ()
     */
    @Override
    protected void disposeInternal() {
        if (this.griddedImageDisplay != null) {
            this.griddedImageDisplay.dispose();
        }
        this.griddedImageDisplay = null;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties)
     */
    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        this.griddedImageDisplay.paint(target, paintProps);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#project(org.opengis.
     * referencing.crs.CoordinateReferenceSystem)
     */
    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        this.griddedImageDisplay.project(this.descriptor.getGridGeometry());
    }
}