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
package com.raytheon.viz.dataaccess.rsc;

import java.nio.Buffer;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.geospatial.interpolation.data.ByteBufferWrapper;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.style.level.SingleLevel;
import com.raytheon.viz.core.drawables.ColorMapParameterFactory;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;

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
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class GenericGridResource extends
        AbstractVizResource<GenericGridResourceData, MapDescriptor> {

    private static final String GRID_COLORMAP = "Grid/gridded data";

    private static final String GENERIC_GRID_LEGEND_TEXT = "Generic Grid ";

    private static final String NODATA_LEGEND_TEXT = GENERIC_GRID_LEGEND_TEXT
            + " No Data Available";

    private GriddedImageDisplay2 griddedImageDisplay;

    private boolean noData = false;

    private Buffer buffer = null;

    private GridGeometry2D gridGeometry;

    private DataTime dataTime;

    protected GenericGridResource(GenericGridResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        this.griddedImageDisplay = null;
    }

    /**
     * Initializes the data buffer, the geometry, and prepares the color map.
     * Since this is generic grid data, the color map will always be set to
     * "Grid/gridded data"
     * 
     * @throws VizException
     */
    private void prepareData() throws VizException {
        IGridData gridData = this.resourceData.getGridData();
        if (gridData == null) {
            this.noData = true;
            return;
        }

        this.gridGeometry = gridData.getGridGeometry();

        // Extract the raw data
        ByteBufferWrapper byteBufferWrapper = new ByteBufferWrapper(
                this.gridGeometry);
        byteBufferWrapper = gridData.populateData(byteBufferWrapper);
        this.buffer = byteBufferWrapper.getBuffer();

        this.dataTime = gridData.getDataTime();

        // Prepare the Color Map
        SingleLevel singleLevel = null;
        if ((gridData.getLevel() == null) == false) {
            // TODO: convert level to single level
        }
        ColorMapParameters colorMapParameters = ColorMapParameterFactory.build(
                this.buffer.array(), gridData.getParameter(),
                gridData.getUnit(), singleLevel);

        colorMapParameters.setColorMapName(GRID_COLORMAP);
        colorMapParameters.setColorMap(ColorMapLoader
                .loadColorMap(colorMapParameters.getColorMapName()));
        getCapability(ColorMapCapability.class).setColorMapParameters(
                colorMapParameters);
    }

    @Override
    public String getName() {
        if (this.noData) {
            return NODATA_LEGEND_TEXT;
        }else if(this.dataTime == null){
            return GENERIC_GRID_LEGEND_TEXT;
        } else {
            StringBuilder legend = new StringBuilder(GENERIC_GRID_LEGEND_TEXT);
            IGridData gridData = resourceData.getGridData();
            if (gridData != null) {
                if (gridData.getParameter() != null) {
                    legend.append(" ");
                    legend.append(gridData.getParameter());
                }
                if (gridData.getLevel() != null) {
                    legend.append(" ");
                    legend.append(gridData.getLevel());
                }
            }
            legend.append(this.dataTime.getLegendString());
            return legend.toString();
        }
    }

    @Override
    protected void disposeInternal() {
        if ((this.griddedImageDisplay == null) == false) {
            this.griddedImageDisplay.dispose();
        }
        this.griddedImageDisplay = null;
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        if (this.noData) {
            return;
        }

        if (this.griddedImageDisplay == null) {
            this.griddedImageDisplay = new GriddedImageDisplay2(this.buffer,
                    this.gridGeometry, this);
        }
        this.griddedImageDisplay.paint(target, paintProps);
    }

    @Override
    public void project(CoordinateReferenceSystem mapData) throws VizException {
        if ((this.griddedImageDisplay == null) == false) {
            this.griddedImageDisplay.dispose();
            this.griddedImageDisplay = null;
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        this.prepareData();
    }
}