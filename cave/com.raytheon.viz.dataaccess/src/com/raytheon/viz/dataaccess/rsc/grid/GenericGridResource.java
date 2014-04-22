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
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import javax.measure.unit.UnitFormat;

import org.apache.commons.lang.StringUtils;
import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataaccess.grid.IGridData;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.numeric.buffer.FloatBufferWrapper;
import com.raytheon.uf.common.style.StyleException;
import com.raytheon.uf.common.style.image.ColorMapParameterFactory;
import com.raytheon.uf.common.style.level.Level.LevelType;
import com.raytheon.uf.common.style.level.SingleLevel;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapLoader;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
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
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 8, 2013            bkowal      Initial creation
 * Jan 31, 2013  1555     bkowal      Refactor
 * Mar 07, 2014  2791     bsteffen    Move Data Source/Destination to numeric
 *                                    plugin.
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

    private Map<DataTime, GriddedImageDisplay2> displays = new HashMap<DataTime, GriddedImageDisplay2>();

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
    protected void prepareData(IGraphicsTarget target, DataTime time)
            throws VizException {
        IGridData gridData = this.resourceData.getFirstDataElement(time);

        GridGeometry2D gridGeometry = gridData.getGridGeometry();

        // Extract the raw data
        FloatBufferWrapper bufferWrapper = new FloatBufferWrapper(
                gridGeometry.getGridRange2D());
        bufferWrapper = gridData.populateData(bufferWrapper);
        Buffer buffer = bufferWrapper.getBuffer();

        initColorMapParameters(gridData, buffer);

        GriddedImageDisplay2 griddedImageDisplay = new GriddedImageDisplay2(
                buffer, gridGeometry, this);
        displays.put(time, griddedImageDisplay);
    }

    protected void initColorMapParameters(IGridData gridData, Buffer buffer)
            throws VizException {
        ColorMapCapability capability = getCapability(ColorMapCapability.class);
        ColorMapParameters colorMapParameters = capability
                .getColorMapParameters();
        if (colorMapParameters == null || this.displays.isEmpty()) {
            SingleLevel singleLevel = null;
            if (gridData.getLevel() != null) {
                try {
                    singleLevel = new SingleLevel(gridData.getLevel()
                            .getMasterLevel().getName());
                } catch (IllegalArgumentException e) {
                    // level cannot be mapped to a level
                    singleLevel = new SingleLevel(LevelType.DEFAULT);
                }
                singleLevel.setValue(gridData.getLevel().getLevelonevalue());
            }
            ColorMapParameters newCmp;
            try {
                newCmp = ColorMapParameterFactory.build(buffer.array(),
                        gridData.getParameter(), gridData.getUnit(),
                        singleLevel);
            } catch (StyleException e) {
                throw new VizException("Unable to build colormap parameters", e);
            }
            if (colorMapParameters != null) {
                // This means the capability was serialized so preserve
                // serialized fields.
                newCmp.applyPersistedParameters(colorMapParameters
                        .getPersisted());
                newCmp.setColorMapName(colorMapParameters.getColorMapName());
            }
            colorMapParameters = newCmp;
        }
        if (colorMapParameters.getColorMap() == null) {
            String colorMapName = colorMapParameters.getColorMapName();
            if (colorMapName == null) {
                colorMapName = GRID_COLORMAP;
                colorMapParameters.setColorMapName(colorMapName);
            }
            colorMapParameters.setColorMap(ColorMapLoader
                    .loadColorMap(colorMapName));
        }
        capability.setColorMapParameters(colorMapParameters);
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.dataaccess.rsc.AbstractDataAccessResource#
     * buildLegendTextInternal()
     */
    @Override
    protected String buildLegendTextInternal() {
        DataTime timeToInspect = null;
        if (!isTimeAgnostic()) {
            timeToInspect = descriptor.getTimeForResource(this);
            if (timeToInspect == null) {
                return "";
            }
        }
        IGridData gridData = this.resourceData
                .getFirstDataElement(timeToInspect);
        if (gridData == null) {
            return StringUtils.EMPTY;
        }

        StringBuilder stringBuilder = new StringBuilder();

        if (gridData.getLocationName() != null) {
            stringBuilder.append(gridData.getLocationName());
            stringBuilder.append(_SPACE_);
        }
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
        Collection<GriddedImageDisplay2> displays = this.displays.values();
        this.displays = new HashMap<DataTime, GriddedImageDisplay2>();
        for (GriddedImageDisplay2 display : displays) {
            display.dispose();
        }
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
        DataTime timeToPaint = null;
        if (!isTimeAgnostic()) {
            timeToPaint = paintProps.getDataTime();
            if (timeToPaint == null) {
                return;
            }
        }
        GriddedImageDisplay2 griddedImageDisplay = displays.get(timeToPaint);
        if (griddedImageDisplay == null) {
            prepareData(target, paintProps.getDataTime());
            griddedImageDisplay = displays.get(paintProps.getDataTime());
        }
        griddedImageDisplay.paint(target, paintProps);

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
        for (GriddedImageDisplay2 display : displays.values()) {
            display.project(this.descriptor.getGridGeometry());
        }
    }

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        DataTime timeToInspect = null;
        if (!isTimeAgnostic()) {
            timeToInspect = descriptor.getTimeForResource(this);
            if (timeToInspect == null) {
                return "";
            }
        }
        GriddedImageDisplay2 display = displays.get(timeToInspect);
        if (display == null) {
            return null;
        }
        ColorMapParameters cmp = getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        double value = Double.NaN;
        try {
            value = display.interrogate(coord.asLatLon());
        } catch (Exception e) {
            throw new VizException(e);
        }
        String unitString = "";
        if (cmp.getDisplayUnit() != null) {
            unitString = _SPACE_
                    + UnitFormat.getUCUMInstance().format(cmp.getDisplayUnit());
            value = cmp.getDataToDisplayConverter().convert(value);
        }
        return value + unitString;
    }
}