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
package com.raytheon.viz.mpe.ui.rsc;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.measure.converter.UnitConverter;
import javax.measure.unit.Unit;

import org.geotools.coverage.grid.GridGeometry2D;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.hydro.spatial.HRAPSubGrid;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.mpe.MPEInterrogationConstants;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * Abstract MPE resource for displaying gridded data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 6, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractGriddedMPEResource<T extends AbstractMPEGriddedResourceData, F extends Frame>
        extends AbstractVizResource<T, IMapDescriptor> implements
        IResourceDataChanged, IMpeResource {

    protected Rectangle displayExtent;

    protected GridGeometry2D gridGeometry;

    protected Map<DataTime, F> frames = new HashMap<DataTime, F>();

    /** Replace once resource can look up time for itself */
    @Deprecated
    private DataTime lastPainted;

    /**
     * @param resourceData
     * @param loadProperties
     */
    protected AbstractGriddedMPEResource(T resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        resourceData.addChangeListener(this);
        dataTimes = new ArrayList<DataTime>();
        getCapability(ColorMapCapability.class).setColorMapParameters(
                MPEDisplayManager.createColorMap(resourceData.getCvUseString(),
                        resourceData.getDurationInHours(),
                        resourceData.getDataUnits(),
                        resourceData.getDisplayUnits()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
     * .uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        try {
            displayExtent = getHrapSubGridExtent();
            gridGeometry = MapUtil.getGridGeometry(new HRAPSubGrid(
                    displayExtent));
        } catch (Exception e) {
            throw new VizException("Error computing hrap extent coordinates");
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
        DataTime currTime = lastPainted = paintProps.getDataTime();
        if (currTime == null) {
            return;
        }

        F frame = getFrame(currTime);
        synchronized (frame) {
            if (resourceData.isDisplayed(DisplayMode.Image)) {
                if (frame.imageDisplay == null) {
                    frame.imageDisplay = createFrameImage(frame);
                }
                frame.imageDisplay.paint(target, paintProps);
            }
            if (resourceData.isDisplayed(DisplayMode.Contour)) {
                if (frame.contourDisplay == null) {
                    frame.contourDisplay = createFrameContour(frame);
                }
                frame.contourDisplay.setColor(getCapability(
                        ColorableCapability.class).getColor());
                frame.contourDisplay.setLineStyle(getCapability(
                        OutlineCapability.class).getLineStyle());
                frame.contourDisplay.paint(target, paintProps);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        for (F frame : frames.values()) {
            frame.dispose();
        }
        frames.clear();
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
        for (Frame frame : frames.values()) {
            frame.dispose();
        }
        issueRefresh();
    }

    /**
     * Gets the {@link F} object for the time
     * 
     * @param currTime
     * @return
     * @throws VizException
     */
    protected F getFrame(DataTime currTime) throws VizException {
        F frame = frames.get(currTime);
        if (frame == null) {
            frame = createFrame(currTime);
            frames.put(currTime, frame);
            dataTimes.add(currTime);
        }
        return frame;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#inspect(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> values = interrogate(coord);
        String dataValueLabel = (String) values
                .get(MPEInterrogationConstants.INTERROGATE_VALUE_LABEL);
        if (dataValueLabel == null) {
            return "NO DATA";
        }
        return dataValueLabel;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractVizResource#interrogate(com.raytheon
     * .uf.viz.core.geospatial.ReferencedCoordinate)
     */
    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            Map<String, Object> values = new HashMap<String, Object>();

            ColorMapParameters parameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            DataMappingPreferences dm = parameters.getDataMapping();
            Unit<?> displayUnit = parameters.getDisplayUnit();
            values.put(MPEInterrogationConstants.INTERROGATE_UNIT, displayUnit);

            double displayValue = Double.NaN;
            String displayValueLabel = null;
            Coordinate gridCell = coord.asGridCell(gridGeometry,
                    PixelInCell.CELL_CORNER);
            if (lastPainted != null && gridCell.x >= 0 && gridCell.y >= 0
                    && gridCell.x < displayExtent.width
                    && gridCell.y < displayExtent.height) {
                int idx = ((int) gridCell.y) * displayExtent.width
                        + ((int) gridCell.x);
                short dataValue = getData(lastPainted)[idx];
                UnitConverter dataToImage = parameters
                        .getDataToImageConverter();
                double imageValue = dataToImage.convert(dataValue);
                displayValueLabel = dm.getLabelValueForDataValue(imageValue);
                if (displayValueLabel == null) {
                    displayValue = parameters.getDataToDisplayConverter()
                            .convert(dataValue);
                    displayValueLabel = String.format("%.3f", displayValue);
                    // This appears to be how A1 MPE works with widgets by
                    // specifying string lengths, they format using %.3f but
                    // only display 2 decimal places
                    displayValueLabel = displayValueLabel.substring(0,
                            displayValueLabel.length() - 1);
                }
            }

            values.put(MPEInterrogationConstants.INTERROGATE_VALUE,
                    displayValue);
            values.put(MPEInterrogationConstants.INTERROGATE_VALUE_LABEL,
                    displayValueLabel);

            return values;
        } catch (Exception e) {
            throw new VizException("Error performing interrogation", e);
        }
    }

    /**
     * Gets the hrap extent for this resource
     * 
     * @return
     */
    public Rectangle getHrapExtent() {
        return new Rectangle(displayExtent);
    }

    /**
     * Use {@link #getData(DataTime)} instead
     * 
     * @return
     * @throws VizException
     */
    @Deprecated
    public final short[] getData() throws VizException {
        return getData(lastPainted);
    }

    public short[] getData(DataTime time) throws VizException {
        Frame frame = getFrame(time);
        return frame.data;
    }

    /**
     * Returns the hrap subgrid extent
     * 
     * @return
     */
    protected abstract Rectangle getHrapSubGridExtent();

    /**
     * @param currTime
     * @return
     */
    protected abstract F createFrame(DataTime currTime) throws VizException;

    protected abstract GriddedContourDisplay createFrameContour(F frame)
            throws VizException;

    protected abstract GriddedImageDisplay2 createFrameImage(F frame)
            throws VizException;
}
