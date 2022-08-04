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
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import javax.measure.Unit;
import javax.measure.UnitConverter;

import org.geotools.coverage.grid.GridGeometry2D;
import org.locationtech.jts.geom.Coordinate;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.datum.PixelInCell;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.geospatial.MapUtil;
import com.raytheon.uf.common.geospatial.ReferencedCoordinate;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.xmrg.hrap.HRAPSubGrid;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.OutlineCapability;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.mpe.MPEInterrogationConstants;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.MPEDisplayManager.DisplayMode;
import com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame;
import com.raytheon.viz.mpe.util.MPEConversionUtils;

/**
 * Abstract MPE resource for displaying gridded data
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Dec 06, 2012           mschenke  Initial creation.
 * Jul 02, 2013  2160     mpduff    Added convenience method getData(Date)
 * Mar 01, 2017  6160     bkowal    Updates for {@link
 *                                  MPEDisplayManager#createColorMap(String,
 *                                  String, int, javax.measure.unit.Unit,
 *                                  javax.measure.unit.Unit)}.
 * Mar 06, 2017  6150     bkowal    No longer limit sampling by the color scale.
 *                                  Eliminate color map unit converter
 *                                  deprecation warnings.
 * Sep 28, 2017  6407     bkowal    Cleanup.
 * Oct 03, 2017  6407     bkowal    Updated to use {@link MPEConversionUtils}.
 * Oct 06, 2017  6407     bkowal    Cleanup. Updates to support GOES-R SATPRE.
 * Nov 28, 2017  5863     bsteffen  Change dataTimes to a NavigableSet
 * Mar 06, 2019  7731     smanoj    Fix an issue with displaying
 *                                  Satellite-Derived Precipitation information.
 * Jun 20, 2019  7137     bhurley   Changed data type to allow for accumulation
 *                                  values greater than 13 inches.
 * 
 * </pre>
 * 
 * @author mschenke
 */
public abstract class AbstractGriddedMPEResource<T extends AbstractMPEGriddedResourceData, F extends Frame>
        extends AbstractVizResource<T, IMapDescriptor>
        implements IResourceDataChanged, IMpeResource {

    protected Rectangle displayExtent;

    protected GridGeometry2D gridGeometry;

    protected Map<DataTime, F> frames = new HashMap<>();

    protected AbstractGriddedMPEResource(T resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties, false);
        resourceData.addChangeListener(this);
        getCapability(ColorMapCapability.class).setColorMapParameters(
                MPEDisplayManager.createColorMap(resourceData.getCvUseString(),
                        resourceData.getDisplayString(),
                        resourceData.getDurationInHours(),
                        resourceData.getDataUnits(),
                        resourceData.getDisplayUnits()));
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        try {
            displayExtent = getHrapSubGridExtent();
            gridGeometry = MapUtil
                    .getGridGeometry(new HRAPSubGrid(displayExtent));
        } catch (Exception e) {
            throw new VizException("Error computing hrap extent coordinates.",
                    e);
        }
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        DataTime currTime = paintProps.getDataTime();
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
                frame.contourDisplay.setColor(
                        getCapability(ColorableCapability.class).getColor());
                frame.contourDisplay.setLineStyle(
                        getCapability(OutlineCapability.class).getLineStyle());
                frame.contourDisplay.paint(target, paintProps);
            }
        }
    }

    @Override
    protected void disposeInternal() {
        for (F frame : frames.values()) {
            frame.dispose();
        }
        frames.clear();
    }

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

    @Override
    public String inspect(ReferencedCoordinate coord) throws VizException {
        Map<String, Object> values = interrogate(coord);
        String dataValueLabel = (String) values
                .get(MPEInterrogationConstants.INTERROGATE_VALUE);
        if (dataValueLabel == null) {
            return "NO DATA";
        }
        return dataValueLabel;
    }

    @Override
    public Map<String, Object> interrogate(ReferencedCoordinate coord)
            throws VizException {
        try {
            Map<String, Object> values = new HashMap<>();

            ColorMapParameters parameters = getCapability(
                    ColorMapCapability.class).getColorMapParameters();
            Unit<?> displayUnit = parameters.getDisplayUnit();
            values.put(MPEInterrogationConstants.INTERROGATE_UNIT, displayUnit);

            Unit<?> dataUnit = getResourceData().getDataUnits();

            double displayValue = Double.NaN;
            String displayValueLabel = null;
            Coordinate gridCell = coord.asGridCell(gridGeometry,
                    PixelInCell.CELL_CORNER);
            final DataTime dataTime = getDescriptor().getTimeForResource(this);
            if (dataTime != null && gridCell.x >= 0 && gridCell.y >= 0
                    && gridCell.x < displayExtent.width
                    && gridCell.y < displayExtent.height) {
                int idx = ((int) gridCell.y) * displayExtent.width
                        + ((int) gridCell.x);
                int dataValue = getData(dataTime)[idx];

                UnitConverter dataToDisplay = MPEConversionUtils
                        .constructConverter(dataUnit, displayUnit);
                if (dataToDisplay == null) {
                    statusHandler
                            .error("Failed to construct a Unit Converter for: "
                                    + dataUnit.toString() + " -> "
                                    + displayUnit.toString());
                    return values;
                }
                displayValue = dataToDisplay.convert(dataValue);
                displayValueLabel = String.format("%.3f", displayValue);
                /*
                 * This appears to be how A1 MPE works with widgets by
                 * specifying string lengths, they format using %.3f but only
                 * display 2 decimal places
                 */
                displayValueLabel = displayValueLabel.substring(0,
                        displayValueLabel.length() - 1);
            }

            values.put(MPEInterrogationConstants.INTERROGATE_VALUE,
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
     * Get the data.
     * 
     * @param time
     *            date of the data
     * @return the Data
     * @throws VizException
     */
    public int[] getData(DataTime time) throws VizException {
        Frame frame = getFrame(time);
        return frame.data;
    }

    /**
     * Get the data.
     * 
     * @param date
     *            date of the data
     * @return the Data
     * @throws VizException
     */
    public int[] getData(Date date) throws VizException {
        return getData(new DataTime(date));
    }

    /**
     * Returns the hrap subgrid extent
     * 
     * @return
     */
    protected abstract Rectangle getHrapSubGridExtent();

    protected abstract F createFrame(DataTime currTime) throws VizException;

    protected abstract GriddedContourDisplay createFrameContour(F frame)
            throws VizException;

    protected abstract GriddedImageDisplay2 createFrameImage(F frame)
            throws VizException;
}
