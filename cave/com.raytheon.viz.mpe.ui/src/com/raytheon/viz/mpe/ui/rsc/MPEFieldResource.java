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

import java.awt.Point;
import java.awt.Rectangle;
import java.io.IOException;
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.measure.converter.UnitConverter;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.uf.viz.core.style.DataMappingPreferences;
import com.raytheon.uf.viz.core.style.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.viz.core.style.LabelingPreferences;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.core.style.contour.ContourPreferences;
import com.raytheon.viz.mpe.MPEDateFormatter;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.DisplayFieldData;
import com.raytheon.viz.mpe.ui.MPEDisplayManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.IPolygonEditsChangedListener;
import com.raytheon.viz.mpe.ui.dialogs.polygon.PolygonEditManager;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData;
import com.raytheon.viz.mpe.ui.dialogs.polygon.RubberPolyData.PolygonEditAction;
import com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame;
import com.raytheon.viz.mpe.ui.rsc.MPEFieldResourceData.MPEFieldFrame;

/**
 * MPE resource that displays field data. Also supports data editing
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class MPEFieldResource extends
        AbstractGriddedMPEResource<MPEFieldResourceData, MPEFieldFrame>
        implements IPolygonEditsChangedListener {

    private static final short MISSING_VALUE = -899;

    private ContourPreferences contourPreferences;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public MPEFieldResource(MPEFieldResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(ColorableCapability.class).setColor(
                new RGB(255, 255, 255));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#initInternal(com
     * .raytheon.uf.viz.core.IGraphicsTarget)
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        contourPreferences = createContourPreferences(getCapability(
                ColorMapCapability.class).getColorMapParameters());
        PolygonEditManager.registerListener(this);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#disposeInternal()
     */
    @Override
    protected void disposeInternal() {
        super.disposeInternal();
        PolygonEditManager.unregisterListener(this);
    }

    public List<RubberPolyData> getPolygonEdits(Date date) {
        try {
            MPEFieldFrame frame = getFrame(new DataTime(date));
            return new ArrayList<RubberPolyData>(frame.getPolygonEdits());
        } catch (VizException e) {
            Activator.statusHandler.handle(
                    Priority.PROBLEM,
                    "Error getting polygon edits for "
                            + resourceData.getFieldData() + " for time: "
                            + date, e);
        }
        return new ArrayList<RubberPolyData>();
    }

    /**
     * @param frame
     * @return
     */
    private short[] getEditedData(MPEFieldFrame frame) {
        short[] editedData = frame.getEditedData();
        if (editedData != null) {
            return editedData;
        }
        short[] data = frame.data;
        editedData = Arrays.copyOf(data, data.length);
        List<RubberPolyData> polygonEdits = frame.getPolygonEdits();
        Map<DisplayFieldData, short[]> dataMap = new HashMap<DisplayFieldData, short[]>();
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter displayToData = parameters.getDisplayToDataConverter();
        UnitConverter dataToDisplay = parameters.getDataToDisplayConverter();
        int width = displayExtent.width;
        int height = displayExtent.height;
        for (RubberPolyData edit : polygonEdits) {
            if (edit.isVisible() == false) {
                continue;
            }
            short[] subData = null;
            PolygonEditAction editAction = edit.getEditAction();
            if (editAction == PolygonEditAction.SUB) {
                subData = dataMap.get(edit.getSubDrawSource());
                if (subData == null) {
                    try {
                        XmrgFile subFile = MPEDisplayManager.getXmrgFile(
                                edit.getSubDrawSource(), frame.getDate());
                        subFile.load();
                        subData = subFile.getData();
                        dataMap.put(edit.getSubDrawSource(), subData);
                    } catch (IOException e) {
                        Activator.statusHandler.handle(
                                Priority.INFO,
                                "Error loading Substitution data from "
                                        + edit.getSubDrawSource()
                                        + " for polygon data", e);
                    }
                }
            }

            double precipValue = edit.getPrecipValue();
            double converted = (short) displayToData.convert(precipValue);

            // Due to data mapping, this ensures we are never less than original
            // precipValue when converting
            double testPrecipValue = dataToDisplay.convert(converted);
            if (testPrecipValue < precipValue) {
                converted += 1.0;
            }
            for (Point p : edit.getEditPoints()) {
                int x = p.x - displayExtent.x;
                int y = displayExtent.y + height - p.y - 1;

                int idx = y * width + x;
                if (subData != null) {
                    editedData[idx] = subData[idx];
                } else {
                    switch (editAction) {
                    case RAISE:
                        editedData[idx] = (short) Math.max(converted,
                                editedData[idx]);
                        break;
                    case LOWER:
                        editedData[idx] = (short) Math.min(converted,
                                editedData[idx]);
                        break;
                    case SCALE:
                        editedData[idx] *= precipValue;
                        break;
                    case SNOW:
                        // TODO: According to MPE users guide, SNOW should set
                        // data to data value and then exclude gages located
                        // within polygon whose values are < precipValue from
                        // the MPE Analysis. Old code did not do this and
                        // incorrectly set values to -9999
                    case SET:
                        editedData[idx] = (short) converted;
                        break;
                    }
                }
            }
        }

        frame.setEditedData(editedData);
        return editedData;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#getData(com.raytheon
     * .uf.common.time.DataTime)
     */
    @Override
    public short[] getData(DataTime time) throws VizException {
        MPEFieldFrame frame = getFrame(time);
        return frame.getEditedData();
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
        if (type == ChangeType.DATA_UPDATE) {
            if (object instanceof Date) {
                // Reload frame for date if created
                Frame frame = frames.remove(new DataTime((Date) object));
                if (frame != null) {
                    frame.dispose();
                }
            }
        } else if (type == ChangeType.CAPABILITY) {
            if (object instanceof ColorMapCapability) {
                ColorMapParameters params = ((ColorMapCapability) object)
                        .getColorMapParameters();
                if (params != null) {
                    contourPreferences = createContourPreferences(params);
                    for (MPEFieldFrame frame : frames.values()) {
                        frame.disposeContour();
                    }
                }
            }
        }
        issueRefresh();
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#getHrapSubGridExtent
     * ()
     */
    @Override
    protected Rectangle getHrapSubGridExtent() {
        try {
            return HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e) {
            throw new RuntimeException("Error getting hrap sub grid extent");
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrame(com
     * .raytheon.uf.common.time.DataTime)
     */
    @Override
    protected MPEFieldFrame createFrame(DataTime currTime) throws VizException {
        // Get field we are displaying
        DisplayFieldData displayField = resourceData.getFieldData();
        // Get accumInterval ensuring not less than 1
        int accumInterval = Math.max(resourceData.getAccumulationInterval(), 1);

        int datasz = displayExtent.height * displayExtent.width;
        short[] frameData = new short[datasz];
        Arrays.fill(frameData, MISSING_VALUE);

        Calendar timeToLoad = Calendar.getInstance();

        for (int i = 0; i < accumInterval; ++i) {
            timeToLoad.setTime(currTime.getRefTime());
            timeToLoad.add(Calendar.HOUR, -i);

            XmrgFile file = MPEDisplayManager.getXmrgFile(displayField,
                    timeToLoad.getTime());
            try {
                file.load();
            } catch (IOException e) {
                Activator.statusHandler.handle(
                        Priority.INFO,
                        "Error loading XMRG file for "
                                + displayField
                                + " at time "
                                + MPEDateFormatter
                                        .format_MMM_dd_yyyy_HH(timeToLoad
                                                .getTime()), e);
                continue;
            }

            Rectangle fileExtent = file.getHrapExtent();
            short[] fileData = file.getData();
            for (int y = 0; y < displayExtent.height; ++y) {
                for (int x = 0; x < displayExtent.width; ++x) {
                    int px = x + displayExtent.x;
                    int py = y + displayExtent.y;
                    if (px >= fileExtent.x
                            && px < (fileExtent.x + fileExtent.width)
                            && py >= fileExtent.y
                            && py < (fileExtent.y + fileExtent.height)) {
                        int frameIdx = y * displayExtent.width + x;
                        int fx = px - fileExtent.x;
                        int fy = py - fileExtent.y;
                        int fileIdx = fy * fileExtent.width + fx;
                        short fi = fileData[fileIdx];
                        short fc = frameData[frameIdx];
                        if (fc < 0 && fi >= 0) {
                            frameData[frameIdx] = fi;
                        } else if (fc >= 0 && fi > 0) {
                            frameData[frameIdx] += fi;
                        }
                    }
                }
            }
        }

        return new MPEFieldFrame(currTime.getRefTime(), frameData,
                PolygonEditManager.getPolygonEdits(resourceData.getFieldData(),
                        currTime.getRefTime()));
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrameContour
     * (com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame)
     */
    @Override
    protected GriddedContourDisplay createFrameContour(MPEFieldFrame frame)
            throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter dataToDisplay = params.getDataToDisplayConverter();
        short[] data = getEditedData(frame);
        int length = data.length;
        float[] contourData = new float[length];
        for (int i = 0; i < length; ++i) {
            contourData[i] = (float) dataToDisplay.convert(data[i]);
        }
        GriddedContourDisplay display = new GriddedContourDisplay(
                getDescriptor(), gridGeometry, FloatBuffer.wrap(contourData));
        display.setPreferences(contourPreferences);
        return display;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.rsc.AbstractGriddedMPEResource#createFrameImage
     * (com.raytheon.viz.mpe.ui.rsc.AbstractMPEGriddedResourceData.Frame)
     */
    @Override
    protected GriddedImageDisplay2 createFrameImage(MPEFieldFrame frame)
            throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter dataToImage = params.getDataToImageConverter();
        short[] data = getEditedData(frame);
        int length = data.length;
        short[] imageData = new short[length];
        for (int i = 0; i < length; ++i) {
            short value = data[i];
            if (value == MISSING_VALUE) {
                imageData[i] = 0;
            } else {
                imageData[i] = (short) dataToImage.convert(value);
            }
        }
        return new GriddedImageDisplay2(ShortBuffer.wrap(imageData),
                gridGeometry, this);
    }

    private ContourPreferences createContourPreferences(
            ColorMapParameters parameters) {
        ContourPreferences preferences = new ContourPreferences();
        DataMappingPreferences prefs = parameters.getDataMapping();
        if (prefs != null) {
            Collection<DataMappingEntry> entries = prefs.getEntries();
            float[] values = new float[entries.size()];
            int i = 0;
            for (DataMappingEntry entry : entries) {
                values[i++] = entry.getDisplayValue().floatValue();
            }
            LabelingPreferences labelingPreferences = new LabelingPreferences();
            labelingPreferences.setValues(values);
            labelingPreferences.setLabelFormat("0.00");
            preferences.setContourLabeling(labelingPreferences);
        }
        return preferences;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.mpe.ui.dialogs.polygon.IPolygonEditsChangedListener#
     * polygonEditsChanged(com.raytheon.viz.mpe.ui.DisplayFieldData,
     * java.util.Date, java.util.List)
     */
    @Override
    public void polygonEditsChanged(DisplayFieldData field, Date date,
            List<RubberPolyData> polygonEdits) {
        if (field == resourceData.getFieldData()) {
            MPEFieldFrame frame = frames.get(new DataTime(date));
            if (frame != null) {
                frame.setPolygonEdits(polygonEdits);
                issueRefresh();
            }
        }
    }
}
