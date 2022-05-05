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
import java.nio.FloatBuffer;
import java.nio.ShortBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.measure.UnitConverter;

import org.apache.commons.collections.CollectionUtils;
import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.dataplugin.mpe.PrecipRecord;
import com.raytheon.uf.common.mpe.fieldgen.PrecipDataKey;
import com.raytheon.uf.common.mpe.fieldgen.PrecipField;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.ContourLabelingPreferences;
import com.raytheon.uf.common.style.ValuesLabelingPreferences;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.xmrg.hrap.HRAPCoordinates;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.grid.display.GriddedImageDisplay2;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.alerts.IAlertObserver;
import com.raytheon.viz.alerts.observers.ProductAlertObserver;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.mpe.ui.Activator;
import com.raytheon.viz.mpe.ui.ComparisonFields;
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
 * Date          Ticket#     Engineer   Description
 * ------------- ----------- ---------- ----------------------------------------
 * Nov 29, 2012              mschenke   Initial creation.
 * May 28, 2013  15971       lbousaidi  change the reading hour for SATPRE since
 *                                      the start time in the file is one hour
 *                                      less than the file time stamp.
 * Jul 02, 2013  2160        mpduff     Changed how edited data are called for
 *                                      return.
 * Sep 17, 2013  16563       snaples    Updated createFrameImage to handle trace
 *                                      precip properly when mapping to screen.
 * Mar 10, 2014  17059       snaples    Added case for Prism data for unit
 *                                      conversion correction.
 * Mar 19, 2014  17109       snaples    Removed code that added an hour to
 *                                      SATPRE, the base file reference time has
 *                                      been adjusted.
 * Nov 05, 2015  18095       lbousaidi  Fixed hour substitued for satellite
 *                                      field precip when drawing polygon.
 * Dec 04, 2015  5165/14513  mduff      Set this resource on the display manager
 *                                      if not set in the display manager.
 * Dec 08, 2015  5180        bkowal     Made the hour substitution special case
 *                                      precise.
 * Feb 15, 2016  5338        bkowal     Update the persistent set of polygons
 *                                      for an existing frame after one or all
 *                                      are deleted.
 * Sep 21, 2017  6407        bkowal     Centralized data loading and retrieval.
 *                                      Implements {@link IAlertObserver} to
 *                                      support automatic refreshing of certain
 *                                      precip field types within the hour.
 * Oct 30, 2017  17911       wkwock     Display RFC QPE
 * May 29, 2019 60162        ksunil     changes to absorb new Contour Label structure
 * Jun 20, 2019  7137        bhurley    Changed data type to allow for
 *                                      accumulation values greater than 13
 *                                      inches.
 * 
 * </pre>
 * 
 * @author mschenke
 */

public class MPEFieldResource
        extends AbstractGriddedMPEResource<MPEFieldResourceData, MPEFieldFrame>
        implements IPolygonEditsChangedListener, IAlertObserver {

    private static final short MISSING_VALUE = -899;

    private static final int BIG_VALUE = 1000;

    private static final int RATIO_CONVERSION_FACTOR = 100;

    private ContourPreferences contourPreferences;

    /**
     * @param resourceData
     * @param loadProperties
     */
    public MPEFieldResource(MPEFieldResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        getCapability(ColorableCapability.class)
                .setColor(new RGB(255, 255, 255));
        if (resourceData.getFieldData() == DisplayFieldData.goesRSatPre) {
            ProductAlertObserver.addObserver(PrecipRecord.PLUGIN_NAME, this);
        }
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        contourPreferences = createContourPreferences(
                getCapability(ColorMapCapability.class)
                        .getColorMapParameters());
        PolygonEditManager.registerListener(this);
        MPEDisplayManager displayManager = MPEDisplayManager
                .getInstance(descriptor.getRenderableDisplay());
        MPEFieldResource rsc = displayManager.getDisplayedFieldResource();
        if (rsc == null) {
            displayManager.setDisplayedResource(this);
        }
    }

    @Override
    protected void disposeInternal() {
        if (resourceData.getFieldData() == DisplayFieldData.goesRSatPre) {
            ProductAlertObserver.removeObserver(PrecipRecord.PLUGIN_NAME, this);
        }
        super.disposeInternal();
        PolygonEditManager.unregisterListener(this);
    }

    public List<RubberPolyData> getPolygonEdits(Date date) {
        try {
            MPEFieldFrame frame = getFrame(new DataTime(date));
            return new ArrayList<>(frame.getPolygonEdits());
        } catch (VizException e) {
            Activator.statusHandler.handle(Priority.PROBLEM,
                    "Error getting polygon edits for "
                            + resourceData.getFieldData() + " for time: "
                            + date,
                    e);
        }
        return new ArrayList<>();
    }

    /**
     * @param frame
     * @return
     */
    private int[] getEditedData(MPEFieldFrame frame) {
        int[] editedData = frame.getEditedData();
        if (editedData != null) {
            return editedData;
        }
        int[] data = frame.data;
        editedData = Arrays.copyOf(data, data.length);
        List<RubberPolyData> polygonEdits = frame.getPolygonEdits();
        Map<DisplayFieldData, short[]> dataMap = new HashMap<>();
        ColorMapParameters parameters = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter displayToData = parameters.getDisplayToDataConverter();
        UnitConverter dataToDisplay = parameters.getDataToDisplayConverter();
        int width = displayExtent.width;
        int height = displayExtent.height;
        for (RubberPolyData edit : polygonEdits) {
            if (!edit.isVisible()) {
                continue;
            }
            short[] subData = null;
            PolygonEditAction editAction = edit.getEditAction();
            if (editAction == PolygonEditAction.SUB) {
                subData = dataMap.get(edit.getSubDrawSource());
                if (subData == null) {
                    try {
                        Date date = frame.getDate();
                        /*
                         * SATPRE MPE file time stamp is the start time of the
                         * hour. i.e. a 12z -13z product has a time stamp of
                         * 12z.
                         */
                        Calendar cal = Calendar.getInstance();
                        cal.setTime(date);
                        if (edit.getSubDrawSource() == DisplayFieldData.satPre) {
                            cal.add(Calendar.HOUR, -1);
                        }

                        MPEPrecipData mpePrecipData = MPEPrecipDataLoader
                                .load(edit.getSubDrawSource(), cal);
                        subData = mpePrecipData.getData();
                        dataMap.put(edit.getSubDrawSource(), subData);
                    } catch (MPEDataLoadException e) {
                        Activator.statusHandler.handle(Priority.INFO,
                                "Error loading Substitution data from "
                                        + edit.getSubDrawSource()
                                        + " for polygon data",
                                e);
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
                    default:
                        /* Do Nothing. */
                        break;
                    }
                }
            }
        }

        frame.setEditedData(editedData);
        return editedData;
    }

    @Override
    public int[] getData(DataTime time) throws VizException {
        MPEFieldFrame frame = getFrame(time);
        return getEditedData(frame);
    }

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

    @Override
    protected Rectangle getHrapSubGridExtent() {
        try {
            return HRAPCoordinates.getHRAPCoordinates();
        } catch (Exception e) {
            throw new RuntimeException("Error getting hrap sub grid extent.",
                    e);
        }
    }

    @Override
    protected MPEFieldFrame createFrame(DataTime currTime) throws VizException {
        // Get field we are displaying
        DisplayFieldData displayField = resourceData.getFieldData();
        // Get accumInterval ensuring not less than 1
        int accumInterval = Math.max(resourceData.getAccumulationInterval(), 1);

        int datasz = displayExtent.height * displayExtent.width;
        int[] frameData = new int[datasz];
        Arrays.fill(frameData, MISSING_VALUE);

        Calendar timeToLoad = Calendar.getInstance();

        for (int i = 0; i < accumInterval; ++i) {
            timeToLoad.setTime(currTime.getRefTime());
            timeToLoad.add(Calendar.HOUR, -i);

            if (displayField == DisplayFieldData.satPre) {
                // SATPRE MPE file time stamp is the start time of the hour
                // i.e. a 12z -13z product has a time stamp of 12z.
                timeToLoad.add(Calendar.HOUR, -1);
            }

            if (displayField.isAComparisonField()) {
                ComparisonFields comparisonFields = displayField
                        .getComparisonFields();
                DisplayFieldData field1 = comparisonFields.getField1();
                DisplayFieldData field2 = comparisonFields.getField2();

                MPEPrecipData mpePrecipData1 = null;
                try {
                    mpePrecipData1 = MPEPrecipDataLoader.load(field1,
                            timeToLoad);
                } catch (MPEDataLoadException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                if (mpePrecipData1 == null) {
                    continue;
                }
                MPEPrecipData mpePrecipData2 = null;
                try {
                    mpePrecipData2 = MPEPrecipDataLoader.load(field2,
                            timeToLoad);
                } catch (MPEDataLoadException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }
                if (mpePrecipData2 == null) {
                    continue;
                }

                boolean isDifference = false;
                boolean isRatio = false;

                if (displayField
                        .equals(DisplayFieldData.precipDifferenceField)) {
                    isDifference = true;

                } else if (displayField
                        .equals(DisplayFieldData.precipRatioField)) {
                    isRatio = true;
                }

                /*
                 * TODO: should really verify that they are both using the same
                 * hrap extents.
                 */
                Rectangle fileExtent = mpePrecipData1.getHrapExtent();
                short[] file1Data = mpePrecipData1.getData();
                short[] file2Data = mpePrecipData2.getData();

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

                            short value1 = file1Data[fileIdx];
                            short value2 = file2Data[fileIdx];

                            short fi = 0;

                            if (isDifference) {
                                short diffValue = calculateDifference(value1,
                                        value2);
                                fi = diffValue;
                            } else if (isRatio) {
                                double ratio = calculateRatio(value1, value2);

                                if (ratio != MISSING_VALUE) {
                                    fi = (short) (ratio
                                            * RATIO_CONVERSION_FACTOR);
                                } else {
                                    fi = MISSING_VALUE;
                                }

                            }

                            // short fc = frameData[frameIdx];
                            // fc is initial value of frameData[frameIdx],
                            // it is used to help accumulate precip in a
                            // multi-hour accum situation
                            frameData[frameIdx] = fi;
                        }
                    }
                }
            } else {
                // is a non-comparison field
                MPEPrecipData mpePrecipData = null;
                try {
                    mpePrecipData = MPEPrecipDataLoader.load(displayField,
                            timeToLoad);
                } catch (MPEDataLoadException e) {
                    statusHandler.handle(Priority.PROBLEM,
                            e.getLocalizedMessage(), e);
                }

                if (mpePrecipData == null) {
                    continue;
                }

                Rectangle fileExtent = mpePrecipData.getHrapExtent();
                short[] fileData = mpePrecipData.getData();
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
                            int fc = frameData[frameIdx];

                            if (fc < 0 && fi >= 0) {
                                // orig precip is missing, and this hour's value
                                // is valid (> = 0)
                                // so set the value to the current hour's value
                                frameData[frameIdx] = fi;
                            } else if (fc >= 0 && fi > 0) {
                                // some previous hour's precip has been recorded
                                // and this hour's value is valid (> = 0)
                                // so accumulate
                                frameData[frameIdx] += fi;
                            }
                        }
                    }
                }
            }

        }

        return new MPEFieldFrame(currTime.getRefTime(), frameData,
                PolygonEditManager.getPolygonEdits(resourceData.getFieldData(),
                        currTime.getRefTime()));
    }

    private short calculateDifference(short value1, short value2) {
        short result = 0;

        if ((value1 >= 0) && (value2 >= 0)) {
            result = (short) (value1 - value2);
        } else {
            result = MISSING_VALUE;
        }

        return result;

    }

    private double calculateRatio(short numerator, short denominator) {
        double result = 0;

        if (denominator > 0) {
            if (numerator >= 0) {
                result = numerator / denominator;
            } else {
                result = MISSING_VALUE;
            }
        }

        else if (denominator == 0) {
            if (numerator == 0) {
                /*
                 * if no rain, they are in agreement, so show this
                 */
                result = 1.0;
            } else if (numerator > 0) {
                result = BIG_VALUE;
            } else {
                // numerator is missing
                result = MISSING_VALUE;
            }
        } else {
            result = MISSING_VALUE;
        }

        return result;

    }

    @Override
    protected GriddedContourDisplay createFrameContour(MPEFieldFrame frame)
            throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter dataToDisplay = params.getDataToDisplayConverter();
        int[] data = getEditedData(frame);
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

    @Override
    protected GriddedImageDisplay2 createFrameImage(MPEFieldFrame frame)
            throws VizException {
        ColorMapParameters params = getCapability(ColorMapCapability.class)
                .getColorMapParameters();
        UnitConverter dataToImage = params.getDataToImageConverter();
        int[] data = getEditedData(frame);
        DisplayFieldData cvuse = resourceData.getFieldData();
        int length = data.length;
        short[] imageData = new short[length];
        switch (cvuse) {
        case Locbias:
        case LocbiasDP:
        case Height:
        case Index:
        case Locspan:
        case LocspanDP:
        case mintempPrism:
        case maxtempPrism:
            for (int i = 0; i < length; ++i) {
                int value = data[i];
                if (value == MISSING_VALUE) {
                    imageData[i] = 0;
                } else {
                    imageData[i] = (short) dataToImage.convert(value);
                }
            }
            break;

        case Prism:
            for (int i = 0; i < length; ++i) {
                int value = data[i];
                if (value < 0) {
                    imageData[i] = 0;
                } else {
                    imageData[i] = (short) dataToImage.convert(value);
                }
            }
            break;

        case precipDifferenceField:
        case precipRatioField:
            for (int i = 0; i < length; ++i) {
                int value = data[i];
                if (value == MISSING_VALUE) {
                    imageData[i] = 0;
                } else {
                    imageData[i] = (short) dataToImage.convert(value);
                }
            }
            break;

        default:
            for (int i = 0; i < length; ++i) {
                int value = data[i];
                if (value == MISSING_VALUE) {
                    imageData[i] = 0;
                } else if (value <= 0) {
                    imageData[i] = 1;
                } else if (value > 0 && value < 25) {
                    value = 10;
                    imageData[i] = (short) dataToImage.convert(value);
                } else {
                    imageData[i] = (short) dataToImage.convert(value);
                }
            }
            break;
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
                values[i] = entry.getDisplayValue().floatValue();
                i++;
            }
            ContourLabelingPreferences labelingPreferences = new ContourLabelingPreferences();

            ValuesLabelingPreferences valuePref = new ValuesLabelingPreferences();
            valuePref.setValues(values);
            labelingPreferences
                    .setValues(new ArrayList<ValuesLabelingPreferences>(
                            Arrays.asList(valuePref)));

            labelingPreferences.setLabelFormat("0.00");
            preferences.setContourLabeling(labelingPreferences);
        }
        return preferences;
    }

    @Override
    public void polygonEditsChanged(DisplayFieldData field, Date date,
            List<RubberPolyData> polygonEdits,
            List<RubberPolyData> persistentRemaining) {
        if (field == resourceData.getFieldData()) {
            MPEFieldFrame frame = frames.get(new DataTime(date));
            if (frame != null) {
                frame.setPolygonEdits(polygonEdits);
                issueRefresh();
            }

            if (persistentRemaining == null) {
                /*
                 * A persistent polygon has been removed. So, the lists of
                 * polygons for any existing frames will need to be rebuilt. The
                 * polygons are stored in a text file without any unique
                 * identifier and polygons with duplicate fields are allowed to
                 * exist. The polygons must currently remain stored in this
                 * format to maintain compatibility with legacy C++ mpe code.
                 * So, the only option (with the current implementation) is to
                 * remove any persistent polygons from the existing list and
                 * re-add the remaining set of persistent polygons. The lists of
                 * polygons cannot be replaced across all frames because the
                 * list of polygon edits passed to this method also contains
                 * frame/time specific polygons.
                 */
                return;
            }
            for (MPEFieldFrame otherFrame : frames.values()) {
                if (otherFrame.getDate().equals(date)) {
                    // already updated this frame.
                    continue;
                }
                List<RubberPolyData> currentPolygonEdits = otherFrame
                        .getPolygonEdits();
                if (CollectionUtils.isEmpty(currentPolygonEdits)) {
                    continue;
                }
                boolean persistentRemoved = false;
                Iterator<RubberPolyData> persistentIter = currentPolygonEdits
                        .iterator();
                while (persistentIter.hasNext()) {
                    if (persistentIter.next().isPersistent()) {
                        persistentIter.remove();
                        persistentRemoved = true;
                    }
                }
                if (!persistentRemoved) {
                    continue;
                }
                currentPolygonEdits.addAll(persistentRemaining);
                otherFrame.setPolygonEdits(currentPolygonEdits);
            }
        }
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        Date refreshDate = null;
        Calendar calendar = getDescriptor().getTimeForResource(this)
                .getRefTimeAsCalendar();
        MPEPrecipInventoryManager inventory;
        try {
            inventory = MPEPrecipInventoryManager
                    .getInstance(PrecipField.SATPRE);
        } catch (MPEDataLoadException e) {
            statusHandler
                    .error("Failed to retrieve the data inventory for field: "
                            + PrecipField.SATPRE.name() + ".", e);
            return;
        }
        for (AlertMessage alertMessage : alertMessages) {
            final DataTime dataTime = (DataTime) alertMessage.decodedAlert
                    .get("dataTime");
            Calendar alertCalendar = dataTime.getRefTimeAsCalendar();
            alertCalendar = TimeUtil.minCalendarFields(alertCalendar,
                    Calendar.MINUTE, Calendar.SECOND, Calendar.MILLISECOND);
            inventory.addToInventory(
                    new PrecipDataKey(PrecipField.SATPRE, alertCalendar));
            if (alertCalendar.equals(calendar)) {
                /*
                 * Only refresh the display if the data updates are applicable
                 * to the current hour.
                 */
                refreshDate = calendar.getTime();
            }
        }
        if (refreshDate != null) {
            resourceChanged(ChangeType.DATA_UPDATE, refreshDate);
        }
    }

    @Override
    protected MPEFieldFrame getFrame(DataTime currTime) throws VizException {
        MPEFieldFrame frame = super.getFrame(currTime);

        List<RFCQPEResource> rfcQpeRscList = MPEDisplayManager.getCurrent()
                .getRfcQpeRscList();
        Iterator<RFCQPEResource> iterator = rfcQpeRscList.iterator();
        while (iterator.hasNext()) {
            RFCQPEResource rfcQpeRsc = iterator.next();

            if (rfcQpeRsc.getStatus() == ResourceStatus.DISPOSED) {
                iterator.remove();
            }
        }

        // if there's RFC QPE displaying, dispayMPE field in it's RFC boundary
        // only else display it in it's HRAP area.
        if (frame != null && frame.data != null) {
            if (rfcQpeRscList.isEmpty()) {
                resourceData.unmaskData(frame);
            } else {
                resourceData.maskData(frame);
            }
        }
        return frame;
    }
}
