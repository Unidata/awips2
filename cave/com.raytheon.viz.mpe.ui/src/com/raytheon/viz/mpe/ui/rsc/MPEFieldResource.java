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

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences;
import com.raytheon.uf.common.colormap.prefs.DataMappingPreferences.DataMappingEntry;
import com.raytheon.uf.common.hydro.spatial.HRAPCoordinates;
import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.style.LabelingPreferences;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.core.contours.rsc.displays.GriddedContourDisplay;
import com.raytheon.viz.core.rsc.displays.GriddedImageDisplay2;
import com.raytheon.viz.mpe.MPEDateFormatter;
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
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 29, 2012            mschenke    Initial creation.
 * May 28, 2013 15971      lbousaidi    change the reading hour for SATPRE
 *                                      since the start time in the file is one 
 *                                      hour less than the file time stamp. 
 * Jul 02, 2013   2160     mpduff      Changed how edited data are called for return.
 * Sep 17, 2013 16563      snaples      Updated createFrameImage to handle trace precip 
 *                                      properly when mapping to screen.	
 * Mar 10, 2014 17059      snaples      Added case for Prism data for unit conversion correction.
 * Mar 19, 2014 17109      snaples      Removed code that added an hour to SATPRE, the base file reference time has been adjusted.
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
    private static final int  BIG_VALUE = 1000 ;
    private static final int  RATIO_CONVERSION_FACTOR = 100;

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
    @SuppressWarnings("incomplete-switch")
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
        return getEditedData(frame);
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

                    	
            if (displayField==DisplayFieldData.satPre) {
            	 //SATPRE MPE file time stamp is the start time of the hour 
            	 //i.e. a 12z -13z product has a time stamp of 12z.             	
            	 timeToLoad.add(Calendar.HOUR, -1);             	 
            }
            	 
           
            
            if (displayField.isAComparisonField() )
            {
            	ComparisonFields comparisonFields = displayField.getComparisonFields();
            	DisplayFieldData field1 = comparisonFields.getField1();
            	DisplayFieldData field2 = comparisonFields.getField2();
            	
            	XmrgFile file1 = MPEDisplayManager.getXmrgFile(field1,
            			timeToLoad.getTime()); 
            	
            	XmrgFile file2 = MPEDisplayManager.getXmrgFile(field2,
            			timeToLoad.getTime());
            	
            	boolean isDifference = false;
            	boolean isRatio = false;
            	
            	if (displayField.equals(DisplayFieldData.precipDifferenceField))
            	{
            		isDifference = true;

            	}
            	else if (displayField.equals(DisplayFieldData.precipRatioField))
            	{
            		isRatio = true;
            	}
            	
            	try {
            		file1.load();
            		file2.load();
            	} catch (IOException e) {
            		Activator.statusHandler.handle(
            				Priority.INFO,
            				"Error loading XMRG file for "
            						+ field1 + " or " + field2 
            						+ " at time "
            						+ MPEDateFormatter
            						.format_MMM_dd_yyyy_HH(timeToLoad
            								.getTime()), e);
            		continue;
            	}
            	
               	Rectangle fileExtent = file1.getHrapExtent();
            	short[] file1Data = file1.getData();
            	short[] file2Data = file2.getData();
            	           	
            	for (int y = 0; y < displayExtent.height; ++y) {
            		for (int x = 0; x < displayExtent.width; ++x) {
            			
            			int px = x + displayExtent.x;
            			int py = y + displayExtent.y;
            			if (px >= fileExtent.x
            					&& px < (fileExtent.x + fileExtent.width)
            					&& py >= fileExtent.y
            					&& py < (fileExtent.y + fileExtent.height))
            			{
            				int frameIdx = y * displayExtent.width + x;
            				int fx = px - fileExtent.x;
            				int fy = py - fileExtent.y;
            				int fileIdx = fy * fileExtent.width + fx;
            				
            				short value1 = file1Data[fileIdx];
            				short value2 = file2Data[fileIdx];
            				
            			
            				short fi = 0;
            				
            				if (isDifference)
            				{
            					short diffValue = calculateDifference(value1, value2);
            					fi = diffValue;
            				}
            				else if (isRatio)
            				{
            					double ratio = calculateRatio(value1, value2);
 
            					if (ratio != MISSING_VALUE)
            					{
            						fi = (short) ( ratio * RATIO_CONVERSION_FACTOR );
            					}
            					else
            					{
            						fi = MISSING_VALUE;
            					}
            					           				
            				}
            				
            				//short fc = frameData[frameIdx];
            				//fc is initial value of frameData[frameIdx],
            				//it is used to help accumulate precip in a multi-hour accum situation
            				frameData[frameIdx] = fi;

            			} //end if (px >=)
            		} //end  for x
            	} //end for y
            	
            }
            else //is a non-comparison field
            {

            	XmrgFile file = MPEDisplayManager.getXmrgFile(displayField,
            			timeToLoad.getTime()); 
            	try {
            	    long fileLength = file.getFile().length();
            	    //System.out.printf("FileName = %s, length = %d\n", file.getFile().getPath(), fileLength);
            	    if (fileLength > 0)
            	    {
            	        file.load();
            	    }
            	    else //can't read the file since it is empty
            	    {
            	        continue;
            	    }
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
            				
            				if (fc < 0 && fi >= 0) 
            				{
            					//orig precip is missing, and this hour's value is valid (> = 0)
            					// so set the value to the current hour's value
            					frameData[frameIdx] = fi;
            				}
            				else if (fc >= 0 && fi > 0)
            				{
            					//some previous hour's precip has been recorded and this hour's value is valid (> = 0)
            					//so accumulate
            					frameData[frameIdx] += fi;
            				}
            			} //end if (px >=)
            		} //end  for x
            	} //end for y
            } //end else is a non-comparison field
            
        } //end for i

        return new MPEFieldFrame(currTime.getRefTime(), frameData,
                PolygonEditManager.getPolygonEdits(resourceData.getFieldData(),
                        currTime.getRefTime()));
    }
    
    private short calculateDifference(short value1, short value2)
    {
        short result = 0;
        
        if (( value1 >= 0) && (value2 >= 0) )
        {
            result = (short) (value1 - value2 );
        }
        else
        {
            result = MISSING_VALUE;
        }
        
        
       
        
        return result;
        
    }
    
    
    private double calculateRatio(short numerator, short denominator)
    {
		double result = 0;
		
		if (denominator > 0)
		{
			if (numerator >= 0)
			{
				result = numerator / denominator;
			}
			else
			{
				result = MISSING_VALUE;
			}
		}
		
		else if (denominator == 0)
		{
			if (numerator == 0)
			{
				result = 1.0; //if no rain, they are in agreeement, so show this
			}
			else if (numerator > 0)
			{
				result = BIG_VALUE;
			}
			else // numerator is missing
			{
				result = MISSING_VALUE;
			}
		}
		else
		{
			result = MISSING_VALUE;
		}
	
		return result;
	
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
                    short value = data[i];
                    if (value == MISSING_VALUE) {
                        imageData[i] = 0;
                    } else {
                        imageData[i] = (short) dataToImage.convert(value);
                    }
                } 
            	break;
            
            case Prism:
                for (int i = 0; i < length; ++i) {
                    short value = data[i];
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
                    short value = data[i];
                    if (value == MISSING_VALUE) {
                        imageData[i] = 0;
                    } 
                    else 
                    {
                        imageData[i] = (short) dataToImage.convert(value);
                    }        	
                } 
            	break;
            	
                
            default :
            	for (int i = 0; i < length; ++i) {
                    short value = data[i];
                    if (value == MISSING_VALUE) {
                        imageData[i] = 0;
                    } else if(value <= 0){
                	    imageData[i] = 1;
                    } else if(value > 0 && value < 25){
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
