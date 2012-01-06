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

package com.raytheon.viz.hydro.pointdatacontrol;

import java.util.ArrayList;

/**
 * This class contains the Time Step elements that will be displayed in
 * the elements control.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation 
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 *
 */
public class TimeStepData
{    
    /**
     * Array of Time Step elements.
     */
    private ArrayList<TimeStepElements>  elementsArray;
    
    /**
     * Constructor.
     */
    public TimeStepData()
    {
        populateElements();
    }
    
    /**
     * Populate the Time Step elements.
     */
    private void populateElements()
    {
        elementsArray = new ArrayList<TimeStepElements>();
        
        TimeStepElements riverTSD = new TimeStepElements("River");
        riverTSD.addDataElement("STAGE/POOL");
        riverTSD.addDataElement("FLOW/STORAGE");
        riverTSD.addDataElement("DEPTH ABOVE FLOOD STAGE");
        riverTSD.addDataElement("PERCENT OF FLOOD FLOW");
        elementsArray.add(riverTSD);
        
        TimeStepElements rainTSD = new TimeStepElements("Rain");
        rainTSD.addDataElement("INSTANTANEOUS");
        rainTSD.addDataElement("1-HOUR PRECIP TOTAL");
        rainTSD.addDataElement("3-HOUR PRECIP TOTAL");
        rainTSD.addDataElement("6-HOUR PRECIP TOTAL");
        rainTSD.addDataElement("24-HOUR TOTAL (12Z)");
        elementsArray.add(rainTSD);
        
        TimeStepElements snowTSD = new TimeStepElements("Snow");
        snowTSD.addDataElement("SNOW WATER EQUIVALENT");
        snowTSD.addDataElement("SWE - 24 HOUR CHANGE");
        elementsArray.add(snowTSD);
        
        TimeStepElements temperatureTSD = new TimeStepElements("Temperature");
        temperatureTSD.addDataElement("TEMPERATURE");
        temperatureTSD.addDataElement("TEMP. 24 HOUR CHANGE");
        temperatureTSD.addDataElement("MAX TEMP");
        temperatureTSD.addDataElement("MIN TEMP");
        elementsArray.add(temperatureTSD);
        
        TimeStepElements humidityTSD = new TimeStepElements("Humidity");
        humidityTSD.addDataElement("DEWPOINT");
        humidityTSD.addDataElement("DEWPT - 24 HR CHANGE");
        humidityTSD.addDataElement("RELATIVE HUMIDITY");
        elementsArray.add(humidityTSD);
        
        TimeStepElements windTSD = new TimeStepElements("Wind");
        windTSD.addDataElement("WIND SPEED");
        windTSD.addDataElement("WIND DIRECTION");
        elementsArray.add(windTSD);
    }
    
    /**
     * Get the array of Time Step elements.
     * @return Array of Time Step elements.
     */
    public ArrayList<TimeStepElements> getDataElementArray()
    {
        return elementsArray;
    }
    
    /**
     * Get an element out the the Time Step element array.
     * @param index Array index.
     * @return Time Step Elements data object.
     */
    public TimeStepElements getDataElement(int index)
    {
        return elementsArray.get(index);
    }
}
