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
import java.util.LinkedHashMap;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydro.pointdatacontrol.db.PDCDataManager;

/**
 * This class contains the Ad-Hoc elements that will be displayed in
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
public class AdHocData {
    /**
     * Array of Ad Hoc elements.
     */
    private ArrayList<AdHocElements>  elementsArray;
    
//    /**
//     * Array of element types.
//     */
//    private ArrayList<String> elementTypes;
//    
//    /**
//     * Array of data elements.
//     */
//    private ArrayList<String>  dataElementArray;
    
    private PDCDataManager dataManager = PDCDataManager.getInstance();
    
    /**
     * Constructor.
     */
    public AdHocData() {
//        populateElementTypes();
//        populateDataElements();
        try {
            populateElements();
            // TODO - implement this check
            // checkOtherList();
        } catch (VizException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
    
    private void populateElements() throws VizException {
        elementsArray = new ArrayList<AdHocElements>();
        
        LinkedHashMap<String, String> peMap = (LinkedHashMap<String, String>)dataManager.getPeNames();
        String[] ingestPE = dataManager.getIngestFilterPE();
        
        
        AdHocElements riverAHD = new AdHocElements("River");
        AdHocElements pressureAHD = new AdHocElements("Pressure");
        AdHocElements precipAHD = new AdHocElements("Rain");
        AdHocElements snowAHD = new AdHocElements("Snow");
        AdHocElements tempAHD = new AdHocElements("Temperature");
        AdHocElements agAHD = new AdHocElements("Agriculture");
        AdHocElements evapoAHD = new AdHocElements("Evaporation");
        AdHocElements fishAHD = new AdHocElements("Fish Count");
        AdHocElements groundAHD = new AdHocElements("Ground");
        AdHocElements iceAHD = new AdHocElements("Ice");
        AdHocElements lakeAHD = new AdHocElements("Lake");
        AdHocElements moistureAHD = new AdHocElements("Moisture");
        AdHocElements gateDamAHD = new AdHocElements("GateDam");
        AdHocElements radiationAHD = new AdHocElements("Radiation");
        AdHocElements wxAHD = new AdHocElements("Weather");
        AdHocElements windAHD = new AdHocElements("Wind");
        AdHocElements powerAHD = new AdHocElements("Power");
        AdHocElements waterQualityAHD = new AdHocElements("Water Quality");
        AdHocElements yUniqueAHD = new AdHocElements("YUnique");
        AdHocElements processedAHD = new AdHocElements("Processed");
        
        precipAHD.addDataElement("PC and PP");
        riverAHD.addDataElement("Primary");

        
//        Set<String> keys = peMap.keySet();
//        Iterator<String> iter = keys.iterator();
        
//        while (iter.hasNext()) {
        for (int i = 0; i < ingestPE.length; i++) {
//            String s = iter.next();
//            char peChar = s.charAt(0);
            String pe = ingestPE[i];
            char peChar = pe.charAt(0);
               
            switch (peChar) {
            case 'H':
            case 'Q':
                riverAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'P':
                /* Get the second character of the pe */
                char c = pe.charAt(1);
                switch (c) {
                case 'A':
                case 'D':
                case 'E':
                case 'L':
                    pressureAHD.addDataElement(pe + " " + peMap.get(pe));
                    break;
                case 'C':
                case 'F':
                case 'J':
                case 'M':
                case 'N':
                case 'P':
                case 'R':
                case 'T':
                case 'Y':
                    precipAHD.addDataElement(pe + " " + peMap.get(pe));
                    break;
                }
                break;
            case 'S':
                snowAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'T':
                tempAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'A':
            case 'B':
            case 'C':
                agAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'E':
                evapoAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'F':
                fishAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'G':
                groundAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'I':
                iceAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'L':
                lakeAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'M':
                moistureAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'N':
                gateDamAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'R':
                radiationAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'X':
                wxAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'U':
                windAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'V':
                powerAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'W':
                waterQualityAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            case 'Y':
                yUniqueAHD.addDataElement(pe + " " + peMap.get(pe));
                break;
            }            
        }
        processedAHD.addDataElement("IE Extent of Ice");
        processedAHD.addDataElement("IR Ice Report");
        processedAHD.addDataElement("PP Precip Increment");
        processedAHD.addDataElement("QR River Discharge");
        processedAHD.addDataElement("TA Air Temperature");
        
        elementsArray.add(riverAHD);
        elementsArray.add(precipAHD);
        elementsArray.add(snowAHD);
        elementsArray.add(tempAHD);
        elementsArray.add(agAHD);
        elementsArray.add(evapoAHD);
        elementsArray.add(fishAHD);
        elementsArray.add(groundAHD);
        elementsArray.add(iceAHD);
        elementsArray.add(lakeAHD);
        elementsArray.add(moistureAHD);
        elementsArray.add(gateDamAHD);
        elementsArray.add(pressureAHD);
        elementsArray.add(radiationAHD);
        elementsArray.add(wxAHD);
        elementsArray.add(windAHD);
        elementsArray.add(powerAHD);
        elementsArray.add(waterQualityAHD);
        elementsArray.add(yUniqueAHD);
        elementsArray.add(processedAHD);
    }
    
    /**
     * Get the array of Time Step elements.
     * @return Array of Time Step elements.
     */
    public ArrayList<AdHocElements> getDataElementArray()
    {
        return elementsArray;
    }
    
    /**
     * Get an element out the the Time Step element array.
     * @param index Array index.
     * @return Time Step Elements data object.
     */
    public AdHocElements getDataElement(int index)
    {
        return elementsArray.get(index);
    }
    
//    
//    /**
//     * Get the array of element types. 
//     * @return Array of element types.
//     */
//    public ArrayList<String> getElementTypes() {
//        return elementTypes;
//    }
//    
//    /**
//     * Get the array of data elements.
//     * @return Array of data elements.
//     */
//    public ArrayList<String> getDataElements() {
//        return dataElementArray;
//    }
    
    /**
     * Populate the element types array.
     */
//    private void populateElementTypes() {
//        elementTypes = new ArrayList<String>();
//        elementTypes.add("River");
//        elementTypes.add("Rain");
//        elementTypes.add("Snow");
//        elementTypes.add("Temperature");
//        elementTypes.add("Agriculture");
//        elementTypes.add("Evaporation");
//        elementTypes.add("FishCount");
//        elementTypes.add("Ground");
//        elementTypes.add("Ice");
//        elementTypes.add("Lake");
//        elementTypes.add("Moisture");
//        elementTypes.add("GateDam");
//        elementTypes.add("Pressure");
//        elementTypes.add("Radiation");
//        elementTypes.add("Weather");
//        elementTypes.add("Wind");
//        elementTypes.add("Power");
//        elementTypes.add("WaterQuality");
//        elementTypes.add("YUnique");
//        elementTypes.add("Processed");
//    }
//    
//    /**
//     * Populate the data elements array. 
//     */
//    private void populateDataElements() {
//        dataElementArray = new ArrayList<String>();
//        dataElementArray.add("HA - Reading Height - Sfc");
//        dataElementArray.add("HB - Depth Below Sfc");
//        dataElementArray.add("HC - Ceiling Height");
//        dataElementArray.add("HD - Head Height");
//        dataElementArray.add("HE - Regulating Gate");
//        dataElementArray.add("HF - Forebay Elevation");
//        dataElementArray.add("HG - River Stage");
//        dataElementArray.add("HH - Reading Height - MSL");
//        dataElementArray.add("HI - Stage Trnd Indicator");
//        dataElementArray.add("HJ - Spillway Gate Height");
//        dataElementArray.add("HK - Lake Elev Abv Datum");
//        dataElementArray.add("HL - Lake Elevation");
//        dataElementArray.add("HM - Tide Height");
//        dataElementArray.add("HO - Flood Stage Height");
//        dataElementArray.add("HP - Pool Elevation");
//        dataElementArray.add("HQ - Distance to River");
//        dataElementArray.add("HR - Reservoir Rule Elev");
//        dataElementArray.add("HS - Spillwy Forebay Elev");
//        dataElementArray.add("HT - Tailwater Elev");
//        dataElementArray.add("HU - Cautionary Stage Hgt");
//        dataElementArray.add("HW - Spillway Tailwater");
//        dataElementArray.add("HZ - Freezing Level Elev");        
//    }
}
