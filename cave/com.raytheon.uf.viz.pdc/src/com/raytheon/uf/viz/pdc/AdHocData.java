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

package com.raytheon.uf.viz.pdc;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.hydrocommon.datamanager.PDCDataManager;
import com.raytheon.viz.hydrocommon.resource.AbstractMultiPointResource;

/**
 * This class contains the Ad-Hoc elements that will be displayed in the
 * elements control.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 29 NOV 2007  373        lvenable    Initial creation
 * Sep 20, 2018 7379       mduff       Refactored for D2D.
 * 
 * </pre>
 * 
 * @author lvenable
 *
 */
public class AdHocData {

    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractMultiPointResource.class);

    private ArrayList<AdHocElements> elementsArray;

    private PDCDataManager dataManager = PDCDataManager.getInstance();

    /**
     * Constructor.
     */
    public AdHocData() {
        try {
            populateElements();
        } catch (VizException e) {
            statusHandler.error("Error querying for AdHoc data.", e);
        }
    }

    private void populateElements() throws VizException {
        elementsArray = new ArrayList<>();

        LinkedHashMap<String, String> peMap = (LinkedHashMap<String, String>) dataManager
                .getPeNames();
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

        for (int i = 0; i < ingestPE.length; i++) {
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
     * 
     * @return Array of Time Step elements.
     */
    public List<AdHocElements> getDataElementArray() {
        return elementsArray;
    }

    /**
     * Get an element out the the Time Step element array.
     * 
     * @param index
     *            Array index.
     * @return Time Step Elements data object.
     */
    public AdHocElements getDataElement(int index) {
        return elementsArray.get(index);
    }
}
