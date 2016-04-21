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
package com.raytheon.viz.aviation.xml;

import java.io.File;
import java.util.ArrayList;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;

/**
 * Aviation Station Configuration
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date         Ticket#     Engineer    Description
 *    ------------ ----------  ----------- --------------------------
 *    2/7/2008     817         grichard    Initial Creation.
 * 
 * </pre>
 * 
 * @author grichard
 * @version 1.0
 */
@XmlRootElement(name = "aviationStationConfig")
@XmlAccessorType(XmlAccessType.NONE)
public class AviationStationConfig implements ISerializableObject {

    /**
     * The aviation station configuration
     */
    @XmlElement(name = "station")
    private ArrayList<StationConfig> stationConfig;

    /**
     * Method to get the aviation station configuration
     * 
     * @return stationConfig
     */
    public ArrayList<StationConfig> getStationConfig() {
        return stationConfig;
    }

    /**
     * Method to set the aviation station configuration
     * 
     * @param stationConfig
     *            -- the aviation station configuration
     */
    public void setStationConfig(ArrayList<StationConfig> stationConfig) {
        this.stationConfig = stationConfig;
    }

    /**
     * Method that marshalls a sample configuration to an XML file
     * 
     * @param argv
     *            -- the arguments to the main function
     */
    public static void main(String[] args) {
        AviationStationConfig ac = new AviationStationConfig();
        StationConfig sc = new StationConfig();
        sc.setWfoSite("KOAX");
        String[] icaos = { "KLNK", "KOFK", "KOMA" };
        sc.setIcaosOfInterest(icaos);
        ArrayList<StationConfig> arr = new ArrayList<StationConfig>();
        arr.add(sc);
        ac.setStationConfig(arr);

        try {
            JAXB.marshal(ac,
                    "/home/grichard/workspace/build/static/common/cave"
                            + File.separatorChar + "etc" + File.separatorChar
                            + "aviation" + File.separatorChar + "avnwatch"
                            + File.separatorChar + "myXML" + ".xml");
        } catch (RuntimeException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }
}
