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
package com.raytheon.uf.common.monitor.xml;

import java.util.ArrayList;
import java.util.HashMap;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.monitor.config.ValueNameIdData;

/**
 * 
 * This class holds the FFFG Data XML.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 10, 2010 #4517      lvenable    Initial creation
 * Jun 17, 2013 #2085      njensen     Double checked locking of maps
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
@XmlRootElement(name = "FFFGData")
@XmlAccessorType(XmlAccessType.NONE)
public class FFFGDataXML {
    @XmlElement(name = "ExpDateTimeInMillis")
    private Long expTimeInMillis;

    @XmlElements({ @XmlElement(name = "Source", type = FFFGSourceXML.class) })
    private ArrayList<FFFGSourceXML> sources;

    private HashMap<String, FFFGSourceXML> sourceDataMap;

    public FFFGDataXML() {
    }

    public Long getExpTimeInMillis() {
        return expTimeInMillis;
    }

    public void setExpTimeInMillis(Long expTimeInMillis) {
        this.expTimeInMillis = expTimeInMillis;
    }

    public ArrayList<FFFGSourceXML> getSources() {
        return sources;
    }

    /**
     * Check if the source name exists.
     * 
     * @param sourceName
     *            Source name.
     * @return True if the source name exists, false otherwise.
     */
    public boolean containsSource(String sourceName) {
        verifySourceDataMap();

        return sourceDataMap.containsKey(sourceName);
    }

    /**
     * Set the source data.
     * 
     * @param sources
     *            Array of source data.
     */
    public void setSourceData(ArrayList<FFFGSourceXML> sources) {
        this.sources = sources;
    }

    /**
     * Get the data associated with the provided source name.
     * 
     * @param sourceName
     *            The source name.
     * @return The FFFG source XML data.
     */
    public FFFGSourceXML getSourceData(String sourceName) {
        verifySourceDataMap();

        return sourceDataMap.get(sourceName);
    }

    /**
     * Get all of the source names.
     * 
     * @return Array of all the source names.
     */
    public ArrayList<String> getAllSourceNames() {
        ArrayList<String> srcNames = new ArrayList<String>();

        if (sources == null) {
            return srcNames;
        }

        for (FFFGSourceXML srcXML : sources) {
            srcNames.add(srcXML.getSourceName());
        }

        return srcNames;
    }

    /**
     * Get an array of county/basin data.
     * 
     * @param sourceName
     *            Source name of the associated data.
     * @return An array of county/basin data.
     */
    public ArrayList<ValueNameIdData> getCountyBasinData(String sourceName) {
        verifySourceDataMap();

        if (sourceDataMap.containsKey(sourceName) == true) {
            return sourceDataMap.get(sourceName).getCountyBasinData();
        }

        return null;
    }

    /**
     * Verify the source data map exists. If not then populate the map with the
     * source name and XML.
     */
    private void verifySourceDataMap() {
        if (sourceDataMap == null) {
            synchronized (this) {
                if (sourceDataMap == null) {
                    sourceDataMap = new HashMap<String, FFFGSourceXML>();

                    if (sources == null) {
                        return;
                    }

                    for (FFFGSourceXML srcXML : sources) {
                        sourceDataMap.put(srcXML.getSourceName(), srcXML);
                    }
                }
            }
        }
    }
}
