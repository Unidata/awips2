package com.raytheon.uf.common.datadelivery.retrieval.xml;

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

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;

/**
 * dataset config class for Service Config
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 20 Oct, 2012   1163      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement(name = "dataSetConfig")
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class DataSetConfig implements ISerializableObject {

    @XmlElements({ @XmlElement(name = "pattern", type = com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern.class) })
    private List<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern> patterns;

    @XmlElements({ @XmlElement(name = "dataSetNaming", type = DataSetNaming.class) })
    private List<DataSetNaming> dataSetNamings;

    private Map<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern, Pattern> patternMap = null;

    private Map<String, com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern> collectionPatternMap = null;

    private Map<String, DataSetNaming> namingPatternMap = null;

    public DataSetConfig() {

    }

    /**
     * Get the DataSetNaming particular to this collection
     * 
     * @param collectionName
     * @return
     */
    public DataSetNaming getDataSetNamingByName(String collectionName) {

        if (namingPatternMap == null) {
            namingPatternMap = new HashMap<String, DataSetNaming>();
            for (DataSetNaming ds : getDataSetNamings()) {
                namingPatternMap.put(ds.getName(), ds);
            }
        }

        return namingPatternMap.get(collectionName);
    }

    /**
     * Gets the Data set naming info
     * 
     * @return
     */
    public List<DataSetNaming> getDataSetNamings() {
        return dataSetNamings;
    }

    /**
     * Get the patterns specific to a collection (or general)
     * 
     * @param collectionName
     * @return
     */
    public com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern getPatternByName(
            String collectionName) {

        if (collectionPatternMap == null) {
            collectionPatternMap = new HashMap<String, com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern>();
            for (com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern pat : getPatterns()) {
                collectionPatternMap.put(pat.getName(), pat);
            }
        }

        return collectionPatternMap.get(collectionName);
    }

    /**
     * Gets the usable patterns for matching
     * 
     * @return
     */
    public Map<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern, Pattern> getPatternMap() {

        if (patternMap == null) {
            patternMap = new HashMap<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern, Pattern>();
            for (com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern pat : getPatterns()) {
                patternMap.put(pat, Pattern.compile(pat.getRegex()));
            }
        }

        return patternMap;
    }

    /**
     * Gets the pattern list
     * 
     * @return
     */
    public List<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern> getPatterns() {
        return patterns;
    }

    /**
     * Sets the list of data set naming objects
     * 
     * @param dataSetNaming
     */
    public void setDataSetNamings(List<DataSetNaming> dataSetNamings) {
        this.dataSetNamings = dataSetNamings;
    }

    /**
     * Sets the patterns
     * 
     * @param pattern
     */
    public void setPatterns(
            List<com.raytheon.uf.common.datadelivery.retrieval.xml.Pattern> patterns) {
        this.patterns = patterns;
    }

}
