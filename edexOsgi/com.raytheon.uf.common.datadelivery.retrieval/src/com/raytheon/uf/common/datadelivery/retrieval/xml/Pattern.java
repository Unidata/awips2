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

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Pattern recognition class for Service Config
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

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Pattern implements ISerializableObject {

    private static final String SEPARATOR = ",";

    @XmlAttribute(name = "name")
    @DynamicSerializeElement
    private String name;

    @XmlAttribute(name = "dataSetLocation")
    @DynamicSerializeElement
    private String dataSetLocation;

    @XmlAttribute(name = "cycleLocation")
    @DynamicSerializeElement
    private String cycleLocation;

    @XmlAttribute(name = "regex")
    @DynamicSerializeElement
    private String regex;

    public Pattern() {

    }

    public String getCycleLocation() {
        return cycleLocation;
    }

    /**
     * Get the list of cycle locations
     * 
     * @return
     */
    public List<Integer> getCycleLocationAsInt() {
        List<Integer> locations = new ArrayList<Integer>();

        String[] chunks = getCycleLocation().split(SEPARATOR);
        if (chunks.length >= 1) {
            for (String chunk : chunks) {
                locations.add(Integer.parseInt(chunk));
            }
        }

        return locations;
    }

    /**
     * gets the cycle location at a value
     * 
     * @param loc
     * @return
     */
    public int getCycleLocationAt(int loc) {
        int value = -2;
        // We use -2 here because -1 indicates a dataset that has no cycles
        // We need to differentiate between the two for how we parse the set.
        List<Integer> list = getCycleLocationAsInt();

        if (list != null && !list.isEmpty() && loc >= 0 && loc < list.size()) {
            value = list.get(loc);
        }

        return value;
    }

    public String getDataSetLocation() {
        return dataSetLocation;
    }

    /**
     * Gets the list of dataset locations
     * 
     * @return
     */
    public List<Integer> getDataSetLocationAsInt() {
        List<Integer> locations = new ArrayList<Integer>();
        String[] chunks = getDataSetLocation().split(SEPARATOR);
        if (chunks.length >= 1) {
            for (String chunk : chunks) {
                locations.add(Integer.parseInt(chunk));
            }
        }

        return locations;
    }

    /**
     * gets the data set location at a value
     * 
     * @param loc
     * @return
     */
    public int getDataSetLocationAt(int loc) {
        int value = -1;
        List<Integer> list = getDataSetLocationAsInt();

        if (list != null && !list.isEmpty() && loc >= 0 && loc < list.size()) {
            value = list.get(loc);
        }

        return value;
    }

    public String getName() {
        return name;
    }

    public String getRegex() {
        return regex;
    }

    public void setCycleLocation(String cycleLocation) {
        this.cycleLocation = cycleLocation;
    }

    public void setDataSetLocation(String dataSetLocation) {
        this.dataSetLocation = dataSetLocation;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setRegex(String regex) {
        this.regex = regex;
    }

}
