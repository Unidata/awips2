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
package com.raytheon.uf.common.nc.bufr.tables;

import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Entry in BUFR translation table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 24, 2014 2905       bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class TableEntry {

    @XmlAttribute(required = true)
    private String indexRange;

    @XmlAttribute(required = false)
    private String description;

    @XmlElement(name = "mappedValue")
    private List<MappedValue> mappedValues;

    /**
     * 
     */
    public TableEntry() {
    }

    /**
     * @param indexRange
     *            list of BUFR table indexes in the format
     *            '[0-9]+|[0-9]+-[0-9]+'
     * @param mappedValues
     *            storage of foreign values mapped to this index range
     */
    public TableEntry(String indexRange, List<MappedValue> mappedValues) {
        this.indexRange = indexRange;
        this.mappedValues = mappedValues;
    }

    /**
     * @param indexRange
     *            list of BUFR table indexes in the format
     *            '[0-9]+|[0-9]+-[0-9]+'
     * @param description
     *            BUFR table text for this index range
     * @param mappedValues
     *            storage of foreign values mapped to this index range
     */
    public TableEntry(String indexRange, String description,
            List<MappedValue> mappedValues) {
        this(indexRange, mappedValues);
        this.description = description;
    }

    /**
     * Append a foreign value to be mapped to this index range
     * 
     * @param value
     */
    public void addMappedValue(MappedValue value) {
        if (this.mappedValues == null) {
            this.mappedValues = new ArrayList<MappedValue>();
        }
        this.mappedValues.add(value);
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the mappedValues
     */
    public List<MappedValue> getMappedValues() {
        return mappedValues;
    }

    /**
     * @param mappedValues
     *            the mappedValues to set
     */
    public void setMappedValues(List<MappedValue> mappedValues) {
        this.mappedValues = mappedValues;
    }

    /**
     * @return the indexRange
     */
    public String getIndexRange() {
        return indexRange;
    }

    /**
     * @param indexRange
     *            the indexRange to set
     */
    public void setIndexRange(String indexRange) {
        this.indexRange = indexRange;
    }

}
