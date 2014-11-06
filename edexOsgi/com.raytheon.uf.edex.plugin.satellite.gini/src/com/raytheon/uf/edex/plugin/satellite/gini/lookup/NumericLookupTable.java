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
package com.raytheon.uf.edex.plugin.satellite.gini.lookup;

import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * Config XML object for GINI satellite numerically indexed table
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 4, 2014  2714      bclement     Initial creation
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class NumericLookupTable {

    @XmlAttribute(required = true)
    private String id;

    @XmlElement(name = "entry")
    private List<NumericTableEntry> entries;

    private transient volatile String[] indexedValues;

    /**
     * 
     */
    public NumericLookupTable() {
    }

    /**
     * @param key
     * @return value associated with key or null if not found
     */
    public String lookup(int key) {
        if (entries == null || entries.isEmpty()) {
            return null;
        }
        if (indexedValues == null) {
            synchronized (this) {
                if (indexedValues == null) {
                    indexedValues = indexEntries(entries);
                }
            }
        }
        String rval = null;
        if (key > -1 && key < indexedValues.length) {
            rval = indexedValues[key];
        }
        return rval;
    }

    /**
     * Create an array large enough to fit all table elements and populate from
     * entries. Array may have null values.
     * 
     * @param entries
     * @return
     */
    private String[] indexEntries(List<NumericTableEntry> entries) {
        /* loop to account for out of place entries */
        int max = Integer.MIN_VALUE;
        for (NumericTableEntry entry : entries) {
            max = Math.max(max, entry.getIndex());
        }
        /* populate lookup table which may have holes */
        String[] rval = new String[max + 1];
        for (NumericTableEntry entry : entries) {
            rval[entry.getIndex()] = entry.getValue();
        }
        return rval;
    }

    /**
     * @return the id
     */
    public String getId() {
        return id;
    }

    /**
     * @param id
     *            the id to set
     */
    public void setId(String id) {
        this.id = id;
    }

    /**
     * @return the entries
     */
    public List<NumericTableEntry> getEntries() {
        return entries;
    }

    /**
     * @param entries
     *            the entries to set
     */
    public void setEntries(List<NumericTableEntry> entries) {
        this.entries = entries;
    }

}
