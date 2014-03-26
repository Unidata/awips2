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
 * Represents a BUFR table that also has foreign values mapped to table entries.
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
public class TranslationTable {

    @XmlAttribute(required = true)
    private String bufrTable;

    @XmlAttribute(required = false)
    private String description;

    @XmlElement(name = "entry")
    private List<TableEntry> entries;

    /**
     * 
     */
    public TranslationTable() {
    }

    /**
     * @param bufrTable
     *            BUFR table identifier (FXY) in the format 'F XX YYY'
     * @param entries
     *            storage for table entries
     */
    public TranslationTable(String bufrTable, List<TableEntry> entries) {
        this.bufrTable = bufrTable;
        this.entries = entries;
    }

    /**
     * @param bufrTable
     *            BUFR table identifier (FXY) in the format 'F XX YYY'
     * @param description
     *            human readable name of table
     * @param entries
     *            storage for table entries
     */
    public TranslationTable(String bufrTable, String description,
            List<TableEntry> entries) {
        this(bufrTable, entries);
        this.description = description;
    }

    /**
     * Append new entry to entries
     * 
     * @param entry
     */
    public void addEntry(TableEntry entry) {
        if (this.entries == null) {
            this.entries = new ArrayList<TableEntry>();
        }
        this.entries.add(entry);
    }

    /**
     * @return the bufrTable
     */
    public String getBufrTable() {
        return bufrTable;
    }

    /**
     * @param bufrTable
     *            the bufrTable to set
     */
    public void setBufrTable(String bufrTable) {
        this.bufrTable = bufrTable;
    }

    /**
     * @return the entries
     */
    public List<TableEntry> getEntries() {
        return entries;
    }

    /**
     * @param entries
     *            the entries to set
     */
    public void setEntries(List<TableEntry> entries) {
        this.entries = entries;
    }

}
