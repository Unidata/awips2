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

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
 * Apr 03, 2014 2905       bclement     added lookup methods
 * Apr 07, 2014 2905       bclement     removed java 1.7 specific Integer.compare()
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
@XmlRootElement
public class TranslationTable {

    public static enum TableType {
        CODE, FLAG;
    }

    @XmlAttribute(required = true)
    private String bufrTable;

    @XmlAttribute(required = false)
    private String description;

    @XmlElement(name = "entry")
    private List<TableEntry> entries;

    public static final Pattern ALL_PATTERN = Pattern
            .compile("^[Aa]ll\\s+(\\d+)$");

    public static final Pattern RANGE_PATTERN = Pattern
            .compile("^(\\d+)-(\\d+)$");

    public static final Pattern SINGLE_PATTERN = Pattern.compile("^\\d+$");

    private transient volatile ArrayList<TableEntry> indexedEntries;

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
     * Lookup table entries for given key. Table will be interpreted differently
     * depending on the table type. Code tables are a one-to-one mapping of
     * index to value. Flag tables are bitmaps where values are returned for
     * every set bit in the key.
     * 
     * @param key
     * @param type
     * @return
     */
    public List<TableEntry> lookupEntries(int key, TableType type) {
        if (TableType.CODE.equals(type)) {
            return lookupCodeEntries(key);
        } else if (TableType.FLAG.equals(type)) {
            return lookupFlagEntries(key);
        } else {
            throw new InvalidParameterException("Invalid table type: " + type);
        }
    }

    /**
     * Get table entry at key index
     * 
     * @param key
     * @return
     */
    protected List<TableEntry> lookupCodeEntries(int key) {
        ArrayList<TableEntry> indexed = getIndexedEntries();
        if (key < 0 || key >= indexed.size()) {
            return Collections.emptyList();
        }
        return Arrays.asList(indexed.get(key));
    }

    /**
     * Flag table keys are bit maps stored in an integer. The bits are aligned
     * so that the first bit in the bitmap is at the most significant bit of the
     * integer. This method walks the bitmap in the key and returns every table
     * entry that has a set bit in the bitmap. However, if the last bit in the
     * bitmap is set, only a special table entry for Missing Value is returned.
     * 
     * @param key
     *            unsigned bitmap
     * @return
     */
    protected List<TableEntry> lookupFlagEntries(int key) {
        ArrayList<TableEntry> indexed = getIndexedEntries();
        final int lastIndex = indexed.size() - 1;
        /* the set bit starts at 1, we want to move size - 1 bits over */
        int mask = (0x80000000 >>> lastIndex);
        List<TableEntry> rval;
        if ((key & mask) != 0) {
            /* last entry in table is Missing Value, return entry at that index */
            rval = Arrays.asList(indexed.get(lastIndex));
        } else {
            /* not size of total table because missing value won't be included */
            rval = new ArrayList<TableEntry>(lastIndex);
            /* move back one from the missing value bit */
            mask <<= 1;
            /* walk table backwards adding included values */
            for (int i = lastIndex - 1; i > -1; --i, mask <<= 1) {
                if ((key & mask) != 0) {
                    rval.add(indexed.get(i));
                }
            }
        }
        return rval;
    }

    /**
     * Get processed entries ordered by index. Results are cached.
     * 
     * @return
     */
    protected ArrayList<TableEntry> getIndexedEntries() {
        if (entries == null) {
            return new ArrayList<TableEntry>(0);
        }
        if (indexedEntries == null) {
            synchronized (this) {
                if (indexedEntries == null) {
                    indexedEntries = createCodeList(entries);
                }
            }
        }
        return indexedEntries;
    }

    /*
     * priority queue node for ordered table entries
     */
    private static class QueueNode {
        public final int index;

        public final TableEntry entry;

        public static final Comparator<QueueNode> comp = new Comparator<QueueNode>() {
            @Override
            public int compare(QueueNode o1, QueueNode o2) {
                return o1.index - o2.index;
            }
        };

        public QueueNode(int index, TableEntry entry) {
            this.index = index;
            this.entry = entry;
        }
    }

    /**
     * Process table entries by ordering entries by index and placing them in an
     * array-backed list for easy indexing
     * 
     * @param entries
     * @return
     */
    private ArrayList<TableEntry> createCodeList(List<TableEntry> entries) {
        /* put in heap first to fix any order issues in the XML */
        PriorityQueue<QueueNode> queue = new PriorityQueue<QueueNode>(
                entries.size(), QueueNode.comp);
        for (TableEntry entry : entries) {
            String indexRange = entry.getIndexRange().trim();
            Matcher m = SINGLE_PATTERN.matcher(indexRange);
            if (m.matches()) {
                int index = Integer.valueOf(indexRange);
                queue.add(new QueueNode(index, entry));
            } else if ((m = RANGE_PATTERN.matcher(indexRange)).matches()) {
                int start = Integer.parseInt(m.group(1));
                int end = Integer.parseInt(m.group(2));
                for (int i = start; i <= end; ++i) {
                    queue.add(new QueueNode(i, entry));
                }
            } else if (((m = ALL_PATTERN.matcher(indexRange)).matches())) {
                int index = Integer.valueOf(m.group(1));
                queue.add(new QueueNode(index, entry));
            } else {
                throw new IllegalStateException("Invalid index range value: "
                        + indexRange);
            }
        }
        /* populate lookup table and verify table integrity */
        ArrayList<TableEntry> rval = new ArrayList<TableEntry>(queue.size());
        int prev = -1;
        for (QueueNode node : queue) {
            if (prev > -1 && prev != (node.index - 1)) {
                throw new IllegalStateException("Invalid lookup table "
                        + bufrTable
                        + ", table must have contiguous index ranges");
            }
            rval.add(node.entry);
            prev = node.index;
        }
        return rval;
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
