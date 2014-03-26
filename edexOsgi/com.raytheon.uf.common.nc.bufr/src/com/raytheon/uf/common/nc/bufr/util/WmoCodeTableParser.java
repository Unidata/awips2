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
package com.raytheon.uf.common.nc.bufr.util;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import com.raytheon.uf.common.nc.bufr.tables.MappedValue;
import com.raytheon.uf.common.nc.bufr.tables.TableEntry;
import com.raytheon.uf.common.nc.bufr.tables.TranslationTable;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Parsing utility for WMO BUFR code table XML
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
public class WmoCodeTableParser {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(WmoCodeTableParser.class);

    public static final String FXY_ELEMENT = "FXY";

    public static final String TABLE_DESC_ELEMENT = "ElementName_en";

    public static final String ENTRY_ELEMENT_PREFIX = "BUFRCREX";

    public static final String INDEX_RANGE_ELEMENT = "CodeFigure";

    public static final String VALUE_ELEMENT = "EntryName_en";

    private static final XMLInputFactory staxFactory = XMLInputFactory
            .newInstance();

    private final XMLStreamReader reader;

    private static final Pattern FXY_PATTERN = Pattern
            .compile("(\\d)(\\d\\d)(\\d\\d\\d)");

    protected static final Pattern WHITESPACE = Pattern.compile("\\s");

    /**
     * @param bufrTables
     *            stream to WMO BUFR XML code table file
     * @throws XMLStreamException
     */
    public WmoCodeTableParser(InputStream bufrTables) throws XMLStreamException {
        this.reader = staxFactory.createXMLStreamReader(bufrTables);
    }

    /**
     * Closes open resources, must be called after parsing
     * 
     * @throws XMLStreamException
     */
    public void close() throws XMLStreamException {
        this.reader.close();
    }

    /**
     * Create TranslationTable objects for each table in WMO XML file
     * 
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @return
     * @throws XMLStreamException
     */
    public Collection<TranslationTable> parseAllTables(
            boolean createPlaceholders) throws XMLStreamException {
        return parseTables(Collections.<String> emptySet(), createPlaceholders);
    }

    /**
     * Create a TranslationTable object for each id in includedTables
     * 
     * @param includedTables
     *            list of included table ids in the format 'F XX YYY'
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @return
     * @throws XMLStreamException
     */
    public Collection<TranslationTable> parseTables(Set<String> includedTables,
            boolean createPlaceholders) throws XMLStreamException {
        Set<String> includedFXYs = unformatSet(includedTables);
        boolean parseAll = includedTables.isEmpty();
        List<TranslationTable> rval = new ArrayList<TranslationTable>();
        TranslationTable currentTable = null;
        String fxy = null;
        String desc = null;
        String index = null;
        String value = null;
        while (reader.hasNext()) {
            switch (reader.next()) {
            /* read text for each element in entry */
            case XMLStreamReader.START_ELEMENT:
                if (isElement(FXY_ELEMENT)) {
                    fxy = reader.getElementText().trim();
                } else if (isElement(TABLE_DESC_ELEMENT)) {
                    desc = reader.getElementText().trim();
                } else if (isElement(INDEX_RANGE_ELEMENT)) {
                    index = reader.getElementText().trim();
                } else if (isElement(VALUE_ELEMENT)) {
                    value = reader.getElementText().trim();
                }
                break;
            case XMLStreamReader.END_ELEMENT:
                /* we can only decide what to do after we see entire entry */
                if (elementHasPrefix(ENTRY_ELEMENT_PREFIX)) {
                    if (fxy == null) {
                        break;
                    }
                    if (currentTable == null || !isSameTable(currentTable, fxy)) {
                        if (currentTable != null) {
                            /* starting a new table, finish previous table */
                            finalizeTable(rval, currentTable, includedFXYs,
                                    parseAll);
                            if (includedFXYs.isEmpty()) {
                                /* early exit */
                                return rval;
                            }
                        }
                        currentTable = new TranslationTable(fxy, desc,
                                new ArrayList<TableEntry>());
                    }
                    /* some entries are just descriptive */
                    if (index != null && value != null) {
                        currentTable.addEntry(createEntry(index, value,
                                createPlaceholders));
                    }
                    fxy = desc = index = value = null;
                }
                break;
            }
        }
        /* take care of last table being processed */
        if (currentTable != null) {
            finalizeTable(rval, currentTable, includedFXYs, parseAll);
        }
        if (!includedFXYs.isEmpty()) {
            StringBuilder sb = new StringBuilder();
            for (String str : includedFXYs) {
                sb.append(str).append(", ");
            }
            log.warn("Requested BUFR table(s) not found in file: "
                    + sb.toString());
        }
        return rval;
    }

    /**
     * Add current table to storage if included in parsing result
     * 
     * @param storage
     * @param currentTable
     * @param includedFXYs
     * @param parseAll
     */
    private void finalizeTable(List<TranslationTable> storage,
            TranslationTable currentTable, Set<String> includedFXYs,
            boolean parseAll) {
        String currentName = currentTable.getBufrTable();
        if (parseAll || includedFXYs.contains(currentName)) {
            includedFXYs.remove(currentName);
            formatTableName(currentTable);
            storage.add(currentTable);
        }
    }

    /**
     * Create a table entry
     * 
     * @param indexRange
     * @param value
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @return
     */
    private TableEntry createEntry(String indexRange, String value,
            boolean createPlaceholders) {
        TableEntry entry = new TableEntry(indexRange, value,
                new ArrayList<MappedValue>());
        if (createPlaceholders) {
            entry.addMappedValue(new MappedValue("parameter", "value"));
        }
        return entry;
    }

    /**
     * Convert from conventional 'F XX YYY' format to 'FXXYYY' format used in
     * WMO XML
     * 
     * @param in
     * @return
     */
    private Set<String> unformatSet(Set<String> in) {
        Set<String> rval = new HashSet<String>(in.size());
        for (String value : in) {
            Matcher m = WHITESPACE.matcher(value);
            rval.add(m.replaceAll(""));
        }
        return rval;
    }

    /**
     * @param table
     * @param id
     * @return true if the translation table has the provided id
     */
    private boolean isSameTable(TranslationTable table, String id) {
        return table.getBufrTable().equals(id);
    }

    /**
     * Convert from 'FXXYYY' format used in WMO XML to conventional 'F XX YYY'
     * format
     * 
     * @param table
     */
    private void formatTableName(TranslationTable table) {
        String oldName = table.getBufrTable();
        Matcher m = FXY_PATTERN.matcher(oldName);
        String newName;
        if (m.matches()) {
            newName = m.group(1) + " " + m.group(2) + " " + m.group(3);
        } else {
            log.warn("Table name does not match expected format: " + oldName);
            newName = oldName;
        }
        table.setBufrTable(newName);
    }

    /**
     * @param name
     * @return true if the current element in the reader has name for the local
     *         name
     */
    private boolean isElement(String name) {
        QName currentElement = reader.getName();
        return currentElement.getLocalPart().equalsIgnoreCase(name);
    }

    /**
     * @param prefix
     * @return true if the reader's current element's local name starts with
     *         prefix
     */
    private boolean elementHasPrefix(String prefix) {
        QName currentElement = reader.getName();
        return currentElement.getLocalPart().startsWith(prefix);
    }

}
