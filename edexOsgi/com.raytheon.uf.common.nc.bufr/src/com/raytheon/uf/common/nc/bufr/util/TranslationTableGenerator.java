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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Set;
import java.util.regex.Matcher;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.stream.XMLStreamException;

import com.raytheon.uf.common.nc.bufr.tables.TranslationTable;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Utility to generate initial translation table XML files from WMO BUFR code
 * table XML document.
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
public class TranslationTableGenerator {

    private static final IUFStatusHandler log = UFStatus
            .getHandler(TranslationTableGenerator.class);

    public static final String XML_TABLE_RESOURCE = "res/tables/BUFRCREX_21_0_0_CodeFlag_en.xml";

    /**
     * Load default WMO BUFR tables XML file into inputstream
     * 
     * @return
     * @throws IOException
     */
    public static InputStream getBufrTables() throws IOException {
        ClassLoader loader = TranslationTableGenerator.class.getClassLoader();
        InputStream rval = loader.getResourceAsStream(XML_TABLE_RESOURCE);
        if (rval == null) {
            File f = new File(XML_TABLE_RESOURCE);
            if (f.exists()) {
                rval = new FileInputStream(f);
            } else {
                log.error("Unable to find bufr tables: " + XML_TABLE_RESOURCE);
                throw new FileNotFoundException("Unable to find bufr tables: "
                        + XML_TABLE_RESOURCE);
            }
        }
        return rval;
    }

    /**
     * Generate base translation table XML files
     * 
     * @param destDir
     *            destination directory for XML files
     * @param includedTables
     *            list of included table ids in the format 'F XX YYY'
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @throws XMLStreamException
     * @throws JAXBException
     * @throws IOException
     */
    public static void generate(File destDir, Set<String> includedTables,
            boolean createPlaceholders) throws XMLStreamException,
            JAXBException, IOException {
        InputStream bufrTables = getBufrTables();
        try {
            generate("", destDir, includedTables, createPlaceholders,
                    bufrTables);
        } finally {
            bufrTables.close();
        }
    }

    /**
     * Generate base translation table XML files
     * 
     * @param tableFilePrefix
     *            string that will prefix generated files
     * @param destDir
     *            destination directory for XML files
     * @param includedTables
     *            list of included table ids in the format 'F XX YYY'
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @param bufrTables
     *            stream to WMO BUFR XML code tables
     * @throws XMLStreamException
     * @throws JAXBException
     */
    public static void generate(String tableFilePrefix, File destDir,
            Set<String> includedTables, boolean createPlaceholders,
            InputStream bufrTables) throws XMLStreamException, JAXBException {
        Collection<TranslationTable> tables = createTables(includedTables,
                createPlaceholders, bufrTables);
        Marshaller marsh = getMarshaller();
        for (TranslationTable table : tables) {
            StringBuilder sb = new StringBuilder();
            if (tableFilePrefix != null && !tableFilePrefix.isEmpty()) {
                sb.append(tableFilePrefix).append('-');
            }
            sb.append(replaceWhiteSpace(table.getBufrTable(), "_"));
            sb.append(".xml");
            File tableFile = new File(destDir, sb.toString());
            log.info("Writing translation table to "
                    + tableFile.getAbsolutePath());
            marsh.marshal(table, tableFile);
        }
    }

    /**
     * Replace all white space characters in str with replacement
     * 
     * @param str
     * @param replacement
     * @return
     */
    private static String replaceWhiteSpace(String str, String replacement) {
        Matcher m = WmoCodeTableParser.WHITESPACE.matcher(str);
        return m.replaceAll(replacement);
    }

    /**
     * Get JAXB marshaller (caches result)
     * 
     * @return
     * @throws JAXBException
     */
    private static Marshaller getMarshaller() throws JAXBException {
        JAXBContext context = JAXBContext.newInstance(TranslationTable.class);
        Marshaller marshaller = context.createMarshaller();
        marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, true);
        return marshaller;
    }

    /**
     * Read WMO BUFR code tables file and create TranslationTable JAXB objects
     * 
     * @param includedTables
     *            list of included table ids in the format 'F XX YYY'
     * @param createPlaceholders
     *            if true, placeholders for mapped values will be created in
     *            each table entry
     * @param bufrTables
     *            stream to WMO BUFR XML code tables
     * @return
     * @throws XMLStreamException
     */
    public static Collection<TranslationTable> createTables(
            Set<String> includedTables, boolean createPlaceholders,
            InputStream bufrTables) throws XMLStreamException {

        Collection<TranslationTable> rval;
        WmoCodeTableParser parser = new WmoCodeTableParser(bufrTables);
        try {
            log.info("Starting to process bufr tables");
            rval = parser.parseTables(includedTables, createPlaceholders);
            log.info("Done processing bufr tables");
        } finally {
            parser.close();
        }
        return rval;
    }

}
