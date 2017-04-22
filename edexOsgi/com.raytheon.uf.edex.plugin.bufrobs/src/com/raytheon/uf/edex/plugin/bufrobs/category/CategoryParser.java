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
package com.raytheon.uf.edex.plugin.bufrobs.category;

import java.util.HashMap;
import java.util.Map;

import javax.xml.namespace.QName;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.localization.exception.LocalizationException;

/**
 * Parsing utility for category to report type mapping configuration
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Apr 01, 2014  2906     bclement  Initial creation
 * Jul 11, 2016  5744     mapeters  Category files moved from edex_static to
 *                                  common_static
 * 
 * </pre>
 * 
 * @author bclement
 */
public class CategoryParser {

    private static final XMLInputFactory factory = XMLInputFactory
            .newInstance();

    public static final String localizationCategoryDirectory = "bufrobs"
            + IPathManager.SEPARATOR + "category";

    private static final String CAT_TAG = "category";

    private static final String SUBCAT_TAG = "subcategory";

    private static final String CODE_ATTRIB = "code";

    private static final String REPORT_TYPE_ATTRIB = "reportType";

    private CategoryParser() {
    }

    /**
     * Read XML configuration mapping category keys to report type integers from
     * localization
     * 
     * @param categoryFileName
     * @return
     * @throws XMLStreamException
     * @throws LocalizationException
     */
    public static Map<CategoryKey, Integer> getReportTypeMap(
            String categoryFileName) throws XMLStreamException,
            LocalizationException {
        LocalizationFile file = getLocalizedCategoryFile(categoryFileName);
        if (file == null) {
            return null;
        }
        Map<CategoryKey, Integer> rval = new HashMap<>();
        XMLStreamReader reader = factory.createXMLStreamReader(file
                .openInputStream());
        try {
            int currentCategoryCode = -1;
            while (reader.hasNext()) {
                switch (reader.next()) {
                case XMLStreamReader.START_ELEMENT:
                    QName name = reader.getName();
                    if (name.getLocalPart().equalsIgnoreCase(CAT_TAG)) {
                        currentCategoryCode = getIntAttrib(reader, CODE_ATTRIB);
                    } else if (name.getLocalPart().equalsIgnoreCase(SUBCAT_TAG)) {
                        int subCatCode = getIntAttrib(reader, CODE_ATTRIB);
                        int reportType = getIntAttrib(reader,
                                REPORT_TYPE_ATTRIB);
                        rval.put(new CategoryKey(currentCategoryCode,
                                subCatCode), reportType);
                    }
                }
            }
        } finally {
            reader.close();
        }
        return rval;
    }

    /**
     * Get integer attribute from STAX parser element
     * 
     * @param reader
     * @param name
     * @return
     * @throws XMLStreamException
     */
    private static int getIntAttrib(XMLStreamReader reader, String name)
            throws XMLStreamException {
        String valStr = reader.getAttributeValue(null, name);
        if (valStr == null) {
            throw new XMLStreamException("Tag " + reader.getName()
                    + " missing required attribute: " + name);
        }
        try {
            return Integer.valueOf(valStr);
        } catch (Exception e) {
            throw new XMLStreamException("Tag " + reader.getName()
                    + " invalid integer value for attribute: " + name);
        }
    }

    /**
     * Get category XML configuration from localization
     * 
     * @param categoryFileName
     * @return
     */
    public static LocalizationFile getLocalizedCategoryFile(
            String categoryFileName) {
        IPathManager pathMgr = PathManagerFactory.getPathManager();
        LocalizationContext commonStaticBase = pathMgr.getContext(
                LocalizationContext.LocalizationType.COMMON_STATIC,
                LocalizationContext.LocalizationLevel.BASE);
        String fileName = localizationCategoryDirectory
                + IPathManager.SEPARATOR + categoryFileName;
        return pathMgr.getLocalizationFile(commonStaticBase, fileName);
    }

}
