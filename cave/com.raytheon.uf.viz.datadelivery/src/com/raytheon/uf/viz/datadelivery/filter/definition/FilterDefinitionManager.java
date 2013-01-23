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
package com.raytheon.uf.viz.datadelivery.filter.definition;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Set;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationFile;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.DataTypeFilterElementXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.DataTypeFilterXML;
import com.raytheon.uf.viz.datadelivery.filter.definition.xml.FilterXML;

/**
 * Filter Definition Manager.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 20, 2012            mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class FilterDefinitionManager {
    /** Single instance of this class */
    private static FilterDefinitionManager instance = null;

    /** 
     * Holds the filters associated with a specific data type. 
     */
    private static final String DATATYPE_XML_FILE = "DataTypeFilter.xml";

    /** 
     * Holds the configuration info for each filter.
     */
    private static final String FILTER_XML_FILE = "Filter.xml";

    /** XML Object */
    private DataTypeFilterXML dataTypeXml = new DataTypeFilterXML();

    /** XML Object */
    private FilterXML filterXml = new FilterXML();

    /** JAXB context */
    private JAXBContext jax;

    /** Unmarshaller object */
    private Unmarshaller unmarshaller;

    /**
     * Private constructor.
     */
    private FilterDefinitionManager() {
        createContext();
        readXML();
    }

    /**
     * Gets the one instance of this class
     * 
     * @return The singleton instance of this class
     */
    public static FilterDefinitionManager getInstance() {
        if (instance == null) {
            instance = new FilterDefinitionManager();
        }
        return instance;
    }

    /**
     * Create the JAXB context
     */
    @SuppressWarnings("rawtypes")
    private void createContext() {
        Class[] classes =
                new Class[] { DataTypeFilterXML.class,
                        DataTypeFilterElementXML.class, FilterXML.class };

        try {
            jax = JAXBContext.newInstance(classes);
            this.unmarshaller = jax.createUnmarshaller();
        } catch (JAXBException e) {
            // statusHandler.handle(Priority.PROBLEM, e.getLocalizedMessage(),
            // e);
            e.printStackTrace();
        }
    }

    /**
     * Read the data type and filter xml files
     */
    private void readXML() {
        try {
            String dataTypeFileName =
                    "DataDelivery" + File.separator + DATATYPE_XML_FILE;
            String filterFileName =
                    "DataDelivery" + File.separator + FILTER_XML_FILE;

            IPathManager pm = PathManagerFactory.getPathManager();

            LocalizationFile dataTypeLocFile =
                    pm.getStaticLocalizationFile(dataTypeFileName);
            LocalizationFile filterLocFile =
                    pm.getStaticLocalizationFile(filterFileName);

            File dataTypeFile = null;
            if (dataTypeLocFile != null) {
                dataTypeFile = dataTypeLocFile.getFile();
            }

            File filterFile = null;
            if (filterLocFile != null) {
                filterFile = filterLocFile.getFile();
            }

            if (dataTypeFile != null) {
                dataTypeXml =
                        (DataTypeFilterXML) unmarshaller
                                .unmarshal(dataTypeFile);
            } else {
                dataTypeXml = new DataTypeFilterXML();
            }

            if (filterFile != null) {
                filterXml = (FilterXML) unmarshaller.unmarshal(filterFile);
            } else {
                filterXml = new FilterXML();
            }
        } catch (JAXBException e1) {
            e1.printStackTrace();
            if (dataTypeXml == null) {
                dataTypeXml = new DataTypeFilterXML();
            }
            if (filterXml == null) {
                filterXml = new FilterXML();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Find the common items in the ArrayLists
     * 
     * @param allDataTypes HashMap of ArrayLists to search
     *          for common items 
     * @return ArrayList of common items
     */
    public ArrayList<String> findCommon(
            HashMap<String, ArrayList<String>> allDataTypes) {
        ArrayList<String> common = new ArrayList<String>();
        LinkedHashSet<String> tmp = new LinkedHashSet<String>();

        if (allDataTypes.size() == 1) {
            Set<String> keySet = allDataTypes.keySet();

            for (String s : keySet) {
                common = allDataTypes.get(s);
            }

            return common;
        }

        int i = 0;
        Set<String> keySet = allDataTypes.keySet();

        for (String key : keySet) {
            if (i == 0) {
                common.addAll(allDataTypes.get(key));
                i++;
            } else {
                ArrayList<String> arrayFromAllArray = allDataTypes.get(key);

                tmp.clear();

                for (int j = 0; j < common.size(); j++) {
                    if (arrayFromAllArray.contains(common.get(j))) {
                        tmp.add(common.get(j));
                    }
                }

                for (int j = 0; j < arrayFromAllArray.size(); j++) {
                    if (common.contains(arrayFromAllArray.get(j))) {
                        tmp.add(arrayFromAllArray.get(j));
                    }
                }

                common.clear();
                common.addAll(tmp);
            }
        }

        return common;
    }

    /**
     * @return the dataTypeXml
     */
    public DataTypeFilterXML getDataTypeXml() {
        return dataTypeXml;
    }

    /**
     * @return filterXML object
     */
    public FilterXML getFilterXml() {
        return filterXml;
    }
}
