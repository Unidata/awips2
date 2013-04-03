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
package com.raytheon.uf.viz.core;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;

import org.apache.commons.beanutils.ConstructorUtils;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.request.GetPluginRecordMapRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.NoPluginException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;

/**
 * Factory implementation for creation and manipulation of PluginDataObjects
 * based on dataURIs
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 7/24/07      353         bphillip    Initial creation  
 * 10/8/2008    1532        bphillip    Refactored to incorporate annotation support
 * 
 * </pre>
 * 
 */
public class RecordFactory {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(RecordFactory.class);

    /** The singleton instance */
    private static RecordFactory instance = new RecordFactory();

    /** Map containing the pluginName/Record class pairs */
    private Map<String, Class<PluginDataObject>> defMap = new HashMap<String, Class<PluginDataObject>>();

    public static final String WILDCARD = "%";

    /**
     * Gets the singleton instance of the RecordFactory
     * 
     * @return The singleton instance of the RecordFactory
     */
    public static RecordFactory getInstance() {
        return instance;
    }

    /**
     * Creates the singleton instance of the RecordFactory
     */
    private RecordFactory() {
        try {
            loadDefMap();
        } catch (Exception e) {
            statusHandler.handle(Priority.WARN,
                    "Failed to load plugin record definitions", e);
        }
    }

    @SuppressWarnings("unchecked")
    private void loadDefMap() throws VizException {
        GetPluginRecordMapRequest req = new GetPluginRecordMapRequest();
        Map<String, String> pluginRecordMap = (Map<String, String>) ThriftClient
                .sendRequest(req);
        for (Map.Entry<String, String> entry : pluginRecordMap.entrySet()) {
            String pluginName = entry.getKey();
            String record = entry.getValue();
            if (record != null) {
                try {
                    Class<PluginDataObject> clazz = (Class<PluginDataObject>) Class
                            .forName(record);
                    defMap.put(pluginName, clazz);
                } catch (Exception e) {
                    statusHandler.handle(Priority.DEBUG,
                            "Can't find record class for " + pluginName
                                    + " plugin", e);
                    System.out.println("DEBUG: Can't find record class for "
                            + pluginName + " plugin - alerts on " + pluginName
                            + " data will be ignored");
                }
            }
        }
    }

    /**
     * Returns a collection of all supported plugins
     * 
     * @return
     */
    public Collection<String> getSupportedPlugins() {
        return new TreeSet<String>(defMap.keySet());
    }

    /**
     * Creates a map of the fields and values that compose a given dataURI
     * 
     * @param dataURI
     *            The dataURI to create the map for
     * @return The map of fields and values from the dataURI
     * @throws VizException
     *             If errors occur creating the field/value map
     */
    public Map<String, Object> loadMapFromUri(String dataURI)
            throws VizException {
        // If no dataURI return
        if (dataURI == null) {
            return null;
        }

        Map<String, Object> map = new HashMap<String, Object>();

        String[] tokens = dataURI.replaceAll("_", " ").split(DataURI.SEPARATOR);
        String pluginName = tokens[1];

        map.put("pluginName", pluginName);
        PluginDataObject obj = null;
        try {
            obj = this.getPluginClass(pluginName).newInstance();

            for (int i = 2; i < tokens.length; i++) {
                if (!tokens[i].equals("%") && !tokens[i].trim().isEmpty()) {
                    String fieldName = PluginDataObject.getDataURIFieldName(
                            obj.getClass(), i - 2);
                    if (fieldName == null) {
                        continue;
                    }
                    // fieldName = fieldName
                    // .substring(fieldName.lastIndexOf(".") + 1);
                    Object value = obj.getDataURIFieldValue(i - 2, tokens[i]);
                    map.put(fieldName, value);
                } else if (tokens[i].equals("%")) {
                    String fieldName = PluginDataObject.getDataURIFieldName(
                            obj.getClass(), i - 2);
                    // fieldName = fieldName
                    // .substring(fieldName.lastIndexOf(".") + 1);
                    map.put(fieldName, WILDCARD);
                }

            }
            map.put("dataURI", dataURI);
        } catch (NoPluginException e) {
            throw e;
        } catch (Exception e) {
            throw new VizException("Unable to create property map for "
                    + dataURI, e);
        }
        return map;
    }

    /**
     * Gets the record class associated with the give plugin. The record class
     * is extracted from the plugin.xml provided in the EDEX plugin client jar
     * 
     * @param pluginName
     *            The name of the plugin
     * @return The record class
     * @throws VizException
     *             If errors occur creating the field/value map
     */
    public Class<PluginDataObject> getPluginClass(String pluginName)
            throws VizException {
        Class<PluginDataObject> retVal = null;
        if (defMap != null) {
            retVal = defMap.get(pluginName);
        }
        if (retVal == null) {
            throw new NoPluginException("Can't find record class for "
                    + pluginName + " plugin");
        }
        return retVal;
    }

    /**
     * Creates a partially populated PluginDataObject from the given dataURI
     * 
     * @param dataURI
     *            The dataURI used for populating the object
     * @return A PluginDataObject populated from the provided dataURI
     * @throws VizException
     *             If the PluginDataObject cannot be created
     */
    public PluginDataObject loadRecordFromUri(String dataURI)
            throws VizException {

        String pluginName = dataURI.substring(dataURI.indexOf("/") + 1,
                dataURI.indexOf("/", 2));
        PluginDataObject record = null;
        try {
            record = (PluginDataObject) ConstructorUtils
                    .invokeExactConstructor(getPluginClass(pluginName), dataURI);
        } catch (Exception e) {
            throw new VizException("Unable to instantiate record class", e);
        }
        return record;
    }
}