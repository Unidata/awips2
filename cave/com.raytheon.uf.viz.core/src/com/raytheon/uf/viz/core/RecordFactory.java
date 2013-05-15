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

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
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
 * Jul 24, 2007 353         bphillip    Initial creation
 * Oct 08, 2008 1532        bphillip    Refactored to incorporate annotation
 *                                      support
 * Mar 05, 2013 1753        njensen     Improved debug message
 * Mar 29, 2013 1638        mschenke    Added dataURI mapping methods
 * May 15, 2013 1869        bsteffen    Move uri map creation to DataURIUtil.
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
                } catch (ClassNotFoundException e) {
                    String msg = "Can't find record class for " + pluginName
                            + " plugin - alerts on " + pluginName
                            + " data will be ignored";
                    statusHandler.handle(Priority.DEBUG, msg);
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

        String[] tokens = dataURI.split(DataURI.SEPARATOR, 3);
        String pluginName = tokens[1];

        try {
            Map<String, Object> map = DataURIUtil.createDataURIMap(dataURI,
                    getPluginClass(pluginName));
            map.put("dataURI", dataURI);
            return map;
        } catch (NoPluginException e) {
            throw e;
        } catch (Exception e) {
            throw new VizException("Unable to create property map for "
                    + dataURI, e);
        }
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
        return loadRecordFromMap(loadMapFromUri(dataURI));
    }

    /**
     * Creates a partially populated PluginDataObject from the given a map of
     * attributes
     * 
     * @param map
     *            The map used for populating the object
     * @return A PluginDataObject populated from the provided dataURI
     * @throws VizException
     *             If the PluginDataObject cannot be created
     */
    public PluginDataObject loadRecordFromMap(Map<String, Object> map)
            throws VizException {
        Class<PluginDataObject> pdoClass = getPluginClass((String) map
                .get("pluginName"));
        if (pdoClass == null) {
            throw new VizException(
                    "Unable to load record from dataURI, PDO class for plugin ("
                            + map.get("pluginName") + ") not found");
        }

        return loadRecordFromMap(map, pdoClass);
    }

    /**
     * Populates a record type object from map
     * 
     * @param map
     * @param type
     * @return
     * @throws VizException
     */
    public <T> T loadRecordFromMap(Map<String, Object> map, Class<T> type)
            throws VizException {
        T record;
        try {
            record = type.newInstance();
        } catch (Exception e) {
            throw new VizException("Unable to create new record for type: "
                    + type, e);
        }
        try {
            PluginDataObject.populateFromMap(record, map);
        } catch (PluginException e) {
            throw new VizException(e);
        }
        return record;
    }
}