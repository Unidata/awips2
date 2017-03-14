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
package com.raytheon.uf.common.datalisting.impl;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import com.raytheon.uf.common.datalisting.DataListing;
import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.dataquery.requests.DbQueryRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.responses.DbQueryResponse;
import com.raytheon.uf.common.serialization.comm.RequestRouter;
import com.raytheon.uf.common.time.DataTime;

/**
 * 
 * A basic implementaion of a {@link DataListing} that uses {@link DataURI}
 * fields as keys and performs no special formatting. This can be extended
 * within a plugin to provide a more customized listing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 2, 2015            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class DefaultDataListing implements DataListing {

    private final String pluginName;

    private final Set<String> keySet;

    public DefaultDataListing(Class<? extends PluginDataObject> pdoClass) throws ReflectiveOperationException {
        this(getPluginName(pdoClass), getPluginKeys(pdoClass));
    }

    public DefaultDataListing(String pluginName, Set<String> keySet) {
        this.pluginName = pluginName;
        this.keySet = keySet;
    }

    public DefaultDataListing(String pluginName, List<String> keySet) {
        this(pluginName, new LinkedHashSet<>(keySet));
    }

    @Override
    public String getPluginName() {
        return pluginName;
    }

    @Override
    public Collection<String> getKeys() {
        return keySet;
    }

    @Override
    public Collection<String> getValues(String key, Map<String, String> keyVals) throws Exception {
        DbQueryRequest request = new DbQueryRequest();
        request.setConstraints(getRequestConstraints(keyVals));
        request.addRequestField(key);
        request.setDistinct(true);
        DbQueryResponse response = (DbQueryResponse) RequestRouter.route(request);
        Object[] paramObjs = response.getFieldObjects(key, Object.class);
        if (paramObjs != null) {
            List<String> params = new ArrayList<>(paramObjs.length);
            for (int i = 0; i < paramObjs.length; i += 1) {
                if (paramObjs[i] != null) {
                    params.add(paramObjs[i].toString());
                }
            }
            return params;
        }
        return Collections.emptyList();
    }

    @Override
    public Map<String, RequestConstraint> getRequestConstraints(Map<String, String> keyVals) {
        Map<String, RequestConstraint> constraints = new HashMap<>(keyVals.size(), 1.0f);
        constraints.put(PluginDataObject.PLUGIN_NAME_ID, new RequestConstraint(pluginName));
        for (Entry<String, String> keyVal : keyVals.entrySet()) {
            constraints.put(keyVal.getKey(), new RequestConstraint(keyVal.getValue()));
        }
        return constraints;
    }

    private static String getPluginName(Class<? extends PluginDataObject> pdoClass) throws ReflectiveOperationException {
        return pdoClass.newInstance().getPluginName();
    }

    private static Set<String> getPluginKeys(Class<?> pdoClass) {
        Set<String> keys = new LinkedHashSet<>();
        Class<?> currentClass = pdoClass;
        while (currentClass != null) {
            for (Field field : currentClass.getDeclaredFields()) {
                DataURI dataURI = field.getAnnotation(DataURI.class);
                if (dataURI == null || DataTime.class.equals(field.getType())) {
                    continue;
                }
                if (dataURI.embedded()) {
                    for (String key : getPluginKeys(field.getType())) {
                        keys.add(field.getName() + "." + key);
                    }
                } else {
                    keys.add(field.getName());
                }

            }
            currentClass = currentClass.getSuperclass();
        }
        return keys;
    }

    @Override
    public Map<String, String> getFormattedValues(String key, Map<String, String> keyVals) throws Exception {
        return getFormattedValues(key, getValues(key, keyVals));
    }

    protected Map<String, String> getFormattedValues(String key, Collection<String> values) {
        Map<String, String> result = new TreeMap<>();
        for (String value : values) {
            result.put(value, value);
        }
        return result;
    }

}
