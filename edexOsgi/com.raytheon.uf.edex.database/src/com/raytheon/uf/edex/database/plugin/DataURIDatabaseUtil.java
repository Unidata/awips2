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
package com.raytheon.uf.edex.database.plugin;

import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.annotations.DataURIUtil;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Utilities related to dataURIs and their corresponding database tables.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 21, 2014 2060       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DataURIDatabaseUtil {

    private DataURIDatabaseUtil() {

    }

    /**
     * Checks for the existence in the database of a PluginDataObject that
     * matches the argument. The check for existence is based on if the dataURI
     * of the PluginDataObject matches.
     * 
     * @param pdo
     *            the PluginDataObject to check to see if its equivalent dataURI
     *            is in the database
     * @return true if a matching dataURI was found, false otherwise
     * @throws PluginException
     */
    public static boolean existingDataURI(PluginDataObject pdo)
            throws PluginException {
        PluginDao dao = PluginFactory.getInstance().getPluginDao(
                pdo.getPluginName());
        DatabaseQuery dbQuery = new DatabaseQuery(pdo.getClass());
        Map<String, Object> dataUriFields = DataURIUtil.createDataURIMap(pdo);
        for (Map.Entry<String, Object> field : dataUriFields.entrySet()) {
            String fieldName = field.getKey();
            // ignore pluginName
            if (!PluginDataObject.PLUGIN_NAME_ID.equals(fieldName)) {
                dbQuery.addQueryParam(field.getKey(), field.getValue());
            }
        }

        try {
            List<?> list = dao.queryByCriteria(dbQuery);
            return (list != null && !list.isEmpty());
        } catch (DataAccessLayerException e) {
            throw new PluginException("Error querying database", e);
        }
    }

}
