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

package com.raytheon.edex.uengine.tasks.catalog;

import java.util.Date;
import java.util.List;

import org.hibernate.metadata.ClassMetadata;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.raytheon.uf.common.dataplugin.persist.PersistableDataObject;
import com.raytheon.uf.common.message.CatalogAttribute;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Provides catalog searching capability
 * 
 * 
 * 
 * <pre>
 * 
 *     SOFTWARE HISTORY
 *    
 *     Date         Ticket#     Engineer    Description
 *     ------------ ----------  ----------- --------------------------
 *     Nov 13, 2006             chammack    Initial Creation.
 *     Feb 19, 2007             garmendariz Modified to use database instead of Lucene
 *     Dec 12, 2007
 *     Dec 10, 2015 5166        kbisanz     Update logging to use SLF4J and
 *                                          fixed warnings
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class CatalogSearcher {

    /** The logger */
    protected final transient static Logger logger = LoggerFactory
            .getLogger(CatalogSearcher.class);

    public static final String TYPE_DISTINCT_VALUE = "distinctValue";

    public static final String TYPE_FULL_DOCUMENT = "document";

    /**
     * Search for all dataitems matching constraints
     * 
     * @param constraints
     *            the query constraints
     * @return the response
     * @throws EdexException
     */
    public static ResponseMessageCatalog search(List<String> fields,
            List<Object> values, List<String> operands) throws EdexException {
        return null;
        // return search(null, fields, values, operands, false);
    }

    /**
     * Perform a search:
     * 
     * Can perform a data item search or a parameter search based on
     * uniqueValues flag.
     * 
     * 
     * @param distinctFields
     *            the fields to return
     * @param constraints
     *            the query constraints
     * @param uniqueValues
     *            if true, perform a parameter query
     * @return the response
     * @throws EdexException
     */
    public static ResponseMessageCatalog search(DatabaseQuery query,
            String dbName, String queryClass) throws EdexException {
        ResponseMessageCatalog rmc = new ResponseMessageCatalog();
        List<?> queryResults = null;
        CoreDao dao = null;
        ClassMetadata metadata = null;

        // Instantiate the data access object
        try {
            dao = new CoreDao(DaoConfig.forClass(dbName, queryClass));
            metadata = dao.getDaoClassMetadata();
        } catch (ClassNotFoundException e) {
            throw new EdexException(
                    "Error instantiating data access object for: " + queryClass
                            + " on database: " + dbName);
        }

        // Execute the query
        queryResults = dao.queryByCriteria(query);

        // If no results returned, output logger information and return empty
        // response
        if (queryResults.isEmpty()) {
            logger.info("Catalog query returned 0 results");
            return rmc;
        }

        CatalogItem[] items = new CatalogItem[queryResults.size()];
        String[] values = new String[1];
        // If the results returned were not full records, i.e. a distinct value
        // query or a query for query for a subset of fields
        if (queryResults.get(0) instanceof Object[]) {

        } else {
            String[] propertyNames = metadata.getPropertyNames();
            for (int i = 0; i < queryResults.size(); i++) {
                PersistableDataObject currentRow = (PersistableDataObject) queryResults
                        .get(i);
                CatalogItem ci = new CatalogItem();
                ci.setKey(String.valueOf(currentRow.getIdentifier()));
                CatalogAttribute[] attributes = new CatalogAttribute[propertyNames.length];
                for (int j = 0; j < propertyNames.length; j++) {
                    attributes[j] = new CatalogAttribute(propertyNames[j], null);
                }
                ci.setAttributes(attributes);
                items[i] = ci;
            }

        }

        rmc.setItems(items);
        rmc.setDataURI("");
        rmc.setFileType("");
        rmc.setValues(values);
        rmc.setValidTime(new Date());

        return rmc;

    }

    public static ResponseMessageCatalog search(DatabaseQuery query)
            throws EdexException {
        ResponseMessageCatalog rmc = null;

        return rmc;

    }

    // PluginVersionDao pluginVersionDao = null;
    // CoreDao dataDao = null;
    // ResponseMessageCatalog rmc = null;
    // List<PluginVersion> pluginVersions = null;
    // List<?> queryResults = null;
    //
    // try {
    // pluginVersionDao = (PluginVersionDao) DaoPool.getInstance()
    // .borrowObject(PluginVersionDao.class);
    //
    // String plugin = null;
    // if (fields.contains("plugin")) {
    // int pluginIndex = fields.indexOf("plugin");
    // plugin = ((String) values.get(pluginIndex)).toLowerCase();
    // fields.remove(pluginIndex);
    // values.remove(pluginIndex);
    // operands.remove(pluginIndex);
    // } else {
    // throw new EdexException("Plugin name not specified");
    // }
    //
    // pluginVersions = pluginVersionDao.loadAllPluginVersions(plugin);
    //
    // // if we do not get a table back, just return an empty list
    // if (pluginVersions == null) {
    // logger.debug("No tables exist for plugin = " + plugin);
    // return new ResponseMessageCatalog();
    // }
    //
    // for(PluginVersion pluginVersion:pluginVersions){
    // if(pluginVersion.isDefaultClass()){
    // String daoClassName = pluginVersion.getDaoClass();
    // Class<?> daoClass = null;
    // String hibClass = pluginVersion.getHibClass();
    // try {
    // if (daoClassName == null) {
    // dataDao = DaoPool.getInstance().borrowObject(
    // DaoConfig.forClass(hibClass));
    // } else {
    // daoClass = CatalogSearcher.class.getClassLoader()
    // .loadClass(daoClassName);
    // dataDao = DaoPool.getInstance().borrowObject(
    // daoClass);
    // }
    // } catch (ClassNotFoundException e) {
    // throw new EdexException("Unable tolaod class", e);
    // }
    // break;
    // }
    // }
    //
    //
    // if (uniqueValues) {
    // queryResults = dataDao.queryByCriteria(fields, values,
    // operands, null, distinctFields.get(0), true,
    // distinctFields);
    // } else {
    // queryResults = dataDao
    // .queryByCriteria(fields, values, operands);
    // }
    //
    // if (queryResults == null || queryResults.size() == 0) {
    // return new ResponseMessageCatalog();
    // }
    //
    // // create a new response message catalog and catalog items based on
    // // the
    // // number of objects returned
    // rmc = new ResponseMessageCatalog();
    // CatalogItem[] items = new CatalogItem[queryResults.size()];
    // if (!uniqueValues) {
    // String propertyKey = null;
    // PluginDataObject obj = null;
    // for (int i = 0; i < items.length; i++) {
    //
    // obj = (PluginDataObject) queryResults.get(i);
    // CatalogItem ci = new CatalogItem();
    // ci.setKey(obj.getDataURI());
    //
    // Map<String, Object> propertyMap = new HashMap<String, Object>();
    //
    // try {
    // propertyMap = PropertyUtils.describe(obj);
    // } catch (Exception e) {
    // throw new EdexException("Unable to get properties for "
    // + obj.getClass(), e);
    // }
    //
    // CatalogAttribute[] attributes = new CatalogAttribute[propertyMap
    // .size()];
    // Iterator<String> it = propertyMap.keySet().iterator();
    // for (int j = 0; j < propertyMap.size(); j++) {
    // propertyKey = it.next();
    // attributes[j] = new CatalogAttribute(propertyKey,
    // propertyMap.get(propertyKey));
    // }
    // ci.setAttributes(attributes);
    // items[i] = ci;
    // }
    //
    // rmc.setItems(items);
    // rmc.setDataURI(plugin);
    // rmc.setFileType("");
    // rmc.setValues(new String[0]);
    // rmc.setValidTime(new Date());
    // } else {
    //
    // String[] vals = new String[queryResults.size()];
    //
    // // Dummy object used for examining fields and types
    // Object dataObject = null;
    // String propertyClass = null;
    // try {
    // for(PluginVersion ver: pluginVersions){
    // dataObject =
    // CatalogSearcher.class.getClassLoader().loadClass(ver.getHibClass()).newInstance();
    // try {
    // propertyClass = PropertyUtils.getPropertyType(dataObject,
    // distinctFields.get(0)).getSimpleName();
    // break;
    // } catch (Exception e2) {
    // //Property not found
    // }
    // }
    // } catch (Exception e1) {
    // throw new EdexException("Unable to instantiate class ["
    // + dataDao.getDaoClass() + "]");
    // }
    //
    //
    // GregorianCalendar cal = null;
    // for (int i = 0; i < queryResults.size(); i++) {
    //
    // if (queryResults.get(i) != null) {
    // if (propertyClass.contains("Calendar")) {
    // cal = (GregorianCalendar) queryResults.get(i);
    // vals[i] = new Timestamp(cal.getTimeInMillis())
    // .toString();
    //
    // } else {
    // vals[i] = queryResults.get(i).toString();
    // }
    // }
    // }
    // rmc.setValues(vals);
    // rmc.setDataURI("");
    // rmc.setFileType("");
    // rmc.setItems(new CatalogItem[0]);
    // rmc.setValidTime(new Date());
    // }
    // } finally {
    // if (pluginVersionDao != null) {
    // DaoPool.getInstance().returnObject(pluginVersionDao);
    // }
    //
    // if (dataDao != null) {
    // DaoPool.getInstance().returnObject(dataDao);
    // }
    //
    // }
    //
    // return rmc;
    //
    // }
}
