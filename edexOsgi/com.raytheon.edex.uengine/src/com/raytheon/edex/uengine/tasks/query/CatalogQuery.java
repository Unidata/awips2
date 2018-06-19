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

package com.raytheon.edex.uengine.tasks.query;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.raytheon.uf.common.dataquery.db.QueryParam;
import com.raytheon.uf.common.message.CatalogAttribute;
import com.raytheon.uf.common.message.CatalogItem;
import com.raytheon.uf.common.message.response.ResponseMessageCatalog;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * 
 * Task for querying an arbitrary table in the database. This class is intended
 * to be used when entire records need not be returned, only a subset of columns
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    6/05/08       #875        bphillip    Initial Creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1
 */
public class CatalogQuery extends DbQuery {

    private static final IUFStatusHandler logger = UFStatus
            .getHandler(CatalogQuery.class);

    private static final long LOG_TIME_THRESHOLD = 200; // milliseconds

    /**
     * Creates a new CatalogQuery
     * 
     * @param database
     *            The database to look at
     * @param aClassName
     *            The class to query for
     * @throws ClassNotFoundException
     */
    public CatalogQuery(String database, String aClassName) {
        super(database, aClassName);
    }

    /**
     * Execute the query.
     * 
     * @throws Exception
     */
    @SuppressWarnings("unchecked")
    @Override
    public ResponseMessageCatalog execute() throws Exception {

        if (dao == null) {
            dao = new CoreDao(DaoConfig.forClass(database, className));
        }
        ResponseMessageCatalog rmc = new ResponseMessageCatalog();

        List<Object[]> results = new ArrayList<Object[]>();
        try {
            long t0 = System.currentTimeMillis();
            results = (List<Object[]>) dao.queryByCriteria(query);
            long t1 = System.currentTimeMillis();
            if (t1 - t0 > LOG_TIME_THRESHOLD) {
                logger.info("CatalogQuery on " + query.getEntityName()
                        + " took " + (t1 - t0) + "ms");
            }
        } catch (Throwable e) {
            // Temporary logging info for query failures
            List<QueryParam> params = query.getParameters();
            String errorMsg = "";
            errorMsg += "\n_______________________\n";
            errorMsg += "CatalogQuery failure details: \n";
            errorMsg += "Database: " + this.database + "\n";
            errorMsg += "Class: " + this.className + "\n";
            errorMsg += "Query Details: \n";
            errorMsg += "Parameters:\n";
            for (QueryParam param : params) {
                errorMsg += param.toString() + "\n";
            }
            errorMsg += "Distinct Parameter: " + query.getDistinctParameter()
                    + "\n";
            errorMsg += "HQL query: " + query.createHQLQuery();
            errorMsg += "\n_______________________\n";

            logger.error(errorMsg);
        }

        CatalogItem[] itemList = new CatalogItem[results.size()];
        String[] values = null;
        if (query.getDistinctParameter() != null) {
            values = new String[results.size()];
        }
        for (int i = 0; i < results.size(); i++) {
            Object[] row = null;

            if (query.getReturnedFieldNames().size() == 1) {
                row = new Object[1];
                row[0] = results.get(i);
            } else {
                row = results.get(i);
            }

            itemList[i] = new CatalogItem();
            itemList[i].setKey(String.valueOf(i));

            CatalogAttribute[] attrs = new CatalogAttribute[row.length];
            for (int j = 0; j < row.length; j++) {
                attrs[j] = new CatalogAttribute();
                attrs[j].setField(query.getReturnedFieldNames().get(j));
                if (row[j] != null) {
                    attrs[j].setValue(row[j].toString());
                } else {
                    attrs[j].setValue(null);
                }
            }
            itemList[i].setAttributes(attrs);

            if (query.getDistinctParameter() != null) {
                values[i] = row[0].toString();
            }

        }
        rmc.setItems(itemList);
        rmc.setDataURI("");
        rmc.setFileType("");
        rmc.setValues(values);
        rmc.setValidTime(new Date());
        return rmc;
    }
}
