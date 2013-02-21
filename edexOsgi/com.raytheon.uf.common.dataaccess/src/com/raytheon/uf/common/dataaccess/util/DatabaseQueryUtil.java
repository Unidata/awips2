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
package com.raytheon.uf.common.dataaccess.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.raytheon.uf.common.dataaccess.exception.DataRetrievalException;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.message.response.AbstractResponseMessage;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.RequestRouter;

/**
 * A utility used to run queries against a specified database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 29, 2013            bkowal     Initial creation
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public class DatabaseQueryUtil {
    /*
     * should this enum actually be provided by the QlServerRequest?
     */
    public static enum QUERY_MODE {
        MODE_SQLQUERY("sqlquery"), MODE_HQLQUERY("hqlquery"), MODE_SQL_STATEMENT(
                "sqlstatement"), MODE_HSQL_STATEMENT("hqlstatement"), MODE_SAVE_OR_UPDATE(
                "saveOrUpdateObject");

        private String modeText;

        QUERY_MODE(String modeText) {
            this.modeText = modeText;
        }

        protected String getModeText() {
            return this.modeText;
        }
    }

    private static final String CONSTRAINT_QUERY = "query";

    private static final String CONSTRAINT_DATABASE = "database";

    private static final String CONSTRAINT_MODE = "mode";

    /**
     * Constructor
     */
    private DatabaseQueryUtil() {
    }

    /**
     * Executes the provided query against the specified database and returns the results of the query execution.
     * 
     * @param mode the request mode
     * @param query the query to execute
     * @param database the database to execute the query against
     * @param dataType the Data Access Framework factory data type
     * @return the information retrieved from the database
     */
    public static List<Object[]> executeDatabaseQuery(QUERY_MODE mode,
            String query, String database, String dataType) {
        Map<String, RequestConstraint> requestConstraintMap = new HashMap<String, RequestConstraint>();
        requestConstraintMap
                .put(CONSTRAINT_QUERY, new RequestConstraint(query));
        requestConstraintMap.put(CONSTRAINT_DATABASE, new RequestConstraint(
                database));
        requestConstraintMap.put(CONSTRAINT_MODE,
                new RequestConstraint(mode.getModeText()));
        QlServerRequest serverRequest = new QlServerRequest(
                requestConstraintMap);

        final String errorMessage = "Error retrieving " + dataType + " data";

        // Execute the request.
        AbstractResponseMessage response = null;
        try {
            response = (AbstractResponseMessage) RequestRouter
                    .route(serverRequest);
        } catch (Exception e) {
            throw new DataRetrievalException(errorMessage, e);
        }

        QueryResult result = null;
        if (response instanceof ResponseMessageError) {
            throw new DataRetrievalException(errorMessage + ": "
                    + response.toString());
        } else if (response instanceof ResponseMessageGeneric) {
            result = (QueryResult) ((ResponseMessageGeneric) response)
                    .getContents();
        } else {
            throw new DataRetrievalException(
                    "Unable to process response of type" + response.getClass());
        }

        List<Object[]> unmappedResults = new ArrayList<Object[]>();
        for (QueryResultRow row : result.getRows()) {
            unmappedResults.add(row.getColumnValues());
        }
        return unmappedResults;
    }
}