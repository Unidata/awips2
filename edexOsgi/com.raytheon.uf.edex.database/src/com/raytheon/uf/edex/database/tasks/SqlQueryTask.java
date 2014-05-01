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

package com.raytheon.uf.edex.database.tasks;

import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * uEngine task to execute and arbitrary sql statement
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 10/15/08     1615        bphillip    Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 */
public class SqlQueryTask {

    /** The HQL query string */
    private String sqlQuery;

    /** The database query */
    private String dbName;

    /**
     * Creates a new sql query with the given sql query on the specified
     * database
     * 
     * @param sqlQuery
     *            The sql query
     * @param dbName
     *            The database to query
     */
    public SqlQueryTask(String sqlQuery, String dbName) {
        this.sqlQuery = sqlQuery;
        this.dbName = dbName;
    }

    /**
     * Creates a new sql query for the default database
     * 
     * @param sqlQuery
     *            The sql query
     */
    public SqlQueryTask(String sqlQuery) {
        this(sqlQuery, DaoConfig.DEFAULT_DB_NAME);
    }

    public QueryResult execute() throws Exception {
        return (QueryResult) new CoreDao(DaoConfig.forDatabase(dbName))
                .executeNativeSql(sqlQuery);
    }

}
