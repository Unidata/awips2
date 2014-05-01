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

import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * uEngine task to execute and arbitrary (non-query) sql statement
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
public class SqlStatementTask {

    /** The SQL statement string */
    private String sqlStatement;

    /** The database name */
    private String dbName;

    /**
     * Constructs a new SqlStatementTask with the provided SQL and database name
     * 
     * @param sqlStatement
     *            The sql to execute
     * @param dbName
     *            The database name
     */
    public SqlStatementTask(String sqlStatement, String dbName) {
        this.sqlStatement = sqlStatement;
        this.dbName = dbName;
    }

    /**
     * Constructs a new SqlStatementTask with the provided SQL on the default
     * database
     * 
     * @param sqlStatement
     *            The sql to execute
     */
    public SqlStatementTask(String sqlStatement) {
        this(sqlStatement, DaoConfig.DEFAULT_DB_NAME);
    }

    public Object execute() throws Exception {
        CoreDao dao = new CoreDao(DaoConfig.forDatabase(dbName));
        return dao.executeNativeSql(sqlStatement);
    }

}
