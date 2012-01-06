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
 * uEngine task to execute and arbitrary HQL statement
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
public class HqlQueryTask {

    /** The HQL query string */
    private String hqlQuery;

    /** The database query */
    private String dbName;

    /**
     * Creates a new HQL query with the given hql query on the specified
     * database
     * 
     * @param hqlQuery
     *            The HQL query
     * @param dbName
     *            The database to query
     */
    public HqlQueryTask(String hqlQuery, String dbName) {
        this.hqlQuery = hqlQuery;
        this.dbName = dbName;
    }

    /**
     * Creates a new HQL query for the default database
     * 
     * @param hqlQuery
     *            The HQL query
     */
    public HqlQueryTask(String hqlQuery) {
        this(hqlQuery, DaoConfig.DEFAULT_DB_NAME);
    }

    public QueryResult execute() throws Exception {
        return new CoreDao(DaoConfig.forDatabase(dbName))
                .executeHQLQuery(hqlQuery);
    }

}
