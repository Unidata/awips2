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
package com.raytheon.uf.edex.database;

import java.util.Map;

import com.raytheon.uf.common.dataquery.requests.QlServerRequest;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.tasks.HqlQueryTask;
import com.raytheon.uf.edex.database.tasks.HqlStatementTask;
import com.raytheon.uf.edex.database.tasks.SqlQueryTask;
import com.raytheon.uf.edex.database.tasks.SqlStatementTask;

/**
 * Handler for QlServerRequest objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 16, 2011 #8070      ekladstrup  Initial creation
 * Nov 08, 2013  2361      njensen     Removed saveOrUpdate mode
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1.0
 */
public class QlServerRequestHandler implements IRequestHandler<QlServerRequest> {

    /*
     * macros from VM_global_library.vm
     * 
     * #macro(hqlQuery $scriptMetadata) import HqlQuery request =
     * HqlQuery.HqlQuery
     * ("$scriptMetadata.remove("query").constraintValue","$scriptMetadata.remove("
     * database").constraintValue") return request.execute() #end
     * 
     * #macro(sqlQuery $scriptMetadata) import SqlQuery request =
     * SqlQuery.SqlQuery
     * ("$scriptMetadata.remove("query").constraintValue","$scriptMetadata.remove("
     * database").constraintValue") return request.execute() #end
     * 
     * #macro(sqlStatement $scriptMetadata) import SqlStatement request =
     * SqlStatement
     * .SqlStatement("$scriptMetadata.remove("query").constraintValue"
     * ,"$scriptMetadata.remove("database").constraintValue") return
     * request.execute() #end
     * 
     * #macro(hqlStatement $scriptMetadata) import HqlStatement request =
     * HqlStatement
     * .HqlStatement("$scriptMetadata.remove("query").constraintValue"
     * ,"$scriptMetadata.remove("database").constraintValue") return
     * request.execute() #end
     * 
     * #macro(saveOrUpdateObject $scriptMetadata) import SaveOrUpdateObject
     * request =
     * SaveOrUpdateObject.SaveOrUpdateObject("$scriptMetadata.remove("dbName
     * ").constraintValue")
     * 
     * #foreach (${obj} in ${scriptMetadata.values()}) #if($obj.constraintValue
     * != "satellite") request.addObject("$obj.constraintValue") #end #end
     * 
     * return request.execute() #end
     */

    /**
     * 
     */
    @Override
    public Object handleRequest(QlServerRequest request) throws Exception {
        // get request constraint map first
        Map<String, RequestConstraint> map = request.getRcMap();

        // get mode from map
        String mode = map.get("mode").getConstraintValue();
        map.remove("mode");

        // get database name
        // set default first
        String dbName = DaoConfig.DEFAULT_DB_NAME;
        if (map.containsKey("database")) {
            dbName = map.get("database").getConstraintValue();
            map.remove("database");
        }

        // get query
        String query = null;
        if (map.get("query") != null) {
            query = map.get("query").getConstraintValue();
        }

        // perform action
        // should perform the same actions as the macros above
        Object result = null;
        if (mode.equals("sqlquery")) {
            SqlQueryTask task = new SqlQueryTask(query, dbName);
            result = task.execute();
        } else if (mode.equals("hqlquery")) {
            HqlQueryTask task = new HqlQueryTask(query, dbName);
            result = task.execute();
        } else if (mode.equals("sqlstatement")) {
            SqlStatementTask task = new SqlStatementTask(query, dbName);
            result = task.execute();
        } else if (mode.equals("hqlstatement")) {
            HqlStatementTask task = new HqlStatementTask(query, dbName);
            result = task.execute();
        }

        // instead of placing a single value in an arraylist, just return the
        // single item
        ResponseMessageGeneric rval = new ResponseMessageGeneric(result);

        return rval;
    }

}
