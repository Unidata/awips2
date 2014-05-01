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
package com.raytheon.edex.common;

import com.raytheon.uf.common.geospatial.AbstractSpatialDbQuery;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Give Cave like GIS spatial queries to EDEX
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 08, 2009            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public class SpatialDbQuery extends AbstractSpatialDbQuery {

    /** The logger */
    public static final IUFStatusHandler handler = UFStatus
            .getHandler(SpatialDbQuery.class);

    /** Data Access Object DB conn */
    public CoreDao dao = null;

    /**
     * Make a DB request
     * 
     * @param sql
     * @return
     */
    public Object[] dbRequest(String sql, String dbname) {
        if (handler.isPriorityEnabled(Priority.DEBUG)) {
            handler.handle(Priority.DEBUG, "Edex SpatialQuery:  SQL to run: "
                    + sql);
        }

        Object[] results = null;
        try {
            if (dao == null) {
                try {
                    dao = new CoreDao(DaoConfig.forDatabase(dbname));
                } catch (Exception ed1) {
                    handler.handle(Priority.ERROR,
                            "Edex SpatialQuery: Core DAO access failed", ed1);
                }
            }
            results = dao.executeSQLQuery(sql);

        } catch (Exception ed2) {
            handler.handle(Priority.DEBUG,
                    "Edex SpatialQuery: SQL Query Failed to process. SQL="
                            + sql, ed2);
        }
        return results;
    }

}
