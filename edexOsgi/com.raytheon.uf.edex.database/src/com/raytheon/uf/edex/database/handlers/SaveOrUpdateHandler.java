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
package com.raytheon.uf.edex.database.handlers;

import java.util.List;

import com.raytheon.uf.common.dataquery.requests.SaveOrUpdateRequest;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;

/**
 * Handler for a SaveOrUpdateRequest that saves the objects to the specified
 * database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 8, 2013  2361       njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class SaveOrUpdateHandler implements
        IRequestHandler<SaveOrUpdateRequest> {

    @Override
    public Object handleRequest(SaveOrUpdateRequest request) throws Exception {
        String dbName = request.getDbName();
        if (dbName == null) {
            throw new IllegalArgumentException("Database name cannot be null");
        }

        CoreDao dao = new CoreDao(DaoConfig.forDatabase(dbName));
        List<Object> objs = request.getObjectsToUpdate();

        // This was originally written to replace the saveOrUpdate part of
        // QlServerRequestHandler. To match that behavior, we're not putting
        // try/catch around each call to saveOrUpdate, and we're only returning
        // a count of the objects saved/updated.
        // TODO contemplate better behavior
        int count = 0;
        if (objs != null) {
            for (Object obj : objs) {
                dao.saveOrUpdate(obj);
                count++;
            }
        }

        return count;
    }

}
