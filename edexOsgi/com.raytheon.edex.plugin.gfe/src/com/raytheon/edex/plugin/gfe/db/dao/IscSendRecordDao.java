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
package com.raytheon.edex.plugin.gfe.db.dao;

import java.util.Date;

import com.raytheon.edex.plugin.gfe.isc.IscSendRecord;
import com.raytheon.edex.plugin.gfe.isc.IscSendRecord.IscSendState;
import com.raytheon.uf.common.dataquery.db.QueryParam.QueryOperand;
import com.raytheon.uf.edex.database.DataAccessLayerException;
import com.raytheon.uf.edex.database.dao.CoreDao;
import com.raytheon.uf.edex.database.dao.DaoConfig;
import com.raytheon.uf.edex.database.query.DatabaseQuery;

/**
 * Data access object for IscSendRecord objects
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2011            bphillip     Initial creation
 * Apr 06, 2012  #457      dgilling     Added deleteForSite().
 * May 08, 2012  #600      dgilling     Refactor to match IscSendRecord
 *                                      changes.
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class IscSendRecordDao extends CoreDao {

    public IscSendRecordDao() {
        super(DaoConfig.forClass(IscSendRecord.class));
    }

    public int purgeExpiredPending() throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("timeRange.end", new Date(),
                QueryOperand.LESSTHAN);
        deleteStmt.addQueryParam("state", IscSendState.PENDING);
        return this.deleteByCriteria(deleteStmt);
    }

    public int deleteForSite(String siteId) throws DataAccessLayerException {
        DatabaseQuery deleteStmt = new DatabaseQuery(this.daoClass);
        deleteStmt.addQueryParam("state", IscSendState.RUNNING,
                QueryOperand.NOTEQUALS);
        deleteStmt.addQueryParam("parmID", "%:" + siteId + "_%",
                QueryOperand.LIKE);
        return deleteByCriteria(deleteStmt);
    }
}