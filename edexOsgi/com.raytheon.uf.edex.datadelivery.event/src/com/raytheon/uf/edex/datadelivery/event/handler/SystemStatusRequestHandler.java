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
package com.raytheon.uf.edex.datadelivery.event.handler;

import java.util.List;

import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatus;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusRequest;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusResponse;
import com.raytheon.uf.common.serialization.comm.IRequestHandler;
import com.raytheon.uf.edex.datadelivery.event.dao.DataDeliverySystemStatusDao;

/**
 * Data Delivery System Status Request Handler
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 18, 2013   1655     mpduff     Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class SystemStatusRequestHandler implements
        IRequestHandler<SystemStatusRequest> {
    /** System status DAO */
    private DataDeliverySystemStatusDao statusDao;

    /**
     * {@inheritDoc}
     */
    @Override
    public Object handleRequest(SystemStatusRequest request) throws Exception {
        List<DataDeliverySystemStatus> dataRecords = statusDao.getAll();

        return new SystemStatusResponse(dataRecords);
    }

    /**
     * @param statusDao
     *            the statusDao to set
     */
    public void setStatusDao(DataDeliverySystemStatusDao statusDao) {
        this.statusDao = statusDao;
    }
}
