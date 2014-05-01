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

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.eventbus.AllowConcurrentEvents;
import com.google.common.eventbus.Subscribe;
import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatus;
import com.raytheon.uf.common.datadelivery.event.status.DataDeliverySystemStatusId;
import com.raytheon.uf.common.datadelivery.event.status.SystemStatusEvent;
import com.raytheon.uf.edex.datadelivery.event.dao.DataDeliverySystemStatusDao;

/**
 * Event bus message handler class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2013   1655     mpduff      Initial creation.
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */
@Service
@Transactional
public class SystemStatusEventBusHandler {
    /** The Data Access Object */
    private DataDeliverySystemStatusDao statusDao;

    /**
     * Gets called when Event Bus publishes an event
     * 
     * @param event
     *            EventBus's
     */
    @Subscribe
    @AllowConcurrentEvents
    public void eventListener(SystemStatusEvent event) {
        DataDeliverySystemStatus record = new DataDeliverySystemStatus();
        DataDeliverySystemStatusId id = new DataDeliverySystemStatusId();
        id.setName(event.getName());
        record.setStatus(event.getStatus().getStatus());
        id.setSystemType(event.getSystemType());
        record.setKey(id);

        statusDao.createOrUpdate(record);
    }

    /**
     * @return the statusDao
     */
    public DataDeliverySystemStatusDao getStatusDao() {
        return statusDao;
    }

    /**
     * @param statusDao
     *            the statusDao to set
     */
    public void setStatusDao(DataDeliverySystemStatusDao statusDao) {
        this.statusDao = statusDao;
    }
}
