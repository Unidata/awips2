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
package com.raytheon.uf.edex.log;

import com.raytheon.uf.common.alertmonitor.AbstractMonitorHandler;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * UFStatus Handler for EDEX Monitor events.
 * 
 * <code>
 * IUFStatusHandler statusHandler = UFStatus.getMonitorHandler(YourClass.class);
 * statusHandler.handle(UFStatus.Priority.CRITICAL, 
 *     "TEST MONITOR ALERT:\n"
 *     + "If there was an issue,\n"
 *     + "this would be a monitor message.")
 * </code>
 */
public class EdexMonitorHandler extends AbstractMonitorHandler {

    public EdexMonitorHandler() {
        setMonitorEndpoint(EDEXUtil.getAlertendpoint());
    }

    public EdexMonitorHandler(String pluginId, String source) {
        super(pluginId, source);
        setMonitorEndpoint(EDEXUtil.getAlertendpoint());
    }

    /**
     * 
     * @param sm
     * @throws Exception
     */
    protected void sendMonitorMessage(StatusMessage sm) throws Exception {
        EDEXUtil.getMessageProducer().sendAsync(getMonitorEndpoint(), sm);
    }

}
