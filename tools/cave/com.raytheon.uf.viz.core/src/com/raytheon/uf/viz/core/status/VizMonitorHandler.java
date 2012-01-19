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
package com.raytheon.uf.viz.core.status;

import com.raytheon.uf.common.alertmonitor.AbstractMonitorHandler;
import com.raytheon.uf.common.message.StatusMessage;

/**
 * UFStatus Handler for Viz Monitor events.
 * 
 * <code>
 * IUFStatusHandler statusHandler = UFStatus.getMonitorHandler(YourClass.class);
 * statusHandler.handle(UFStatus.Priority.CRITICAL, 
 *     "TEST MONITOR ALERT:\n"
 *     + "If there was an issue,\n"
 *     + "this would be a monitor message.")
 * </code>
 */
public class VizMonitorHandler extends AbstractMonitorHandler {

    public VizMonitorHandler() {
    }

    /**
     * Initialization constructor.
     * 
     * @param pluginId
     * @param category
     * @param source
     */
    public VizMonitorHandler(String pluginId, String source) {
        super(pluginId, source);
    }

    @Override
    protected void sendMonitorMessage(StatusMessage sm) throws Exception {
        MessageSender.sendToTopic(getMonitorEndpoint(), sm);
    }

}
