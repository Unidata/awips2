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

package com.raytheon.edex.plugin.gfe.util;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.gfe.server.message.ServerResponse;
import com.raytheon.uf.common.dataplugin.gfe.server.notify.GfeNotification;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 * Sends GFE notifications to the GFE notify JMS topic.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/30/08     #875       bphillip    Initial Creation
 * 09/22/09     3058       rjpeter     changed to utility.
 * 06/12/13     2099       dgilling    Remove error when passed empty list of
 *                                     notifications.
 * 10/08/14     #3684      randerso    Changed to send directly to JMS topic
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */
public class SendNotifications {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(SendNotifications.class);

    public static ServerResponse<?> send(GfeNotification notification) {
        List<GfeNotification> notifications = new ArrayList<GfeNotification>();
        notifications.add(notification);
        return send(notifications);
    }

    public static ServerResponse<?> send(
            List<? extends GfeNotification> notifications) {
        ServerResponse<?> sr = new ServerResponse<String>();
        if (notifications.isEmpty()) {
            return sr;
        }

        try {
            EDEXUtil.getMessageProducer().sendAsyncThriftUri(
                    "jms-generic:topic:edex.alerts.gfe?timeToLive=60000",
                    notifications);
        } catch (Exception e) {
            statusHandler.error("Error sending gfe notification", e);
            sr.addMessage("Error sending gfe notification");
        }
        return sr;
    }
}
