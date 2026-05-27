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
package com.raytheon.uf.edex.registry.status;

import java.util.List;

import com.raytheon.uf.common.registry.RegistryHandler;
import com.raytheon.uf.common.registry.RegistryQueryResponse;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.edex.core.EDEXUtil;

/**
 *
 * This class runs a cron job to check the the registry status and sends
 * notification if the registry is down
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2021  8267      aelgorashi   Initial creation
 * Oct 12, 2022  8949      thuggins     Add ability to disable Registry Status check for sites
 *
 * </pre>
 *
 * @author aelgorashi
 */
public class RegistryStatusHealthCheck {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(RegistryStatusHealthCheck.class);

    private static final String PLUGIN = "com.raytheon.uf.edex.registry.status";

    private static final String SOURCE = "EDEX";

    private static final String CATEGORY = "DEFAULT";

    private static final String warningMessage = "Warning: Registry is down";

    private final long alertVizIntervalMillis = Long
            .parseLong(System.getProperty("alertVizNotificationInterval"))
            * TimeUtil.MILLIS_PER_MINUTE;

    // registry health check enable will default to true if not set
    private final boolean enabled = Boolean.parseBoolean(
            System.getProperty("registry.healthCheck.enabled", "true"));

    private long lastAlertVizTime = 0;

    private final RegistryHandler registryHandler;

    public RegistryStatusHealthCheck(RegistryHandler registryHandler) {
        this.registryHandler = registryHandler;
    }

    public void checkRegistryStatus() {
        if (!enabled) {
            return;
        }
        statusHandler.info("--> Start Checking Registry Status");
        long currTime = SimulatedTime.getSystemTime().getMillis();

        Throwable error = null;
        try {
            RegistryQueryResponse<?> response = registryHandler
                    .getObjects(new DummyRegistryQuery());
            List<Throwable> errors = response.getErrors();
            if (!errors.isEmpty()) {
                // Just pull first error
                error = errors.get(0);
            }
        } catch (Throwable t) {
            error = t;
        }

        if (error != null) {
            long diff = currTime - lastAlertVizTime;
            if (diff >= alertVizIntervalMillis) {
                EDEXUtil.sendMessageAlertViz(Priority.ERROR, PLUGIN, SOURCE,
                        CATEGORY, warningMessage, warningMessage, null);
                lastAlertVizTime = currTime;
                statusHandler.error(warningMessage, error);
            } else {
                // Just log error without stack trace to avoid spamming
                statusHandler.error(warningMessage);
            }
        }
    }
}
