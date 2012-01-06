package com.raytheon.uf.viz.monitor;

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

import java.util.Collection;

import com.raytheon.uf.viz.core.alerts.AlertMessage;
import com.raytheon.uf.viz.core.notification.NotificationMessage;
import com.raytheon.uf.viz.monitor.events.IMonitorConfigurationEvent;
import com.raytheon.uf.viz.monitor.events.IMonitorThresholdEvent;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 9, 2010            dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

public abstract class ResourceMonitor extends Monitor {

    @Override
    protected void processNotifyMessage(NotificationMessage filtered) {
        // Not used by these classes
    }

    @Override
    protected void processProductMessage(AlertMessage filtered) {
        // Not used by these classes
    }

    @Override
    protected boolean filterNotifyMessage(NotificationMessage alertMessage) {
        // Not used by these classes
        return false;
    }

    @Override
    protected boolean filterProductMessage(AlertMessage alertMessage) {
        // Not used by these classes
        return false;
    }

    @Override
    public void alertArrived(Collection<AlertMessage> alertMessages) {
        // Not used by these classes
    }

    @Override
    public void initObserver(String pluginName, Monitor monitor) {
        // Not used by these classes
    }

    @Override
    protected abstract void nullifyMonitor();

    // used by multi site scan
    protected abstract void nullifyMonitor(String icao);

    @Override
    public abstract void thresholdUpdate(IMonitorThresholdEvent me);

    @Override
    public abstract void configUpdate(IMonitorConfigurationEvent me);

}
