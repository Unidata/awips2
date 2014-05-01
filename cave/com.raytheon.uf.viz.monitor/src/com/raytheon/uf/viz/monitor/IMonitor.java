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
package com.raytheon.uf.viz.monitor;

import java.util.ArrayList;
import java.util.Date;

import com.raytheon.viz.alerts.IAlertObserver;

public interface IMonitor extends IAlertObserver {
    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface.
     */
    public void fireMonitorEvent();

    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface. This method takes the name of the
     * listener class as an argument that can be used for targeted
     * notifications.
     */
    public void fireMonitorEvent(String type);

    /**
     * Fires events to listeners of the IMonitorListener interface generally
     * used to notify updates of data to display elements from classes that
     * implement the IMonitor interface.
     */
    public void fireMonitorEvent(Monitor monitor);

    /**
     * Adds this monitor as a listener to the ProductAlerts
     * 
     * @param pluginName
     */
    public void initObserver(String pluginName, Monitor monitor);

    /**
     * Order the dates
     * 
     * @param type
     * @return
     */
    public ArrayList<Date> getTimeOrderedKeys(IMonitor monitor, String type);
}
