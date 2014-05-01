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
package com.raytheon.uf.edex.site;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import com.raytheon.uf.common.site.notify.ClusterActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification;
import com.raytheon.uf.common.site.notify.SiteActivationNotification.ACTIVATIONSTATUS;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 9, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class SiteActivationMonitor {

    private Map<String, Map<String, Set<String>>> activationMap = new HashMap<String, Map<String, Set<String>>>();

    private Map<String, Map<String, Set<String>>> deactivationMap = new HashMap<String, Map<String, Set<String>>>();

    private static SiteActivationMonitor instance;

    private ACTIVATIONSTATUS status = ACTIVATIONSTATUS.SUCCESS;

    private SiteActivationMonitor() {
    }

    public static SiteActivationMonitor getInstance() {
        if (instance == null) {
            instance = new SiteActivationMonitor();
        }
        return instance;
    }

    public Set<String> getPendingActivations(String plugin, String site) {
        if (activationMap.containsKey(plugin)) {
            if (activationMap.get(plugin).containsKey(site)) {
                return activationMap.get(plugin).get(site);
            } else {
                return Collections.emptySet();
            }
        } else {
            return Collections.emptySet();
        }
    }

    public Set<String> getPendingDeactivations(String plugin, String site) {
        if (deactivationMap.containsKey(plugin)) {
            if (deactivationMap.get(plugin).containsKey(site)) {
                return deactivationMap.get(plugin).get(site);
            } else {
                return Collections.emptySet();
            }
        } else {
            return Collections.emptySet();
        }
    }

    public boolean isFailure() {
        return status.equals(ACTIVATIONSTATUS.FAILURE);
    }

    public void resetFailure() {
        status = ACTIVATIONSTATUS.SUCCESS;
    }
    
    public ACTIVATIONSTATUS getStatus(){
        return status;
    }

    public SiteActivationNotification handleNotification(
            SiteActivationNotification notification) {
        if (notification instanceof ClusterActivationNotification) {
            return notification;
        }

        String plugin = notification.getPluginName();
        String modifiedSite = notification.getModifiedSite();
        String serverAndMode = notification.getServerAndRunMode();
        if (!activationMap.containsKey(plugin)) {
            activationMap.put(plugin, new HashMap<String, Set<String>>());
        }

        if (!activationMap.get(plugin).containsKey(modifiedSite)) {
            activationMap.get(plugin).put(modifiedSite, new HashSet<String>());
        }

        if (!deactivationMap.containsKey(plugin)) {
            deactivationMap.put(plugin, new HashMap<String, Set<String>>());
        }

        if (!deactivationMap.get(plugin).containsKey(modifiedSite)) {
            deactivationMap.get(plugin)
                    .put(modifiedSite, new HashSet<String>());
        }

        if (notification.isBegin()) {
            if (notification.isActivation()) {
                activationMap.get(plugin).get(modifiedSite).add(serverAndMode);
            } else if (notification.isDeactivation()) {
                deactivationMap.get(plugin).get(modifiedSite)
                        .add(serverAndMode);
            }
        } else if (notification.isSuccess()) {
            if (notification.isActivation()) {
                activationMap.get(plugin).get(modifiedSite)
                        .remove(serverAndMode);
            } else if (notification.isDeactivation()) {
                deactivationMap.get(plugin).get(modifiedSite)
                        .remove(serverAndMode);
            }
        } else if (notification.isFailure()) {
            status = ACTIVATIONSTATUS.FAILURE;
        }
        return notification;
    }
}
