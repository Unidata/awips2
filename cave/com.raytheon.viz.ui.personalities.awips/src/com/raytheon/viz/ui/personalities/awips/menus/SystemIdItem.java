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
package com.raytheon.viz.ui.personalities.awips.menus;

import java.lang.management.ManagementFactory;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.ui.actions.CompoundContributionItem;

import sun.management.Agent;

/**
 * Command that displays the system id
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class SystemIdItem extends CompoundContributionItem {

    /**
     * Boolean to mark if we can use the file names as a connection checking
     * method for determining orphaned locks
     */
    private static boolean canCheckConnection = false;

    static {
        try {
            String val = System
                    .getProperty("com.sun.management.jmxremote.port");
            // If the port is set, then the Agent is already started and we
            // would shutdown if we couldn't connect
            if (val == null) {
                // Get starting jmx remote port, default to 20000
                boolean connectionFailed = true;
                int startPort = 20000;
                int i = 0;
                System.setProperty("com.sun.management.jmxremote.port", ""
                        + startPort);
                do {
                    // Stop at 50 since at that point, may be underlying issue
                    // (firewall) blocking it and less likely there are 50
                    // instances
                    try {
                        ++i;
                        Agent.startAgent();
                        // jmx manager successfully started
                        connectionFailed = false;
                        canCheckConnection = true;
                    } catch (Exception e) {
                        System.setProperty("com.sun.management.jmxremote.port",
                                "" + (startPort + i));
                        Agent.getManagementProperties()
                                .setProperty(
                                        "com.sun.management.jmxremote.port",
                                        System.getProperty("com.sun.management.jmxremote.port"));

                    }
                } while (connectionFailed && i < 50);
            } else {
                canCheckConnection = true;
            }
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    private static final String jmxPort = System
            .getProperty("com.sun.management.jmxremote.port");

    private static final String NAME = ManagementFactory.getRuntimeMXBean()
            .getName();

    private static final String HOST = NAME.split("[@]")[1];

    private static final String PID = NAME.split("[@]")[0];

    // name is in form processid@host, ID = host:port if we can check
    // connections or we will just use processid@host
    private static final String ID = HOST + ":"
            + (canCheckConnection ? jmxPort : PID);

    static {
        System.out.println(ID);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.ui.actions.CompoundContributionItem#getContributionItems()
     */
    @Override
    protected IContributionItem[] getContributionItems() {
        return new IContributionItem[] { new ActionContributionItem(new Action(
                HOST + ":" + PID) {
            @Override
            public boolean isEnabled() {
                return false;
            }
        }) };
    }

}
