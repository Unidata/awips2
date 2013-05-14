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
package com.raytheon.uf.edex.esb.camel.context;

import java.util.ArrayList;
import java.util.List;

import org.apache.camel.CamelContext;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Dynamically starts/stops a context and its associated routes so that only one
 * context in the cluster is running. This should mainly be used for reading
 * from topics so that only box is processing the topic data in the cluster for
 * singleton type events.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 10, 2010 5050       rjpeter     Initial creation
 * May 13, 2013 1989       njensen     Camel 2.11 compatibility
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class ContextManager {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ContextManager.class);

    private List<CamelContext> contextList = new ArrayList<CamelContext>();

    private static ContextManager instance = new ContextManager();

    public static ContextManager getInstance() {
        return instance;
    }

    private ContextManager() {
    }

    public void startContexts() {
        statusHandler.info("Context Manager starting routes");
        for (CamelContext camelContext : contextList) {
            try {
                /*
                 * In camel 2.11, all contexts are "started" automatically but
                 * the isAutoStartup() flag determines if the routes are
                 * automatically started. The code in DefaultCamelContext is
                 * safe to call start() on a second time to get the routes
                 * started.
                 * 
                 * For more information, see:
                 * http://camel.465427.n5.nabble.com/Camel
                 * -context-autostartup-td5721638.html
                 * 
                 * https://issues.apache.org/jira/browse/CAMEL-5759
                 */
                if (!camelContext.isAutoStartup()) {
                    camelContext.start();
                }
            } catch (Exception e) {
                statusHandler.handle(Priority.ERROR,
                        "Failed to start routes for " + camelContext.getName(),
                        e);
            }
        }
    }

    public ContextManager register(CamelContext context) {
        contextList.add(context);
        return this;
    }
}
