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
package com.raytheon.uf.edex.ingest.notification.router;

import java.util.LinkedList;
import java.util.List;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.edex.core.EDEXUtil;
import com.raytheon.uf.edex.core.EdexException;
import com.raytheon.uf.edex.ingest.notification.PluginNotifierConfig;

/**
 * Routes pdos to a destination. Should only be used for destination inside the
 * jvm.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 19, 2013 2170       rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
public class PdoRouter implements INotificationRouter {

    /**
     * Destination URI for this router.
     */
    private final String route;

    /**
     * PDO's in use by current thread. Since all PDO data is sent immediately
     * handles concurrency via thread local instance.
     */
    private final ThreadLocal<List<PluginDataObject>> myPdos = new ThreadLocal<List<PluginDataObject>>() {

        @Override
        protected List<PluginDataObject> initialValue() {
            return new LinkedList<PluginDataObject>();
        }

    };

    public PdoRouter(PluginNotifierConfig config) {
        this.route = config.getEndpointUri();
    }

    @Override
    public String getRoute() {
        return route;
    }

    @Override
    public void process(PluginDataObject pdo) {
        myPdos.get().add(pdo);
    }

    @Override
    public void sendImmediateData() throws EdexException {
        List<PluginDataObject> pdos = myPdos.get();
        myPdos.remove();
        if (pdos.size() > 0) {
            EDEXUtil.getMessageProducer().sendAsyncUri(route, pdos);
        }
    }

    @Override
    public void sendQueuedData() throws EdexException {
        // NOOP all data sent immediately
    }
}
