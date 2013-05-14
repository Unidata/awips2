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
package com.raytheon.uf.edex.esb.camel.directvm;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import org.apache.camel.Consumer;
import org.apache.camel.Producer;
import org.apache.camel.component.direct.DirectEndpoint;
import org.apache.camel.impl.DefaultConsumer;

/**
 * Provides a cross-context synchronous component
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 18, 2008            chammack     Initial creation
 * Jul 16, 2012 DR 15073   D. Friedman  Override correct methods
 * May 09, 2013 1989       njensen      Camel 2.11 compatibility
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class DirectVMEndpoint extends DirectEndpoint {
    private boolean allowMultipleConsumers = true;

    private CopyOnWriteArrayList<DefaultConsumer> consumers = new CopyOnWriteArrayList<DefaultConsumer>();

    public DirectVMEndpoint(String uri, DirectVMComponent component) {
        super(uri, component);
    }

    public DirectVMEndpoint(String uri, DirectVMComponent component,
            CopyOnWriteArrayList<DefaultConsumer> consumers) {
        super(uri, component);
        this.consumers = consumers;
    }

    @Override
    public Producer createProducer() throws Exception {
        return new DirectVMProducer(this);
    }

    @Override
    public Consumer createConsumer(org.apache.camel.Processor processor)
            throws Exception {
        return new DefaultConsumer(this, processor) {
            @Override
            protected void doStart() throws Exception {
                if (!allowMultipleConsumers && !consumers.isEmpty()) {
                    throw new IllegalStateException(
                            "Endpoint "
                                    + getEndpointUri()
                                    + " only allows 1 active consumer but you attempted to start a 2nd consumer.");
                }

                consumers.add(this);
                super.doStart();
            }

            @Override
            protected void doStop() throws Exception {
                super.doStop();
                consumers.remove(this);
            }
        };
    }

    public List<DefaultConsumer> getConsumers() {
        return consumers;
    }

}
