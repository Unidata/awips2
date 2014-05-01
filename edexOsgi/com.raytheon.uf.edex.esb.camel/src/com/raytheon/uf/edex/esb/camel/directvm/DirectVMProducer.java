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

import org.apache.camel.AsyncCallback;
import org.apache.camel.AsyncProcessor;
import org.apache.camel.CamelExchangeException;
import org.apache.camel.Exchange;
import org.apache.camel.component.direct.DirectProducer;
import org.apache.camel.impl.DefaultConsumer;
import org.apache.camel.impl.converter.AsyncProcessorTypeConverter;
import org.apache.camel.util.AsyncProcessorHelper;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Direct VM Producer used with DirectVMConsumer.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 27, 2010            mschenke     Initial creation
 * May 23, 2013 1989       njensen      Deprecated
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 * @deprecated Use camel's built-in direct-vm component instead. This component
 *             can be deleted after that has been tested thoroughly.
 */

@Deprecated
public class DirectVMProducer extends DirectProducer {
    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(DirectVMProducer.class);

    private DirectVMEndpoint endpoint;

    /**
     * @param endpoint
     */
    public DirectVMProducer(DirectVMEndpoint endpoint) {
        super(endpoint);
        this.endpoint = endpoint;
    }

    public void process(Exchange exchange) throws Exception {
        List<DefaultConsumer> consumers = endpoint.getConsumers();
        if (consumers == null || consumers.size() == 0) {
            statusHandler.handle(Priority.PROBLEM,
                    "No consumers available on endpoint: " + endpoint
                            + " to process: " + exchange);
            throw new CamelExchangeException(
                    "No consumers available on endpoint: " + endpoint, exchange);
        } else {
            for (DefaultConsumer consumer : consumers) {
                consumer.getProcessor().process(exchange);
            }
        }
    }

    public boolean process(Exchange exchange, AsyncCallback callback) {
        List<DefaultConsumer> consumers = endpoint.getConsumers();
        if (consumers == null || consumers.size() == 0) {
            statusHandler.handle(Priority.PROBLEM,
                    "No consumers available on endpoint: " + endpoint
                            + " to process: " + exchange);
            // indicate its done synchronously
            exchange.setException(new CamelExchangeException(
                    "No consumers available on endpoint: " + endpoint, exchange));
            callback.done(true);
            return true;
        } else {
            boolean sync = true;
            for (DefaultConsumer consumer : consumers) {
                AsyncProcessor processor = AsyncProcessorTypeConverter
                        .convert(consumer.getProcessor());
                sync &= AsyncProcessorHelper.process(processor, exchange,
                        callback);
            }

            return sync;
        }
    }

}
