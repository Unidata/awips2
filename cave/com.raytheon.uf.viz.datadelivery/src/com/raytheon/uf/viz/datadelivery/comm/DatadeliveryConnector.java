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

package com.raytheon.uf.viz.datadelivery.comm;

import org.eclipse.core.runtime.IProgressMonitor;

import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.comm.MessageMarshaller;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;

/**
 * 
 * Provides connectivity to the Datadelivery REG/REP
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------  ----------  ----------- --------------------------
 *    02/14/12                    dhladky    Initial Creation.
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1
 */
public class DatadeliveryConnector {

    private static final String ENDPOINT_NAME = "/datadelivery";

    /** Preference Constant: HTTP Connection Method */
    public final static String HTTP_METHOD = "http";

    /** instance */
    private static DatadeliveryConnector instance;

    /**
     * Singleton instance
     * 
     * @return
     * @throws VizException
     */
    public static DatadeliveryConnector getInstance() throws VizException {
        if (instance == null) {
            instance = new DatadeliveryConnector();
        }
        return instance;
    }

    /**
     * Private constructor
     * 
     * @throws VizException
     */
    private DatadeliveryConnector() throws VizException {

    }

    /**
     * Send a script to the server to be processed
     * 
     * @param script
     *            the edex script
     * @param monitor
     *            the progress monitor
     * @param timeout
     *            the timeout for edex in milliseconds
     * @return a service response
     * @throws VizException
     */
    public Object[] connect(String script, IProgressMonitor monitor, int timeout)
            throws VizException {
        Message message = connectHTTP(script, monitor, timeout);
        return MessageMarshaller.localizeResponse(message);
    }

    /**
     * Send a catalog script to the server to be processed
     * 
     * @param script
     *            the edex script
     * @param monitor
     *            the progress monitor
     * @param timeout
     *            the timeout for edex in milliseconds
     * @return a service response
     * @throws VizException
     */
    public Message connectMessage(String script, IProgressMonitor monitor,
            int timeout) throws VizException {
        Message message = connectHTTP(script, monitor, timeout);
        return message;
    }

    /**
     * Send a script via synchronous http
     * 
     * @param script
     *            the script to send
     * @param monitor
     *            the progress monitor
     * @param timeOut
     *            the HTTP timeout in milliseconds
     * @return a service response
     * @throws VizException
     * @throws VizCommunicationException
     */
    private Message connectHTTP(String query, final IProgressMonitor monitor,
            int timeOut) throws VizException, VizCommunicationException {
        // long t0 = System.currentTimeMillis();
        String httpAddress = VizApp.getHttpServer();
        if (monitor != null) {
            monitor.worked(1);
            monitor.subTask("Sending request/Receiving Response...");
        }

        Message result = null;
        try {
            result = MessageMarshaller.decodeServiceMessage(
                    monitor,
                    HttpClient.getInstance().postBinary(
                            httpAddress + ENDPOINT_NAME, query.getBytes()), "");

        } catch (Exception e) {
            throw new VizCommunicationException(e);
        }

        if (monitor != null) {
            monitor.worked(4);
        }
        if (result == null) {
            throw new VizServerSideException("Got null response from Reg/Rep");
        }

        if (monitor != null) {
            monitor.worked(1);
        }

        return result;
    }
}
