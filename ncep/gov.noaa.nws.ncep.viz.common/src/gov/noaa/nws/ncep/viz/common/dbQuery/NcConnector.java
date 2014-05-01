/*****************************************************************************************
 * COPYRIGHT (c), 2006-2008, RAYTHEON COMPANY
 * ALL RIGHTS RESERVED, An Unpublished Work 
 *
 * RAYTHEON PROPRIETARY
 * If the end user is not the U.S. Government or any agency thereof, use
 * or disclosure of data contained in this source code file is subject to
 * the proprietary restrictions set forth in the Master Rights File.
 *
 * U.S. GOVERNMENT PURPOSE RIGHTS NOTICE
 * If the end user is the U.S. Government or any agency thereof, this source
 * code is provided to the U.S. Government with Government Purpose Rights.
 * Use or disclosure of data contained in this source code file is subject to
 * the "Government Purpose Rights" restriction in the Master Rights File.
 *
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * Use or disclosure of data contained in this source code file is subject to
 * the export restrictions set forth in the Master Rights File.
 ******************************************************************************************/

package gov.noaa.nws.ncep.viz.common.dbQuery;

import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import org.eclipse.core.runtime.IProgressMonitor;

import com.raytheon.uf.common.comm.CommunicationException;
import com.raytheon.uf.common.comm.HttpClient;
import com.raytheon.uf.common.dataquery.db.QueryResult;
import com.raytheon.uf.common.dataquery.db.QueryResultRow;
import com.raytheon.uf.common.message.Body;
import com.raytheon.uf.common.message.Header;
import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.common.message.Property;
import com.raytheon.uf.common.message.response.ResponseMessageError;
import com.raytheon.uf.common.message.response.ResponseMessageGeneric;
import com.raytheon.uf.common.serialization.DynamicSerializationManager;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.comm.MessageMarshaller;
import com.raytheon.uf.viz.core.exception.VizCommunicationException;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.exception.VizServerSideException;

/**
 * 
 * Provides connectivity to the ESB
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 *    Date          Ticket#     Engineer    Description
 *    ------------	----------	-----------	--------------------------
 *    7/1/06                    chammack    Initial Creation.
 *    4/17/08       1088        chammack    Refactored to use apache httpclient
 *    02/08/10                  mlaryukhin  Adapted com.raytheon.uf.viz.core.comm.Connector 
 *                                          for the use without calling VizApp (which needs an Eclipse plug in)
 * </pre>
 * 
 * @author chammack, mlaryukhin
 * @version 1
 */
public class NcConnector {

    private static final IUFStatusHandler statusHandler = UFStatus.getHandler(
            NcConnector.class, "DEFAULT");

    private static final String ENDPOINT_NAME = "/pyproductthrift";

    /** Preference Constant: HTTP Connection Method */
    public final static String HTTP_METHOD = "http";

    /** Preference Constant: JMS Connection Method */
    public final static String JMS_METHOD = "jms";

    /** instance */
    private static NcConnector instance;

    static {
        // these classes should be loaded when running from command line
        // they are loaded if we run eclipse plug in
        Class<?>[] toLoad = { Message.class, Header.class, Property.class,
                Body.class, ResponseMessageGeneric.class, QueryResult.class,
                QueryResultRow.class, ResponseMessageError.class };
        for (Class<?> c : toLoad) {
            DynamicSerializationManager.inspect(c);
        }
    }

    /**
     * Singleton instance
     * 
     * @return
     * @throws VizException
     */
    public static NcConnector getInstance() throws VizException {
        if (instance == null) {
            instance = new NcConnector();
        }
        return instance;
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
     * @param httpAddress
     * @return a service response
     * @throws VizException
     */
    public Object[] connect(String script, IProgressMonitor monitor,
            int timeout, String httpAddress) throws VizException {
        Message message = connectHTTP(script, monitor, timeout, httpAddress);
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
     * @param httpAddress
     * @return a service response
     * @throws VizException
     */
    public Message connectMessage(String script, IProgressMonitor monitor,
            int timeout, String httpAddress) throws VizException {
        Message message = connectHTTP(script, monitor, timeout, httpAddress);
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
     * @param httpAddress
     * @return a service response
     * @throws VizException
     * @throws VizCommunicationException
     */
    private Message connectHTTP(String script, final IProgressMonitor monitor,
            int timeOut, String httpAddress) throws VizException,
            VizCommunicationException {

        if (monitor != null) {
            monitor.worked(1);
            monitor.subTask("Sending request/Receiving Response...");
        }

        final Message[] result = new Message[1];
        try {
            HttpClient.getInstance().postStreamingString(
                    httpAddress + ENDPOINT_NAME, script,
                    new HttpClient.IStreamHandler() {

                        @Override
                        public void handleStream(InputStream is)
                                throws CommunicationException {
                            try {
                                result[0] = MessageMarshaller
                                        .decodeServiceMessageStreaming(monitor,
                                                is, "");

                            } catch (SerializationException e) {
                                throw new CommunicationException(
                                        "Error deserializing", e);
                            } catch (VizException e) {
                                throw new CommunicationException(e);
                            }
                        }

                    });
        } catch (CommunicationException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (UnsupportedEncodingException e) {
            // TODO Auto-generated catch block. Please revise as appropriate.
            statusHandler.handle(Priority.CRITICAL, e.getLocalizedMessage(), e);
        }

        if (monitor != null) {
            monitor.worked(4);
        }
        if (result[0] == null) {
            throw new VizServerSideException("Got null response from server");
        }

        if (monitor != null) {
            monitor.worked(1);
        }

        return result[0];
    }
}
