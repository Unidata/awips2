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
package com.raytheon.uf.edex.services;

import static com.raytheon.uf.edex.services.textdbimpl.CommandExecutor.createErrorMessage;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.common.message.Message;
import com.raytheon.uf.edex.services.textdbimpl.CommandExecutor;
import com.raytheon.uf.edex.services.textdbsrv.ICommandExecutor;

/**
 * 
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 03, 2008       1538 jkorman     Initial implementation
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
public class TextDBSrv {

    private static Integer instanceId = 0;

    private Integer serviceInstanceId = null;

    // private boolean jmxModeOn = false;

    // private ObjectName serviceJmxId = null;

    // private boolean serviceRegistered = false;

    // Exposed properties

    // private String serviceName = null;

    private int messageCount = 0;

    private Log logger = LogFactory.getLog(getClass());

    private ICommandExecutor executor = null;

    public TextDBSrv() {
        super();
        synchronized (instanceId) {
            instanceId = instanceId + 1;
            serviceInstanceId = new Integer(instanceId);
        }
        executor = new CommandExecutor();
    }

    // /**
    // *
    // */
    // public String process(String text) throws EdexException {
    // String retMsg = "";
    // if (text != null) {
    //
    // try {
    // messageCount++;
    // String xmlMessage = null;
    // try {
    // Object m = unmarshalFromXml(text);
    //
    // Message sMessage = null;
    //
    // if (m instanceof Message) {
    //                        
    // sMessage = executeMessage((Message) m);
    //
    // if (sMessage != null) {
    // xmlMessage = marshalToXml(sMessage);
    // } else {
    // xmlMessage =
    // marshalToXml(createErrorMessage("ERROR:Null return from execute"));
    // }
    // } else {
    // String errMsg = "Message content was null";
    // if (m != null) {
    // errMsg = "ERROR:Incorrect message type "
    // + m.getClass().getName();
    // }
    // xmlMessage = marshalToXml(createErrorMessage(errMsg));
    // }
    // } catch (Exception e) {
    // logger.error("Error processing message", e);
    // // attempt to send an error message back to the client.
    // try {
    // xmlMessage =
    // marshalToXml(createErrorMessage("ERROR:Exception processing message"));
    // } catch (JAXBException e1) {
    // logger.error(e1);
    // }
    // }
    //
    // retMsg = xmlMessage;
    //
    // } catch (Exception e) {
    // logger.error("Error getting message payload", e);
    // }
    // }
    //
    // if (retMsg == null) {
    // retMsg = "An error occurred";
    // }
    //
    // return retMsg;
    // }

    public Message processMessage(Message message) {
        Message returnMessage = null;
        try {
            if (message != null) {
                messageCount++;
                returnMessage = executeMessage(message);

                if (returnMessage == null) {
                    returnMessage = createErrorMessage("ERROR:Null return from execute");
                }
            } else {
                String errMsg = "Message content was null";
                returnMessage = createErrorMessage(errMsg);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return returnMessage;
    }

    // /**
    // *
    // * @return
    // */
    // public boolean isJmxModeOn() {
    // return jmxModeOn;
    // }
    //
    // /**
    // *
    // * @param desiredMode
    // */
    // public void setJmxModeOn(boolean desiredJmxMode) {
    // jmxModeOn = desiredJmxMode;
    // // if (desiredJmxMode) {
    // // register(serviceName);
    // // }
    // }

    // /**
    // * Get the name of this service.
    // *
    // * @return The service name.
    // */
    // @Override
    // public String getServiceName() {
    // return serviceName;
    // }
    //
    // /**
    // * Set the name of this service.
    // *
    // * @param serviceName
    // * The service name.
    // */
    // public void setServiceName(String serviceName) {
    // this.serviceName = serviceName;
    // }

    // /**
    // * Clear the message count to zero.
    // */
    // @Override
    // public void clearMessageCount() {
    // messageCount = 0;
    // }
    //
    // /**
    // * Get a count of messages processed since startup or the last reset.
    // *
    // * @return Message count.
    // */
    // @Override
    // public int getMessageCount() {
    // return messageCount;
    // }

    /**
     * 
     * @param command
     *            A command to execute.
     */
    public Message executeMessage(Message command) {
        return executor.execute(command);
    }

    /**
     * Execute an arbitrary string command.
     * 
     * @param command
     *            A command to execute.
     */
    public void executeString(String command) {
        executeCommand(command);
    }

    /**
     * Execute an arbitrary string command. This method the the actual execution
     * agent.
     * 
     * @param command
     *            A command to execute.
     */
    private synchronized void executeCommand(String command) {
        if ("read".equals(command)) {
            logger.info("Processing command");
        }
    }

    // /**
    // * Register this service with the JMX management.
    // */
    // protected void register(String name) {
    // if (serviceRegistered || !isJmxModeOn()) {
    // return;
    // }
    //
    // String domain = rightShortenName(
    // this.getClass().getPackage().getName(), 2);
    //
    // // Get the MBean server for the platform
    // MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
    // try {
    // // register the "server" dummy class, if necessary
    // ObjectName dummyId = new ObjectName(domain + ":type=server");
    // if (!mbs.isRegistered(dummyId)) {
    // mbs.registerMBean(new ServerGroup(), dummyId);
    // }
    // // register this class as an MBean
    // serviceJmxId = new ObjectName(domain + ":type=server,name=" + name
    // + "." + serviceInstanceId);
    // StandardMBean smbean = new StandardMBean(this,
    // TextDBSrvInterface.class);
    // mbs.registerMBean(smbean, serviceJmxId);
    // serviceRegistered = true;
    // } catch (Exception e) {
    // logger.error("register(2) failed to register with JMX server", e);
    //
    // serviceRegistered = false;
    // jmxModeOn = false;
    // }
    // }
    //
    // /**
    // * Unregister this service from the JMX server. This should be called
    // prior
    // * to shutting down the service.
    // */
    // protected void unRegister(String name) {
    // if (!serviceRegistered || !isJmxModeOn()) {
    // return;
    // }
    // // Get the MBean server for the platform
    // MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
    // try {
    // if (mbs.isRegistered(serviceJmxId)) {
    // mbs.unregisterMBean(serviceJmxId);
    // }
    //
    // serviceRegistered = false;
    // logger.info("JMX Monitoring for " + serviceName + " stopped");
    // } catch (Exception e) {
    // logger.error("register(2) failed to register with JMX server", e);
    // serviceRegistered = false;
    // jmxModeOn = false;
    // }
    // }

}
