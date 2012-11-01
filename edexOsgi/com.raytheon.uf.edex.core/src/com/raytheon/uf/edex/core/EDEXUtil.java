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

package com.raytheon.uf.edex.core;

import java.io.File;
import java.io.IOException;
import java.net.InetAddress;
import java.net.URL;
import java.net.UnknownHostException;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.PluginException;
import com.raytheon.uf.common.dataplugin.persist.IPersistable;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.common.util.FileUtil;
import com.raytheon.uf.edex.core.props.EnvProperties;
import com.raytheon.uf.edex.core.props.PropertiesFactory;

/**
 * Contains utility methods for use by EDEX.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 04/23/08     1088        chammack    Split from Util
 * 11/22/2010   2235        cjeanbap    Added audio file to StatusMessage.
 * 02/02/2011   6500        cjeanbap    Added paramter to method signature and
 *                                      properly assign source value.
 * 06/12/2012   0609        djohnson    Use EDEXUtil for EDEX_HOME.
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class EDEXUtil implements ApplicationContextAware {

    public static final String EDEX_HOME = System.getProperty("edex.home");

    public static final String EDEX_BIN = EDEX_HOME + File.separatorChar
            + "bin";

    static Log logger = LogFactory.getLog(EDEXUtil.class);

    private static ApplicationContext CONTEXT;

    private static IMessageProducer MESSAGE_PRODUCER;

    private static IContextAdmin CONTEXT_ADMIN;

    // TODO
    private static final String alertEndpoint = "alertVizNotify";

    private static int serverId;
    static {
        try {
            String hostname = InetAddress.getLocalHost().getCanonicalHostName();
            serverId = hostname.hashCode();
        } catch (UnknownHostException e) {
            serverId = 0;
            // assume localhost, no network connection
        }
    }

    public static int getServerId() {
        return serverId;
    }

    @Override
    public void setApplicationContext(ApplicationContext context)
            throws BeansException {
        CONTEXT = context;
    }

    public static ApplicationContext getSpringContext() {
        return CONTEXT;
    }

    /**
     * Retrieve an object from the ESB context This object could be a Spring
     * Bean, a context or a property container
     * 
     * @param The
     *            name of the object
     * @return The instance
     */
    public static Object getESBComponent(String name) {
        Object result = null;

        try {
            result = CONTEXT.getBean(name);
        } catch (Exception e) {
            logger.error("Unable to retrieve component: " + name
                    + " from ESB. " + e);
        }

        return result;

    }

    public static boolean isRunning() {
        return "Operational".equals(System.getProperty("System.status"));
    }

    public static boolean containsESBComponent(String name) {
        return CONTEXT.containsBean(name);
    }

    /**
     * Retrieve a URL to a new file with a UUID name
     * 
     * @return A URL to the new file
     * @throws IOException
     * @throws PluginException
     */
    public synchronized static URL newUUIDFile() throws IOException,
            PluginException {

        URL url = null;
        String fileName = null;
        File dbDirectory = null;
        String canonicalPath = null;
        String persistDir = null;

        // create the index directory
        EnvProperties envProperties = PropertiesFactory.getInstance()
                .getEnvProperties();

        if (envProperties == null) {
            throw new PluginException("Unable to get an EnvProperties instance");
        }
        persistDir = envProperties.getEnvValue("PERSISTDIR");
        persistDir = FileUtil.convertFilePath(persistDir);

        if (persistDir == null)
            throw new PluginException(
                    "Unable to retrieve value for the PERSISTDIR");

        dbDirectory = new File(persistDir);
        dbDirectory.mkdirs();
        canonicalPath = dbDirectory.getCanonicalPath();

        // get a unique file name
        fileName = new String(canonicalPath + File.separator
                + java.util.UUID.randomUUID().toString());

        url = new File(fileName).toURI().toURL();

        return url;
    }

    public static String objectToString(Object obj) {
        String stringValue = null;
        if (obj == null) {
            stringValue = "null";
        } else if (obj instanceof String) {
            if (((String) obj).isEmpty()) {
                stringValue = "null";
            } else {
                stringValue = (String) obj;
            }
        } else if (obj instanceof Calendar) {
            stringValue = TimeUtil.formatCalendar((Calendar) obj);
        } else {
            stringValue = String.valueOf(obj);
        }
        return stringValue;
    }

    public static IMessageProducer getMessageProducer() {
        return MESSAGE_PRODUCER;
    }

    public void setMessageProducer(IMessageProducer message_producer) {
        MESSAGE_PRODUCER = message_producer;
    }

    public static IContextAdmin getContextAdmin() {
        return CONTEXT_ADMIN;
    }

    public void setContextAdmin(IContextAdmin admin) {
        CONTEXT_ADMIN = admin;
    }

    public static void checkPersistenceTimes(PluginDataObject[] pdos) {
        for (PluginDataObject record : pdos) {
            if (record instanceof IPersistable) {
                if (((IPersistable) record).getPersistenceTime() == null) {
                    ((IPersistable) record).setPersistenceTime(new Date());
                }
            } else {
                record.setInsertTime(Calendar.getInstance(TimeZone
                        .getTimeZone("GMT")));
            }
        }
    }

    /**
     * Send a message to alertViz
     * 
     * @param priority
     * @param pluginName
     * @param source
     * @param category
     * @param message
     * @param details
     */
    public static void sendMessageAlertViz(Priority priority,
            String pluginName, String source, String category, String message,
            String details, String audioFile) {

        StatusMessage sm = new StatusMessage();
        sm.setPriority(priority);
        sm.setPlugin(pluginName);
        sm.setCategory(category);
        sm.setMessage(message);
        sm.setMachineToCurrent();
        sm.setSourceKey(source);
        sm.setDetails(details);
        sm.setEventTime(new Date());
        sm.setAudioFile(audioFile);

        try {
            getMessageProducer().sendAsync(alertEndpoint, sm);
        } catch (Exception e) {
            logger.error("Could not send message to AlertViz");
        }
    }

    /**
     * @return the alertendpoint
     */
    public static String getAlertendpoint() {
        return alertEndpoint;
    }
}
