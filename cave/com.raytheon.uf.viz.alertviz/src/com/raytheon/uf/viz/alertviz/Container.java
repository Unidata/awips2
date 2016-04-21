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
package com.raytheon.uf.viz.alertviz;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.SimulatedTime;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ForcedConfiguration;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;
import com.raytheon.uf.viz.alertviz.internal.PurgeLogJob;
import com.raytheon.uf.viz.core.VizApp;

/**
 * Container provides basic message dispatching services
 * 
 * Partially Ported from GDN_Container.C
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 8, 2008  1433       chammack    Initial creation
 * Oct 18, 2010 5849       cjeanbap    NullPointerExceptin thrown if category is null
 * Jun 03, 2013 2026       randerso    Fixed typo
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class Container implements IConfigurationChangedListener {

    public static final String CATEGORY_MISSING = "Category was not found";

    public static final String SOURCE_MISSING = "Source was not found";

    private transient static final org.apache.log4j.Logger adminLogger = Logger
            .getLogger("AlertVizAdminLogger");

    private Configuration configuration;

    private ForcedConfiguration forcedConfiguration;

    private StatusMessage lastMessage;

    private Set<IAlertArrivedCallback> callbacks;

    private long lastMessageReceivedInSec = 0;

    private int messagesReceivedInSecond = 0;

    private static final int UI_FLOOD_THRESHOLD = 50;

    private static final int SHOTGUN_MESSAGE_MILLISECOND_THRESHOLD = 1000;

    private String lastErrorDialogMessage;

    private long lastErrorDialogTime;

    private int shotgunMessageCount;

    private long shotgunMessageStartTime;

    public Container(Set<IAlertArrivedCallback> callbacks) {
        configurationChanged();
        forcedConfiguration = ConfigurationManager.getInstance()
                .getForcedConfiguration();
        ConfigurationManager.getInstance().addListener(this);
        this.callbacks = callbacks;
        PurgeLogJob archive = new PurgeLogJob();
        archive.schedule();
    }

    public void messageReceived(final StatusMessage message) {

        // If sending machine is defined, check for a match. If not matched,
        // then return and take no action. A defined sending machine means
        // that the sender only wants this message conveyed to the same
        // machine is it was sent from.

        String sourceKey = message.getSourceKey();
        String categoryKey = message.getCategory();
        Source source = configuration.lookupSource(sourceKey);
        final Category category = configuration.lookupCategory(categoryKey);

        if (category == null) {
            message.setCategory("GDN_ADMIN");
            message.setMessage(message.getMessage() + " (" + CATEGORY_MISSING
                    + ": " + categoryKey + ")");
            messageReceived(message);
            return;
        }

        if ((source == null) || (source.getConfigurationItem() == null)) {
            message.setSourceKey("GDN_ADMIN");
            message.setCategory("GDN_ADMIN");
            message.setMessage(message.getMessage() + " (" + SOURCE_MISSING
                    + ": " + sourceKey + ")");
            messageReceived(message);
            return;
        }

        // Check to see if this message is part of a shotgun blast
        if (isShotGun(message)) {
            return;
        }

        updateMessageStatistics();

        addToLog(message);

        // Check to make sure messages aren't coming in so fast it could cause a
        // system issue
        if (this.messagesReceivedInSecond > UI_FLOOD_THRESHOLD) {
            logInternal(Priority.WARN, "**** POTENTIAL UI FLOOD");
            return;
        }

        AlertMetadata amd = source.getConfigurationItem().lookup(
                message.getPriority());

        if (forcedConfiguration != null) {
            amd = forcedConfiguration.applyForcedSettings(amd, message);
        }

        final AlertMetadata metadata = amd;

        VizApp.runAsync(new Runnable() {

            @Override
            public void run() {
                for (IAlertArrivedCallback callback : callbacks) {
                    callback.alertArrived(message, metadata, category,
                            configuration.getGlobalConfiguration());
                }
            }

        });

    }

    public void addToEventLog(Priority priority, String msg) {
        // for now, just send to the regular log
        StatusMessage sm = new StatusMessage();
        sm.setPriority(priority);
        sm.setMachineToCurrent();
        sm.setSourceKey("GDN_ADMIN");
        sm.setCategory("GDN_ADMIN");
        sm.setMessage(msg);
        sm.setEventTime(SimulatedTime.getSystemTime().getTime());
        addToLog(sm);
    }

    /**
     * Returns true if the message is a shotgun blast (e.g. identical message)
     * 
     * @param message
     * @return
     */
    private boolean isShotGun(StatusMessage message) {
        boolean retVal = false;
        if (lastMessage != null) {
            final long shotgunMessageCheckTime = this.shotgunMessageStartTime == 0 ? this.lastMessage
                    .getEventTime().getTime() : this.shotgunMessageStartTime;

            if (this.lastMessage.getCategory().equals(message.getCategory())
                    && (this.lastMessage.getPriority() == message.getPriority())
                    && this.lastMessage.getMessage().equals(
                            message.getMessage())
                    && (Math.abs(message.getEventTime().getTime()
                            - shotgunMessageCheckTime) < SHOTGUN_MESSAGE_MILLISECOND_THRESHOLD)) {
                retVal = true;
                ++this.shotgunMessageCount;
                if (this.shotgunMessageStartTime == 0) {
                    this.shotgunMessageStartTime = lastMessage.getEventTime()
                            .getTime();
                }
            } else {
                if (this.shotgunMessageCount > 1) {
                    StringBuilder sb = new StringBuilder("Received ")
                            .append(this.shotgunMessageCount)
                            .append(" duplicate messages in ")
                            .append(this.lastMessage.getEventTime().getTime()
                                    - this.shotgunMessageStartTime)
                            .append(" milliseconds. For message: ")
                            .append(this.lastMessage.getCategory()).append(":")
                            .append(this.lastMessage.getSourceKey())
                            .append(" ").append(this.lastMessage.getMessage());
                    StatusMessage sm = new StatusMessage(
                            this.lastMessage.getSourceKey(), "GDN_ADMIN",
                            this.lastMessage.getPriority(),
                            this.lastMessage.getPlugin(), sb.toString(), null);
                    sm.setEventTime(SimulatedTime.getSystemTime().getTime());
                    logInternal(sm);
                }
                this.shotgunMessageStartTime = 0;
                this.shotgunMessageCount = 1;
            }
        }

        this.lastMessage = message;

        return retVal;
    }

    public void updateMessageStatistics() {
        long currentSecond = System.currentTimeMillis() / 1000;
        if (currentSecond == this.lastMessageReceivedInSec) {
            this.messagesReceivedInSecond++;
        } else {
            this.messagesReceivedInSecond = 1;
            this.lastMessageReceivedInSec = currentSecond;
        }

    }

    public void addToLog(StatusMessage message) {
        try {
            LogMessageDAO.getInstance().save(message);
        } catch (AlertvizException e) {
            String errorMsg = e.getMessage();
            boolean printError = true;
            if (errorMsg != null) {
                if (errorMsg.equals(lastErrorDialogMessage)) {
                    if ((System.currentTimeMillis() - lastErrorDialogTime) < 60000) {
                        printError = false;
                    }
                }
            } else if (lastErrorDialogMessage == null) {
                if ((System.currentTimeMillis() - lastErrorDialogTime) < 60000) {
                    printError = false;
                }
            }

            if (printError) {
                Container
                        .logInternal(
                                Priority.ERROR,
                                "Container: exception saving to internal log database: ",
                                e);
                lastErrorDialogMessage = errorMsg;
                lastErrorDialogTime = System.currentTimeMillis();
                StringBuilder tmp = new StringBuilder();
                PrintWriter pWriter = null;
                try {
                    StringWriter sWriter = new StringWriter();
                    pWriter = new PrintWriter(sWriter);
                    e.printStackTrace(pWriter);
                    pWriter.flush();
                    tmp.append(sWriter.toString());
                } catch (Exception e1) {
                    // ignore
                } finally {
                    if (pWriter != null) {
                        pWriter.close();
                    }
                }

                // Status status = null;
                ErrorDialog.openError(new Shell(),
                        "Error saving to internal database",
                        "Serious internal error occurred", new Status(
                                Status.ERROR, Activator.PLUGIN_ID,
                                "Saved failed", new Exception(tmp.toString())));
            }
        }
    }

    @Override
    public void configurationChanged() {
        configuration = ConfigurationManager.getInstance()
                .getCurrentConfiguration();
    }

    public static boolean hasMissing(StatusMessage statMsg) {
        return (statMsg.getMessage() != null)
                && (statMsg.getMessage().contains(CATEGORY_MISSING) || statMsg
                        .getMessage().contains(SOURCE_MISSING));
    }

    public static void logInternal(Priority p, String msg) {
        logInternal(p, msg, null);
    }

    public static void logInternal(Priority p, String msg, Throwable t) {
        logInternal(new StatusMessage("GDN_ADMIN", "GDN_ADMIN", p,
                "com.raytheon.uf.viz.alertViz", msg, t));
    }

    public static void logInternal(StatusMessage message) {
        String cat = message.getCategory();
        String source = message.getSourceKey();

        boolean isInternal = ((cat != null) && cat
                .equalsIgnoreCase("GDN_ADMIN"))
                || ((source != null) && source.equalsIgnoreCase("GDN_ADMIN"));
        if (isInternal) {
            switch (message.getPriority()) {
            case CRITICAL:
                adminLogger.fatal(message);
                break;
            case SIGNIFICANT:
                adminLogger.error(message);
                break;
            case PROBLEM:
                adminLogger.warn(message);
                break;
            case EVENTA: // fall through
            case EVENTB:
                adminLogger.info(message);
                break;
            case VERBOSE:
                adminLogger.debug(message);
                break;
            }
        }
    }
}
