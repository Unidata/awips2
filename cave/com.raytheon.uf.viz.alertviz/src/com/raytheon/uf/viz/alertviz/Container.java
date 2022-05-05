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
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;

import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.util.TimeUtil;
import com.raytheon.uf.viz.alertviz.config.AlertMetadata;
import com.raytheon.uf.viz.alertviz.config.Category;
import com.raytheon.uf.viz.alertviz.config.Configuration;
import com.raytheon.uf.viz.alertviz.config.ForcedConfiguration;
import com.raytheon.uf.viz.alertviz.config.Source;
import com.raytheon.uf.viz.alertviz.internal.LogMessageDAO;
import com.raytheon.uf.viz.core.VizApp;
import com.raytheon.uf.viz.core.localization.LocalizationManager;

/**
 * Container provides basic message dispatching services
 *
 * Partially Ported from GDN_Container.C
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Sep 08, 2008  1433     chammack  Initial creation
 * Oct 18, 2010  5849     cjeanbap  NullPointerExceptin thrown if category is
 *                                  null
 * Jun 03, 2013  2026     randerso  Fixed typo
 * Jul 27, 2015  4654     skorolev  Added a localization level filtration
 * Sep 21, 2015  4654     njensen   Made filter logic strict instead of eager
 * Jan 14, 2016  5054     randerso  Remove dummy shell
 * Feb 23, 2016  5314     dgilling  Support changes to PurgeLogJob.
 * Mar 20, 2018  7096     randerso  Remove call to StatusMessage.setEventTime()
 * Aug 13, 2018  6670     randerso  Changed isShotGun() to consider source so
 *                                  FSS monitor icons work correctly.
 * Oct 04, 2018  7484     randerso  Changed to use AV_ADMIN for internal errors
 * Oct 08, 2018  7515     randerso  Adjusted priorities of AlertViz internal
 *                                  errors. Improved format of shotgun log message.
 * Jun 05, 2020  8175     bhurley   Fix message flood handling
 *
 * </pre>
 *
 * @author chammack
 */

public class Container implements IConfigurationChangedListener {

    public static final String CATEGORY_MISSING = "Category was not found";

    public static final String SOURCE_MISSING = "Source was not found";

    private static final org.apache.log4j.Logger adminLogger = Logger
            .getLogger("AlertVizAdminLogger");

    private Configuration configuration;

    private final ForcedConfiguration forcedConfiguration;

    private StatusMessage lastMessage;

    private final Set<IAlertArrivedCallback> callbacks;

    private long lastMessageReceivedInSec = 0;

    private int messagesReceivedInSecond = 0;

    private static final int UI_FLOOD_THRESHOLD = 50;

    private static final long SHOTGUN_MESSAGE_MILLISECOND_THRESHOLD = TimeUtil.MILLIS_PER_SECOND;

    private String lastErrorDialogMessage = null;

    private long lastErrorDialogTime = 0;

    private int shotgunMessageCount;

    private long shotgunMessageStartTime;

    public Container(Set<IAlertArrivedCallback> callbacks) {
        configurationChanged();
        forcedConfiguration = ConfigurationManager.getInstance()
                .getForcedConfiguration();
        ConfigurationManager.getInstance().addListener(this);
        this.callbacks = callbacks;
        PurgeLogJob archive = PurgeLogJob.getInstance();
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
            message.setSourceKey("AV_ADMIN");
            message.setCategory("AV_ADMIN");
            message.setPriority(Priority.SIGNIFICANT);
            message.setMessage(message.getMessage() + " (" + CATEGORY_MISSING
                    + ": " + categoryKey + ")");
            messageReceived(message);
            return;
        }

        if ((source == null) || (source.getConfigurationItem() == null)) {
            message.setSourceKey("AV_ADMIN");
            message.setCategory("AV_ADMIN");
            message.setPriority(Priority.SIGNIFICANT);
            message.setMessage(message.getMessage() + " (" + SOURCE_MISSING
                    + ": " + sourceKey + ")");
            messageReceived(message);
            return;
        }

        // Check to see if this message is part of a shotgun blast
        if (isShotGun(message)) {
            return;
        }

        addToLog(message);

        if (message.getFilters() != null && !message.getFilters().isEmpty()) {
            Map<String, String> filters = message.getFilters();
            LocalizationLevel[] lvls = LocalizationLevel.values();
            boolean matchFound = false;
            for (LocalizationLevel lvl2 : lvls) {
                String lvl = LocalizationManager.getContextName(lvl2);
                String key = lvl2.name();
                if (filters.containsKey(key)) {
                    String value = filters.get(key);
                    if (value != null && value.equalsIgnoreCase(lvl)) {
                        matchFound = true;
                        break;
                    }
                }
            }

            if (!matchFound) {
                return;
            }
        }

        AlertMetadata amd = source.getConfigurationItem()
                .lookup(message.getPriority());

        if (forcedConfiguration != null) {
            amd = forcedConfiguration.applyForcedSettings(amd, message);
        }

        final AlertMetadata metadata = amd;

        if (metadata.isAudioEnabled() || metadata.isBlink()
                || metadata.isPopup() || metadata.isText()) {

            updateMessageStatistics();

            // Check to make sure messages aren't coming in so fast it could
            // cause a system issue
            if (this.messagesReceivedInSecond > UI_FLOOD_THRESHOLD) {
                logInternal(Priority.WARN, "**** POTENTIAL UI FLOOD");
                return;
            }

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
    }

    public void addToEventLog(Priority priority, String msg) {
        // for now, just send to the regular log
        StatusMessage sm = new StatusMessage();
        sm.setPriority(priority);
        sm.setMachineToCurrent();
        sm.setSourceKey("AV_ADMIN");
        sm.setCategory("AV_ADMIN");
        sm.setMessage(msg);
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
            final long shotgunMessageCheckTime = this.shotgunMessageStartTime == 0
                    ? this.lastMessage.getEventTime().getTime()
                    : this.shotgunMessageStartTime;

            if (this.lastMessage.getSourceKey().equals(message.getSourceKey())
                    && this.lastMessage.getCategory()
                            .equals(message.getCategory())
                    && (this.lastMessage.getPriority() == message.getPriority())
                    && this.lastMessage.getMessage()
                            .equals(message.getMessage())
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
                            .append(" milliseconds. For message: ").append('(')
                            .append(this.lastMessage.getPriority().ordinal())
                            .append(") | ")
                            .append(this.lastMessage.getCategory())
                            .append(" | ")
                            .append(this.lastMessage.getSourceKey())
                            .append(": ").append(this.lastMessage.getMessage());
                    StatusMessage sm = new StatusMessage("AV_ADMIN", "AV_ADMIN",
                            Priority.INFO, this.lastMessage.getPlugin(),
                            sb.toString(), null);
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
        long currentSecond = System.currentTimeMillis()
                / TimeUtil.MILLIS_PER_SECOND;
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
                    if ((System.currentTimeMillis()
                            - lastErrorDialogTime) < TimeUtil.MILLIS_PER_MINUTE) {
                        printError = false;
                    }
                }
            } else if (lastErrorDialogMessage == null) {
                if ((System.currentTimeMillis()
                        - lastErrorDialogTime) < TimeUtil.MILLIS_PER_MINUTE) {
                    printError = false;
                }
            }

            if (printError) {
                Container.logInternal(Priority.ERROR,
                        "Container: exception saving to internal log database: ",
                        e);
                lastErrorDialogMessage = errorMsg;
                lastErrorDialogTime = System.currentTimeMillis();
                StringBuilder tmp = new StringBuilder();
                try (StringWriter sWriter = new StringWriter();
                        PrintWriter pWriter = new PrintWriter(sWriter)) {
                    e.printStackTrace(pWriter);
                    pWriter.flush();
                    tmp.append(sWriter.toString());
                } catch (Exception e1) {
                    // ignore
                }

                // Status status = null;
                ErrorDialog.openError(null, "Error saving to internal database",
                        "Serious internal error occurred",
                        new Status(IStatus.ERROR, Activator.PLUGIN_ID,
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
                && (statMsg.getMessage().contains(CATEGORY_MISSING)
                        || statMsg.getMessage().contains(SOURCE_MISSING));
    }

    public static void logInternal(Priority p, String msg) {
        logInternal(p, msg, null);
    }

    public static void logInternal(Priority p, String msg, Throwable t) {
        logInternal(new StatusMessage("AV_ADMIN", "AV_ADMIN", p,
                "com.raytheon.uf.viz.alertViz", msg, t));
    }

    public static void logInternal(StatusMessage message) {
        String cat = message.getCategory();
        String source = message.getSourceKey();

        boolean isInternal = ((cat != null) && "AV_ADMIN".equalsIgnoreCase(cat))
                || ((source != null) && "AV_ADMIN".equalsIgnoreCase(source));
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

            case EVENTA:
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
