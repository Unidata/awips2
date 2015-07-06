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
package com.raytheon.uf.viz.alertview.logback;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkUtil;
import org.osgi.util.tracker.ServiceTracker;

import ch.qos.logback.classic.PatternLayout;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Appender;
import ch.qos.logback.core.AppenderBase;
import ch.qos.logback.core.Layout;

import com.raytheon.uf.viz.alertview.Alert;
import com.raytheon.uf.viz.alertview.AlertDestination;

/**
 * 
 * An {@link Appender} that converts {@link ILoggingEvent}s to {@link Alert}s
 * and sends them to all registered {@link AlertDestination}s. The
 * {@link Appender} can be configured with a {@link Layout} to provide custom
 * details in the alert, if no layout is provided the default layout is just the
 * exception with a full stack trace.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jun 16, 2015  4474     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class AlertViewAppender extends AppenderBase<ILoggingEvent> {

    protected Layout<ILoggingEvent> layout;

    private transient ServiceTracker<AlertDestination, AlertDestination> destinationTracker;

    @Override
    protected void append(ILoggingEvent event) {
        AlertDestination[] destinations = destinationTracker
                .getServices(new AlertDestination[0]);
        if(destinations == null || destinations.length == 0){
            return;
        }
        Alert alert = new LoggingEventAlert(layout, event);
        for (AlertDestination destination : destinations) {
            destination.handleAlert(alert);
        }

    }

    @Override
    public void start() {
        Bundle bundle = FrameworkUtil.getBundle(getClass());
        BundleContext context = bundle.getBundleContext();
        context.addBundleListener(new AppenderBundleListener(bundle, this));
        destinationTracker = new ServiceTracker<>(context, AlertDestination.class,
                null);
        destinationTracker.open();
        if (layout == null) {
            PatternLayout patternLayout = new PatternLayout();
            patternLayout.setContext(this.context);
            patternLayout.setPattern("%ex");
            layout = patternLayout;
        }
        layout.start();
        super.start();
    }

    @Override
    public void stop() {
        super.stop();
        layout.stop();
        if (destinationTracker != null) {
            destinationTracker.close();
            destinationTracker = null;
        }
    }

    @Override
    public boolean isStarted() {
        if (super.isStarted()) {
            return destinationTracker != null;
        } else {
            return false;
        }
    }

    public void setLayout(Layout<ILoggingEvent> layout) {
        this.layout = layout;
    }

}
