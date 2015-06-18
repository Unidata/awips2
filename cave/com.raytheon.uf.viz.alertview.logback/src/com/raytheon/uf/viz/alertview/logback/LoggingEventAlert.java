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

import java.util.Date;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.Layout;

import com.raytheon.uf.viz.alertview.Alert;

/**
 * 
 * Wraps an {@link ILoggingEvent} in the {@link Alert} interface so it can be
 * added to the AlertView.
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
public class LoggingEventAlert implements Alert {

    private final Layout<ILoggingEvent> layout;

    private final ILoggingEvent event;

    public LoggingEventAlert(Layout<ILoggingEvent> layout, ILoggingEvent event) {
        this.layout = layout;
        this.event = event;
    }

    @Override
    public Date getTime() {
        return new Date(event.getTimeStamp());
    }

    @Override
    public Priority getPriority() {
        if (event.getLevel() == Level.ERROR) {
            return Priority.ERROR;
        } else if (event.getLevel() == Level.WARN) {
            return Priority.WARN;
        } else if (event.getLevel() == Level.INFO) {
            return Priority.INFO;
        } else {
            return Priority.DEBUG;
        }
    }

    @Override
    public String getOrigin() {
        return event.getLoggerName();
    }

    @Override
    public String getMessage() {
        return event.getFormattedMessage();
    }

    @Override
    public String getDetails() {
        return layout.doLayout(event);
    }

}
