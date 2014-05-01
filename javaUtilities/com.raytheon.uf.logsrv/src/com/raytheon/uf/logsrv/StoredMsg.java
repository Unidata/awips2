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
package com.raytheon.uf.logsrv;

import java.util.Date;

import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.IThrowableProxy;
import ch.qos.logback.classic.spi.StackTraceElementProxy;
import ch.qos.logback.classic.spi.ThrowableProxyUtil;

/**
 * A translation of a logging event to a POJO that closely resembles what is in
 * the database.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 27, 2013            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class StoredMsg {

    private Date eventTime;

    private String message;

    private String javaClass;

    private int lineNumber;

    // TODO build date or build number? not sure how to get logback to send that
    // along

    private String stacktrace;

    private String threadName;

    private String machineName;

    private String level;

    public StoredMsg(ILoggingEvent event) {
        eventTime = new Date(event.getTimeStamp());

        // find the most underlying cause
        IThrowableProxy cause = event.getThrowableProxy();
        if (cause != null) {
            stacktrace = ThrowableProxyUtil.asString(cause);
            while (cause.getCause() != null) {
                cause = cause.getCause();
            }

            message = cause.getMessage();
            StackTraceElementProxy[] stack = cause
                    .getStackTraceElementProxyArray();
            if (stack != null && stack.length > 0) {
                StackTraceElement ste = stack[0].getStackTraceElement();
                lineNumber = ste.getLineNumber();
                javaClass = ste.getClassName();
            }
        } else {
            message = event.getMessage();
        }
        threadName = event.getThreadName();
        machineName = event.getLoggerContextVO().getPropertyMap()
                .get("HOSTNAME");
        level = event.getLevel().toString();

    }

    public Date getEventTime() {
        return eventTime;
    }

    public void setEventTime(Date eventTime) {
        this.eventTime = eventTime;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getJavaClass() {
        return javaClass;
    }

    public void setJavaClass(String javaClass) {
        this.javaClass = javaClass;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public void setLineNumber(int lineNumber) {
        this.lineNumber = lineNumber;
    }

    public String getThreadName() {
        return threadName;
    }

    public void setThreadName(String threadName) {
        this.threadName = threadName;
    }

    public String getMachineName() {
        return machineName;
    }

    public void setMachineName(String machineName) {
        this.machineName = machineName;
    }

    public String getStacktrace() {
        return stacktrace;
    }

    public void setStacktrace(String stacktrace) {
        this.stacktrace = stacktrace;
    }

    public String getLevel() {
        return level;
    }

    public void setLevel(String level) {
        this.level = level;
    }

    @Override
    public String toString() {
        return "StoredMsg [eventTime=" + eventTime + ", message=" + message
                + ", javaClass=" + javaClass + ", lineNumber=" + lineNumber
                + ", stacktrace=" + stacktrace + ", threadName=" + threadName
                + ", machineName=" + machineName + ", level=" + level + "]";
    }

}
