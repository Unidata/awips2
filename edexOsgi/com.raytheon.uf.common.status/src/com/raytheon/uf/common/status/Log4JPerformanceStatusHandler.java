package com.raytheon.uf.common.status;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Log4j Performance status handler.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 14, 2013   1584     mpduff      Initial creation
 * Jun 27, 2013   2142     njensen     Switched to SLF4J
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class Log4JPerformanceStatusHandler implements IPerformanceStatusHandler {
    /** Logger */
    private final Logger perfLog = LoggerFactory.getLogger("PerformanceLogger");

    /** Prefix to append to all log messages */
    private final String prefix;

    /**
     * Constructor.
     * 
     * @param prefix
     *            Message prefix
     */
    public Log4JPerformanceStatusHandler(String prefix) {
        this.prefix = prefix;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void log(String message) {
        perfLog.info(prefix + " " + message);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void logDuration(String message, long timeMs) {
        perfLog.info(prefix + " " + message + " took " + timeMs + " ms");
    }
}
