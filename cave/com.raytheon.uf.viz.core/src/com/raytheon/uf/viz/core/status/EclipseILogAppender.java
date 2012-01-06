package com.raytheon.uf.viz.core.status;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.Level;
import org.apache.log4j.spi.LoggingEvent;
import org.eclipse.core.internal.runtime.InternalPlatform;
import org.eclipse.core.runtime.ILog;
import org.osgi.framework.Bundle;

import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Appender for the Eclipse status framework.
 */
@SuppressWarnings("restriction")
public class EclipseILogAppender extends AppenderSkeleton {

    private String pluginId;

    private String category;

    private String source;

    private ILog log;

    @Override
    protected void append(LoggingEvent arg0) {
        String pluginId = arg0.getLoggerName();
        Level level = arg0.getLevel();
        Throwable t = arg0.getThrowableInformation() == null ? null : arg0
                .getThrowableInformation().getThrowable();
        Priority priority = toPriority(level);
        UFStatus status = new UFStatus(priority, arg0.getRenderedMessage(), t);

        getLog().log(new ILogStatus(status, category, source, pluginId));
    }

    private ILog getLog() {
        if (log == null) {
            Bundle bundle = InternalPlatform.getDefault().getBundle(pluginId);
            log = InternalPlatform.getDefault().getLog(bundle);
        }
        return log;
    }

    @Override
    public void close() {
        // do nothing
    }

    @Override
    public boolean requiresLayout() {
        return false;
    }

    /**
     * Transform log4j logging level to iStatus logging level.
     * 
     * @param level
     * @return
     */
    private static Priority toPriority(Level level) {
        switch (level.toInt()) {
        case Level.TRACE_INT:
            return Priority.VERBOSE;
        case Level.DEBUG_INT:
            return Priority.DEBUG;
        case Level.INFO_INT:
            return Priority.INFO;
        case Level.WARN_INT:
            return Priority.WARN;
        case Level.ERROR_INT:
            return Priority.ERROR;
        case Level.FATAL_INT:
            return Priority.FATAL;
        default:
            return Priority.INFO;
        }
    }

    /**
     * @return the pluginId
     */
    public String getPluginId() {
        return pluginId;
    }

    /**
     * @param pluginId
     *            the pluginId to set
     */
    public void setPluginId(String pluginId) {
        this.pluginId = pluginId;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @param category
     *            the category to set
     */
    public void setCategory(String category) {
        this.category = category;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @param source
     *            the source to set
     */
    public void setSource(String source) {
        this.source = source;
    }

    private static class ILogStatus extends VizStatusInternal {
        public ILogStatus(UFStatus status, String category, String source,
                String pluginName) {
            super(status, category, source, pluginName);
        }
    }
}