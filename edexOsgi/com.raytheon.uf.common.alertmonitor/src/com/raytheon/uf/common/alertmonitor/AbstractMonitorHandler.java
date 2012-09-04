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
package com.raytheon.uf.common.alertmonitor;

import java.util.Date;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * UFStatus abstract base class for for monitor event handlers.
 */
public abstract class AbstractMonitorHandler implements IUFStatusHandler {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(AbstractMonitorHandler.class, "GDN_ADMIN", "GDN_ADMIN");

    public static final String MONITOR = "MONITOR";

    private static String monitorEndpoint;

    private final String pluginId;

    private final String source;

    public AbstractMonitorHandler() {
        this.pluginId = "";
        this.source = "";
    }

    public AbstractMonitorHandler(String pluginId, String source) {
        this.pluginId = pluginId;
        this.source = source;
    }

    @Override
    public boolean isPriorityEnabled(Priority p) {
        return true;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
     * .common.status.UFStatus)
     */
    @Override
    public void handle(UFStatus status) {
        this.handle(status.getPriority(), status.getMessage(),
                status.getException());
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
	 * .common.status.UFStatus, java.lang.String)
	 */
	@Override
	public void handle(UFStatus status, String category) {
		this.handle(status.getPriority(), category, status.getMessage(),
				status.getException());
	}

    @Override
    public void handle(Priority p, String msg) {
		this.handle(p, msg, (Throwable) null);
    }

    @Override
    public void handle(Priority p, String msg, Throwable t) {
        StringBuilder sb = new StringBuilder(msg.length() + 64);
        sb.append(MONITOR);

        if (source != null) {
            sb.append(": ");
            sb.append(source);
        }

        sb.append(" - ");
        sb.append(msg);
        msg = sb.toString();
        sendMonitorMessage(p, msg, null, null);
    }

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
	 * .common.status.UFStatus.Priority, java.lang.String, java.lang.String)
	 */
	@Override
	public void handle(Priority priority, String category, String message) {
		handle(priority, category, message, (Throwable) null);
	}

	@Override
	public void handle(Priority p, String category, String msg, Throwable t) {
		StringBuilder sb = new StringBuilder(msg.length() + 64);
		sb.append(MONITOR);

		if (source != null) {
			sb.append(": ");
			sb.append(source);
		}

		sb.append(" - ");
		sb.append(msg);
		msg = sb.toString();
		sendMonitorMessage(p, category, msg, null, null);
	}

	/**
	 * Send a message to alertViz
	 * 
	 * @param priority
	 * @param message
	 * @param details
	 * @param audioFile
	 */
    private void sendMonitorMessage(Priority priority, String message,
            String details, String audioFile) {
		sendMonitorMessage(priority, MONITOR, message, details, audioFile);
	}

	/**
	 * Send a message to alertViz
	 * 
	 * @param priority
	 * @param category
	 * @param message
	 * @param details
	 * @param audioFile
	 */
	private void sendMonitorMessage(Priority priority, String category,
			String message, String details, String audioFile) {

        StatusMessage sm = new StatusMessage();
        sm.setPriority(priority);
        sm.setPlugin(pluginId);
		sm.setCategory(category);
        sm.setMessage(message);
        sm.setSourceKey(source);
        sm.setDetails(details);
        sm.setEventTime(new Date());
        sm.setAudioFile(audioFile);

        try {
            sendMonitorMessage(sm);
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Could not send message to the AlertMonitor", e);
        }
    }

    /**
     * 
     * @param sm
     * @throws Exception
     */
    abstract protected void sendMonitorMessage(StatusMessage sm)
            throws Exception;

    /**
     * @return the monitorEndpoint
     */
    public static String getMonitorEndpoint() {
        return monitorEndpoint;
    }

    /**
     * @param monitorEndpoint
     *            the monitorEndpoint to set
     */
    public void setMonitorEndpoint(String monitorEndpoint) {
        AbstractMonitorHandler.monitorEndpoint = monitorEndpoint;
    }

    @Override
    public void debug(String message) {
        handle(Priority.DEBUG, message);
    }

    @Override
	public void debug(String category, String message) {
		handle(Priority.DEBUG, category, message);
	}

	@Override
    public void info(String message) {
        handle(Priority.INFO, message);
    }

    @Override
	public void info(String category, String message) {
		handle(Priority.INFO, category, message);
	}

	@Override
    public void warn(String message) {
        handle(Priority.WARN, message);
    }

	public void warn(String category, String message) {
		handle(Priority.WARN, category, message);
	}

    @Override
    public void error(String message) {
        handle(Priority.ERROR, message);
    }

    @Override
	public void error(String category, String message) {
		handle(Priority.ERROR, category, message);
	}

	@Override
    public void error(String message, Throwable throwable) {
        handle(Priority.ERROR, message, throwable);
    }

    @Override
	public void error(String category, String message, Throwable throwable) {
		handle(Priority.ERROR, category, message, throwable);
	}

	@Override
    public void fatal(String message, Throwable throwable) {
        handle(Priority.FATAL, message, throwable);
    }

	@Override
	public void fatal(String category, String message, Throwable throwable) {
		handle(Priority.FATAL, category, message, throwable);
	}
}
