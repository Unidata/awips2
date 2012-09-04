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
package com.raytheon.uf.common.status;

import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 15, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SysErrStatusHandler implements IUFStatusHandler {

    private final String pluginId;

    private final String category;

    private final String source;

    public SysErrStatusHandler(String pluginId, String category, String source) {
        this.pluginId = pluginId;
        this.category = category;
        this.source = source;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandler#isPriorityEnabled(com.
     * raytheon.uf.common.status.UFStatus.Priority)
     */
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
        handle(status.getPriority(), status.getMessage(), status.getException());
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
		handle(status.getPriority(), category, status.getMessage(),
				status.getException());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
	 * .common.status.UFStatus.Priority, java.lang.String)
	 */
    @Override
    public void handle(Priority priority, String message) {
		handle(priority, message, (Throwable) null);
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
	public void handle(Priority priority, String message, Throwable throwable) {
		handle(priority, category, message, throwable);
	}

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.common.status.IUFStatusHandler#handle(com.raytheon.uf
     * .common.status.UFStatus.Priority, java.lang.String, java.lang.Throwable)
     */
    @Override
	public void handle(Priority priority, String category, String message,
			Throwable throwable) {
        StringBuilder sb = new StringBuilder();
        sb.append(priority).append(' ');
        sb.append(this.pluginId).append(": ");

        if (this.category != null) {
            sb.append(this.category);

            if (this.source != null) {
                sb.append(": ");
                sb.append(this.source);
            }
            sb.append(" - ");
        }

        sb.append(message);
        System.err.println(sb.toString());
        if (throwable != null) {
            throwable.printStackTrace(System.err);
        }
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

    @Override
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
