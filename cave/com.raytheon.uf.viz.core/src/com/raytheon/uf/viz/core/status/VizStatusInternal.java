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
package com.raytheon.uf.viz.core.status;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;

import com.raytheon.uf.common.message.StatusMessage;
import com.raytheon.uf.common.status.UFStatus;

/**
 * An internal remapping of {@link UFStatus} to Eclipse {@link IStatus}
 * 
 * <B>This class should not be used outside of internal status processing
 * code.</B>
 * 
 * 
 * 
 * Note that in cases where VizStatus must be mapped to a traditional Eclipse
 * Status, the following mapping is used:
 * <UL>
 * <LI><B>CRITICAL</B>, <B>SIGNIFICANT</B>, and <B>PROBLEM</B> are mapped to the
 * Eclipse <B>ERROR</B>
 * <LI><B>EVENTA</B> and <B>EVENTB</B> are mapped to Eclipse <B>WARNING</B>
 * <LI><B>VERBOSE</B> is mapped to Eclipse <B>INFO</B>
 * </UL>
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 6, 2008            chammack     Initial creation
 * </pre>
 * 
 * @author chammack
 * @version 1.0
 */

public class VizStatusInternal extends UFStatus implements IStatus {

    /** Eclipse version of severity */
    private final int severity;

    /** The category */
    protected final String category;

    /** The source (may be null) */
    protected final String source;

    /** The plugin name */
    protected final String pluginName;

    /**
     * Protected constructor
     * 
     * @param status
     */
    protected VizStatusInternal(UFStatus status, String category,
            String source, String pluginName) {
        super(status);
        this.category = category;
        this.source = source;
        this.pluginName = pluginName;
        this.severity = priority.ordinal() < 3 ? Status.ERROR : priority
                .ordinal() < 5 ? Status.WARNING : Status.INFO;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#getChildren()
     */
    @Override
    public IStatus[] getChildren() {
        return new IStatus[0];
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#getCode()
     */
    @Override
    public int getCode() {
        return 0;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#getSeverity()
     */
    @Override
    public int getSeverity() {
        return severity;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#isMultiStatus()
     */
    @Override
    public boolean isMultiStatus() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#isOK()
     */
    @Override
    public boolean isOK() {
        return false;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IStatus#matches(int)
     */
    @Override
    public boolean matches(int severityMask) {
        return false;
    }

    /**
     * @return the plugin
     */
    @Override
    public String getPlugin() {
        return pluginName;
    }

    /**
     * @return the category
     */
    public String getCategory() {
        return category;
    }

    /**
     * @return the source
     */
    public String getSource() {
        return source;
    }

    /**
     * @return the pluginName
     */
    public String getPluginName() {
        return pluginName;
    }

    public StatusMessage toStatusMessage() {
        return new StatusMessage(this.source, this.category, this.priority,
                this.pluginName, this.message, this.exception);
    }

}
