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
package com.raytheon.edex.monitors;

import java.io.FilenameFilter;

import com.raytheon.uf.common.util.file.FilenameFilters;

/**
 * An abstract base class for directory monitors.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08May2008    1113       MW Fegan    Initial creation.
 * Mar 14, 2013 1794       djohnson    Consolidate common FilenameFilter implementations.
 * 
 * </pre>
 * 
 * @author mfegan
 * @version 1.0
 */

public abstract class AEdexDirMonitor extends AEdexMonitor {
    /** format string for invalid directory messages */
    protected static final String DIR_ERROR_FORMAT = "Specified path [%s] is not a directory";
    /** indicates if the directory search is recursive */
    protected boolean recursive = false;
    
    /** path to the directory to scan */
    protected String dirToCheck = ".";

    /* file filter that screens out "dot" files */
    protected FilenameFilter filter = FilenameFilters.NO_LINUX_HIDDEN_FILES;

    /**
     * Constructor. Takes no action.
     */
    public AEdexDirMonitor() {
        // intentionally empty
    }
    /**
     * Constructor. Sets the {@code dirToCheck} to the specified value.
     */
    public AEdexDirMonitor(String directory) {
        this.dirToCheck = directory;
    }
    /**
     * Constructor. Sets the {@code dirToCheck} and {@code recursive} attributes
     * to the specified values.
     */
    public AEdexDirMonitor(String directory, String tree) {
        this.dirToCheck = directory;
        this.recursive = Boolean.valueOf(tree);
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#execute()
     */
    @Override
    abstract public void execute();

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#setData(java.lang.String)
     */
    @Override
    abstract public void setData(String data);
}
