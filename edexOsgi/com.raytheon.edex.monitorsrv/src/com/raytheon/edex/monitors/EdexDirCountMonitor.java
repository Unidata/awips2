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

import java.io.File;

import com.raytheon.edex.services.MonitorSrv;

/**
 * Performs a count of the files in a directory or directory tree. The total
 * file count is reported by logging to the application log. The format
 * of the log message is
 * <P>
 *      {directory path}: Directory contains {count} files
 * <P>
 * File scan defaults to the current directory (not tree search).
 * <P>
 * This class is intended to be injected into an {@link MonitorSrv} instance by
 * Mule. Because of that, all constructor arguments are of type String. 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08May2008    1113       MW Fegan    Initial creation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public class EdexDirCountMonitor extends AEdexDirMonitor {
    /* format string for reporting statistics */
    private static final String REPORT_FORMAT = "%s: Directory contains %d files";

    /**
     * Constructor. Creates a monitor using the defaults.
     */
    public EdexDirCountMonitor() {
        super();
    }

    /**
     * Constructor. Creates a monitor for the specified directory.
     * The scan is non-recursive.
     * 
     * @param directory the directory to scan.
     */
    public EdexDirCountMonitor(String directory) {
        super(directory);
    }
    /**
     * Constructor. Creates a monitor for the specified directory.The second
     * argument controls scanning depth. Pass "true" to scan the tree, pass
     * "false" to scan just the specified directory.
     *  
     * @param path path to the directory to scan
     * @param tree scan tree flag. Use "true" to scan entire tree. 
     */
    public EdexDirCountMonitor(String directory, String tree) {
        super(directory,tree);
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.IEdexMonitor#execute()
     */
    @Override
    public void execute() {
        File dir = new File(dirToCheck);
        if (!dir.isDirectory()) {
            logger.warn(String.format(DIR_ERROR_FORMAT, dirToCheck));
            return;
        }
        long count = getCounts(dir,this.recursive);
        logger.info(String.format(REPORT_FORMAT, dirToCheck, count));
    }
    /**
     * 
     * @param dir
     * @param recursive
     * @return
     */
    private long getCounts(File dir,boolean recursive) {
        long retVal = 0;
        for(File file : dir.listFiles(filter)){
            if (recursive && file.isDirectory()) {
                retVal += getCounts(file,recursive);
            } else {
                retVal++;
            }
        }
        return retVal;
    }

    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.IEdexMonitor#setData(java.lang.String)
     */
    @Override
    public void setData(String data) {
        this.dirToCheck = data;
    }

}
