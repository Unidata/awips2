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
 * Performs a file size calculation on a directory or directory tree. The total
 * file size is reported by logging to the application log. The format
 * of the log message is
 * <P>
 *             {directory path}: Total file size {size} bytes
 * <P>
 * File scan defaults to the current directory (not tree search).
 * <P>
 * This class is intended to be injected into an {@link MonitorSrv} instance by
 * Mule. Because of that, all constructor arguments are of type String. 
 * <P>
 * 
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

public class EdexDirSizeMonitor extends AEdexDirMonitor {
    
    private static final String REPORT_FORMAT = "%s: Total file size %d bytes";
    
    /**
     * Constructor. Creates a directory size monitor using the default settings.
     */
    public EdexDirSizeMonitor() {
        super();
    }
    /**
     * Constructor. Creates a directory size monitor for the specified directory.
     * The scan is limited to the specified directory.
     * 
     * @param directory path to the directory to scan
     */
    public EdexDirSizeMonitor(String directory) {
        super(directory);
    }
    /**
     * Constructor. Creates a directory size monitor for the specified directory.
     * The second argument controls scanning depth. Pass "true" to scan the tree,
     * pass "false" to scan just the specified directory.
     *  
     * @param directory path to the directory to scan
     * @param tree scan tree flag. Use "true" to scan entire tree. 
     */
    public EdexDirSizeMonitor(String directory, String tree) {
        super(directory,tree);
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#execute()
     */
    @Override
    public void execute() {
        File dir = new File(dirToCheck);
        if (!dir.isDirectory()) {
            logger.warn(String.format(DIR_ERROR_FORMAT, dirToCheck));
            return;
        }
        long size = getSizes(dir,this.recursive);
        logger.info(String.format(REPORT_FORMAT, dirToCheck,size));
    }
    /**
     * Finds the total size of the files in a directory. Recursively
     * scans the directory and finds the total number of bytes in the
     * files in the directory. Setting the recursive flag to false limits
     * the search to a single directory.
     * 
     * @param dir the directory to search
     * @param recursive set true to search entire tree
     * 
     * @return number of bytes used by files in the directory
     */
    private long getSizes(File dir, boolean recursive) {
        long retVal = 0;
        for(File file : dir.listFiles(filter)){
            if (recursive && file.isDirectory()) {
                retVal += getSizes(file,recursive);
            } else {
                retVal += file.length();
            }
        }
        return retVal;
    }
    /* (non-Javadoc)
     * @see com.raytheon.edex.monitors.AEdexMonitor#setData(java.lang.String)
     */
    @Override
    public void setData(String data) {
        dirToCheck = data;
    }

}
