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
package com.raytheon.uf.edex.esb.camel;

import java.io.File;
import java.util.HashMap;
import java.util.Map;

import org.apache.camel.Exchange;
import org.apache.camel.component.file.GenericFile;
import org.apache.camel.component.file.GenericFileEndpoint;
import org.apache.camel.component.file.GenericFileExclusiveReadLockStrategy;
import org.apache.camel.component.file.GenericFileOperations;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 2, 2010            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

public class FileChangedExclusiveReadLockStrategy implements
        GenericFileExclusiveReadLockStrategy<File> {
    private Map<String, Long> modifyTimeMap = new HashMap<String, Long>();

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.camel.component.file.GenericFileExclusiveReadLockStrategy#
     * acquireExclusiveReadLock
     * (org.apache.camel.component.file.GenericFileOperations,
     * org.apache.camel.component.file.GenericFile, org.apache.camel.Exchange)
     */
    @Override
    public boolean acquireExclusiveReadLock(
            GenericFileOperations<File> operations, GenericFile<File> file,
            Exchange exchange) throws Exception {
        boolean rval = false;
        String key = file.getAbsoluteFilePath();
        long modifyTime = file.getLastModified();

        if (modifyTimeMap.containsKey(key)) {
            if (modifyTime == modifyTimeMap.get(key).longValue()) {
                rval = true;
            } else {
                modifyTimeMap.put(key, modifyTime);
            }
        } else {
            long curTime = System.currentTimeMillis();
            if (curTime - modifyTime > 60000) {
                // hasn't been modified in the last minute, just accept file
                rval = true;
            } else {
                modifyTimeMap.put(key, modifyTime);
            }
        }

        return rval;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.camel.component.file.GenericFileExclusiveReadLockStrategy#
     * prepareOnStartup(org.apache.camel.component.file.GenericFileOperations,
     * org.apache.camel.component.file.GenericFileEndpoint)
     */
    @Override
    public void prepareOnStartup(GenericFileOperations<File> operations,
            GenericFileEndpoint<File> endpoint) throws Exception {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.camel.component.file.GenericFileExclusiveReadLockStrategy#
     * releaseExclusiveReadLock
     * (org.apache.camel.component.file.GenericFileOperations,
     * org.apache.camel.component.file.GenericFile, org.apache.camel.Exchange)
     */
    @Override
    public void releaseExclusiveReadLock(
            GenericFileOperations<File> operations, GenericFile<File> file,
            Exchange exchange) throws Exception {
        modifyTimeMap.remove(file.getAbsoluteFilePath());
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.camel.component.file.GenericFileExclusiveReadLockStrategy#
     * setTimeout(long)
     */
    @Override
    public void setTimeout(long timeout) {
    }
}
