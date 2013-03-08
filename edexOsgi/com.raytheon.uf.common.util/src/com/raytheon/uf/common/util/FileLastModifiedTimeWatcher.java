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
package com.raytheon.uf.common.util;

import java.io.File;

import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Implementation of {@link IFileModifiedWatcher} that watches the last modified
 * time of the file. This class is not thread-safe. Intentionally
 * package-private, only FileUtil should be constructing instances.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 12, 2013 1778       djohnson     Initial creation
 * 
 * </pre>
 * 
 * @author djohnson
 * @version 1.0
 */
class FileLastModifiedTimeWatcher implements IFileModifiedWatcher {
    private static final IUFStatusHandler statusHandler = UFStatus
            .getHandler(FileLastModifiedTimeWatcher.class);

    private final File file;

    private long configFileLastModified;

    /**
     * Constructor.
     * 
     * @param file
     *            the file to watch
     */
    FileLastModifiedTimeWatcher(File file) {
        this.file = file;
        this.configFileLastModified = file.lastModified();
    }

    @Override
    public boolean hasBeenModified() {
        final long currentConfigFileLastModified = file.lastModified();
        final boolean fileModified = (currentConfigFileLastModified != configFileLastModified);

        if (statusHandler.isPriorityEnabled(Priority.DEBUG)) {
            statusHandler
                    .debug(String
                            .format("last known last modified [%s], current last modified [%s], changed [%s]",
                                    configFileLastModified,
                                    currentConfigFileLastModified, fileModified));
        }

        if (fileModified) {
            configFileLastModified = currentConfigFileLastModified;
        }
        return fileModified;
    }
}
