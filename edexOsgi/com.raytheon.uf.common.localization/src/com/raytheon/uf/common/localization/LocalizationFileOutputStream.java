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
package com.raytheon.uf.common.localization;

import java.io.FileNotFoundException;
import java.io.IOException;

import com.raytheon.uf.common.localization.exception.LocalizationException;
import com.raytheon.uf.common.localization.exception.LocalizationOpFailedException;

/**
 * Class which opens a LocalizationFile for writing to
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 23, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationFileOutputStream extends LockingFileOutputStream {

    private LocalizationFile file;

    /**
     * @param file
     * @param isAppending
     * @throws FileNotFoundException
     * @throws IOException
     * @throws LocalizationException
     */
    LocalizationFileOutputStream(LocalizationFile file, boolean isAppending)
            throws FileNotFoundException, LocalizationException {
        super(file.getFile(false), isAppending);
        this.file = file;
    }

    /**
     * Closes input stream for the {@link LocalizationFile} and calls
     * {@link LocalizationFile#save()} on the file to ensure contents are
     * persisted. Calling {@link #close()} does not trigger a save
     * 
     * @param save
     * @throws IOException
     */
    public void closeAndSave() throws IOException,
            LocalizationOpFailedException {
        try {
            closeWithoutUnlocking();
            file.save();
        } finally {
            // Make sure we unlock the file
            unlock();
        }
    }
}
