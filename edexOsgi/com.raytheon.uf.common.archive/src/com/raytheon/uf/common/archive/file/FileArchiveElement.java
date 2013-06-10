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
package com.raytheon.uf.common.archive.file;

import java.io.File;

import com.raytheon.uf.common.archive.IArchiveElement;
import com.raytheon.uf.common.archive.exception.ArchiveException;

/**
 * Archive element for a file or directory.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 13, 2013 1966       rferrel     Initial creation
 * 
 * </pre>
 * 
 * @author rferrel
 * @version 1.0
 */

public class FileArchiveElement implements IArchiveElement {

    private String name;

    public FileArchiveElement(String name) {
        this.name = name;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.archive.IArchiveElement#purge()
     */
    @Override
    public boolean purge() throws ArchiveException {
        File file = new File(name);

        boolean state = false;
        try {
            if (!file.exists()) {
                state = true;
            } else {
                if (file.isDirectory()) {
                    deleteContents(file);
                }
                state = file.delete();
            }
        } catch (SecurityException ex) {
            throw new ArchiveException(ex.getMessage());
        }
        return state;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.common.archive.IArchiveElement#getName()
     */
    @Override
    public String getName() {
        return name;
    }

    /**
     * Delete all files and sub-directories.
     * 
     * @param directory
     */
    private void deleteContents(File directory) {
        File[] files = directory.listFiles();
        for (File file : files) {
            if (file.isDirectory()) {
                deleteContents(file);
            }
            file.delete();
        }
    }
}
