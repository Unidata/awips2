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

package com.raytheon.edex.util;

import java.io.Serializable;

/**
 * <code>FileMsg</code> TODO (document class)
 * 
 * @author F. Griffith
 */
public class FileMsg implements Serializable {
    private static final long serialVersionUID = 1L;

    private String fileName;

    private byte[] fileBytes;

    /**
     * 
     */
    public FileMsg() {
    }

    /**
     * @return Returns the file name.
     */
    public String getFileName() {
        return fileName;
    }

    /**
     * @param fileName
     *            The file name to set.
     */
    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    /**
     * @return Returns the file as bytes.
     */
    public byte[] getFileBytes() {
        return fileBytes;
    }

    /**
     * @param fileBytes
     *            The file bytes to set.
     */
    public void setFileBytes(byte[] fileBytes) {
        this.fileBytes = fileBytes;
    }
}
