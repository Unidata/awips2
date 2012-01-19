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
import java.io.FileFilter;

/**
 * Extension of the {@link FileFilter} class for any file type
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 06/14/06                garmendariz Initial check-in
 * 
 * </pre>
 *
 * @author garmendariz
 * @version 1.0
 */
public class Filter implements FileFilter {

    /** A file type (jar, tiff, xml) - any case */
    private String fileType;
    
    
	/**
	 * Implementation of the {@link FileFilter#accept(File)} method to retrieve only files with the
     * set file type.
	 */
	public boolean accept(File pathname) {
		if (pathname.isDirectory()
				|| pathname.getName().toUpperCase().endsWith("." + fileType.toUpperCase())) {
			return true;
		} else {
			return false;
		}
	}
    
    /**
     * Retrieve the file extension
     * @return  A file extension
     */
    public String getFileType()
    {
        return fileType;
    }
    
    
    /**
     * Set the file type
     * @param fileType  A file extension
     */
    public void setFileType(String fileType)
    {
        this.fileType = fileType;
    }

}
