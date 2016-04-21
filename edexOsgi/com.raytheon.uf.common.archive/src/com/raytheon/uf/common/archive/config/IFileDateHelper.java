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
package com.raytheon.uf.common.archive.config;

import java.io.File;

/**
 * Helper to get a file last modification date.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 21, 2013            bgonzale     Initial creation
 * Aug 28, 2013 2299       rferrel     Change getFileDate argument.
 * Dec 04, 2013 2603       rferrel     Changes to improve archive purging.
 * Dec 17, 2013 2603       rjpeter     Clean up imports.
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

public interface IFileDateHelper {

    /**
     * Get data associated with the file.
     * 
     * @param file
     * @return calendar
     */
    public DataSetStatus getFileDate(File file);

}
