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
package com.raytheon.uf.edex.core.modes;

import java.io.File;
import java.io.FilenameFilter;

/**
 * An edex mode filter finds all spring XML files. For the case when no EDEX run
 * configuration is specified.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 27, 2010            njensen     Initial creation
 * Dec 05, 2013 2566       bgonzale    Migrated to edex.core.modes package.
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

public class DefaultEdexMode implements FilenameFilter {

    /*
     * (non-Javadoc)
     * 
     * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
     */
    @Override
    public boolean accept(File dir, String name) {
        return name.contains(EDEXModesUtil.RES_SPRING)
                && name.endsWith(EDEXModesUtil.XML);
    }

}
