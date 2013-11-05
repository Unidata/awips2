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
package com.raytheon.uf.edex.wcs.format;

import java.io.File;
import java.io.InputStream;

import com.raytheon.uf.edex.wcs.reg.Coverage;

/**
 * 
 * @author bclement
 * @version 1.0
 */
public interface IWcsDataFormatter {

    /**
     * 
     * @return
     */
    public String getIdentifier();

    /**
     * get coverage identifier
     * @param format
     * @return
     */
    public boolean matchesFormat(String format);

    /**
     * Format coverage
     * @param coverage
     * @return
     * @throws Exception
     */
    public InputStream format(Coverage coverage) throws Exception;

    /**
     * Store coverage
     * @param coverage
     * @return
     * @throws Exception
     */
    public File store(Coverage coverage) throws Exception;

}
