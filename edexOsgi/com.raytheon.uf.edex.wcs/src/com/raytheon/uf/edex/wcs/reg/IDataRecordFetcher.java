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
 *
 **/
package com.raytheon.uf.edex.wcs.reg;

import com.raytheon.uf.common.spatial.reprojection.ReferencedDataRecord;
import com.raytheon.uf.edex.wcs.WcsException;

/**
 * Data Record Fetcher for WCS
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 3, 2013            bclement     Initial creation
 * Aug 18, 2013 #2097      dhladky     interface standards
 * 
 * </pre>
 * 
 * @author bclement
 * @version 1.0
 */
public interface IDataRecordFetcher {

    /**
     * @param cache
     *            true if the record will likely be retrieved again
     * @return
     * @throws Exception
     */
    public ReferencedDataRecord get(boolean cache) throws WcsException;

}
