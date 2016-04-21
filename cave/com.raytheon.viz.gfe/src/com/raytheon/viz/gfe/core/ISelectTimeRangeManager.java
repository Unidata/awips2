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
package com.raytheon.viz.gfe.core;

import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange;
import com.raytheon.uf.common.dataplugin.gfe.time.SelectTimeRange.Mode;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009            randerso     Initial creation
 * Aug 1, 2012   #965     dgilling     Change location of SelectTimeRange.
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public interface ISelectTimeRangeManager {

    /**
     * Get the list of defined SelectTimeRanges
     * 
     * @return the list of defined SelectTimeRanges
     */
    public String[] inventory();

    /**
     * Save a new SelectTimeRange
     * 
     * @param name
     *            the name of the new SelectTimeRange
     * @param start
     *            the starting hour
     * @param end
     *            the ending hour
     * @param mode
     *            the mode
     */
    public void save(String name, int start, int end, Mode mode);

    /**
     * Get the named SelectTimeRange
     * 
     * @param name
     * @return the named SelectTimeRange
     */
    public SelectTimeRange getRange(String name);

    /**
     * Remove the named SelectTimeRange
     * 
     * @param name
     */
    public void remove(String name);

    /**
     * Dispose of this instance
     */
    public void dispose();
}