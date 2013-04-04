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
package com.raytheon.uf.viz.datadelivery.common.ui;

/**
 * Sort column properties
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 16, 2012            mpduff     Initial creation
 *
 * </pre>
 *
 * @author mpduff
 * @version 1.0	
 */

public interface ISortColumn {
    /**
     * Get the text of the column to be sorted.
     * @return The column index.
     */
    public String getSortColumnText();
    
    /**
     * Get the sort direction.
     * @return SWT.UP, SWT.DOWN, SWT.NONE, or -1 (sort both ways)
     */
    public int getSortDirection();
}
