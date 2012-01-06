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
package com.raytheon.viz.gfe.contours;

import com.raytheon.uf.common.dataplugin.gfe.grid.Grid2DFloat;

/**
 * Defines a common interface for the Contour analysis algorithm
 * classes used by the GFE tool.
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 13Mar2008    968        MW Fegan    Initial Implementation.
 * 
 * </pre>
 *
 * @author mfegan
 * @version 1.0	
 */

public interface IGfeAnalyzer {
    /**
     * Returns the results of the grid analysis computation.
     */
    public Grid2DFloat getFinalResultData();
    /**
     * Triggers the grid analysis.
     * 
     * @return results of the computation
     */
    public Grid2DFloat recomputeGrid();
}
