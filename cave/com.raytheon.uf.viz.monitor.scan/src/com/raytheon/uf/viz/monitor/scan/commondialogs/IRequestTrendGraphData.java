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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import java.util.Date;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;

/**
 * 
 * Interface used for requesting trend graph data.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public interface IRequestTrendGraphData
{
    /**
     * Request trend graph data.
     * @param type SCAN table identifier.
     * @param field Field/Attribute name.
     * @param ident Ident (table row identifier).
     * @return Linked map of trend graph data and data point color.
     */
    TrendGraphData requestTrendGraphData(ScanTables type, String field, String ident);
        
    /**
     * Get the current date off of the table.
     * @return The current date off of the table.
     */
    Date getCurrentDate();

    /**
     * Is the cell still valid?  (In the table)
     * @param ident the ident of the cell
     * @return true if the cell is still valid
     */
    boolean cellValid(String ident);
}
