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
package com.raytheon.uf.viz.monitor.scan.tables;

/**
 * Interface implemented to handle table actions.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 2, 2009  #3039      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public interface ITableAction {
    /**
     * Called when the column on the table is selected for sorting the data.
     * 
     * @param columnName
     *            Column name of the column that was sorted.
     */
    void sortedColumn(String columnName);

    /**
     * Center the display by the specified ident.
     * 
     * @param ident
     *            Indentifier.
     */
    void centerByIdent(String ident);

    /**
     * Center the display by the specified stormId.
     * 
     * @param ident
     *            Indentifier.
     */
    void centerByStormId(String stormId);

    /**
     * Get the trend set name.
     * 
     * @return The trend set name.
     */
    String getTrendSetName();

}
