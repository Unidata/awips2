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
package com.raytheon.uf.viz.monitor.ui.dialogs;

/**
 * Interface used for an action callback on the Station table.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr  6, 2009            lvenable     Initial creation
 * Dec 17, 2009            zhao         added methods for trend plot and history table
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public interface IStationTableAction
{
    /**
     * Action to be taken on the Station table.
     */
    public void stationTableAction();
    
    /**
     * launch a trending plot for a specific obs variable and a specific station 
     * as specified by the row and column indices of the station table
     * @param rowIndex the row index (link to station id)
     * @param colIndex the column index (link to observation variable)
     */
    public void launchTrendPlot(int rowIndex, int colIndex);
    
    /**
     * launch a obs history table for a station as 
     * specified by the row index of the station table
     * @param rowIndex row index (link to station id)
     */
    public void launchObHistoryTable(int rowIndex);
    
    /**
     * Re-center map to a station specified by the row index of the station
     * table
     * 
     * @param rowIndex
     *            row index (link to station id)
     */
    public void zoomToStation(int rowIndex);
}
