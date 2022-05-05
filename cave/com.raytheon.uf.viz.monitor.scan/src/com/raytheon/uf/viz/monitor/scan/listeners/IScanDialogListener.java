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
package com.raytheon.uf.viz.monitor.scan.listeners;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.tables.AbstractTableDlg;

/**
 *
 * IScanDialogListener
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 10, 2009            dhladky     Initial creation
 * Aug  1, 2018 6567       tgurney     launchDialog return the dialog
 * </pre>
 *
 * @author dhladky
 *
 */

public interface IScanDialogListener {

    /**
     * Scan specific dialog control over monitor
     *
     */
    public void paintScan();

    public void recenter(String ident, ScanTables type, String icao);

    public void updateDrawingConfig();

    public TrendGraphData getGraphData(ScanTables type, String icao,
            String field, String ident);

    public AbstractTableDlg launchDialog(Shell shell, String icao,
            ScanTables table);

    public void launchTrendGraphs(ScanTables table, String icao, String id);

    public void configurationLoaded(ScanTables table, String icao);

    public void thresholdUpdated(ScanTables table, String icao, String attr);
}