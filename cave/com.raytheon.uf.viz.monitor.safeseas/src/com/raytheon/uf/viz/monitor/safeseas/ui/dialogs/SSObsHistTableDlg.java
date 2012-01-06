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
package com.raytheon.uf.viz.monitor.safeseas.ui.dialogs;

import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.data.CommonConfig.AppName;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.ui.dialogs.ObsHistTableDlg;

public class SSObsHistTableDlg extends ObsHistTableDlg {
    public SSObsHistTableDlg(Shell parent, TableData tableData,
            String stationID, double lat, double lon, AppName appName,
            ObsHistType obsType) {
        super(parent, tableData, stationID, lat, lon, appName, obsType);
        // TODO Auto-generated constructor stub
    }

    protected void configAction() {
        System.out.println("SAFESEAS Config Action");
    }
}
