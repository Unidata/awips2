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

import java.io.File;

import javax.xml.bind.JAXB;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.LocalizationContext;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationLevel;
import com.raytheon.uf.common.localization.LocalizationContext.LocalizationType;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.monitor.data.CommonConfig;
import com.raytheon.uf.common.monitor.data.CommonTableConfig.ObsHistType;
import com.raytheon.uf.viz.monitor.data.TableData;
import com.raytheon.uf.viz.monitor.xml.HistConfigXML;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Observation History table dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * Aug 6, 2010  6877      skorolev     Rewrote with using CaveSWTDialog
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ObsHistTableDlg extends CaveSWTDialog {

    private final CommonConfig.AppName appName;

    private final String stationId;

    private HistConfigXML visColsXML;

    private ObsHistTableComp obsHistTable;

    private final ObsHistType obsType;

    private final TableData tableData;

    private final double lat;

    private final double lon;

    protected ObsHistTableDlg(Shell parent, TableData tableData,
            String stationID, double lat, double lon,
            CommonConfig.AppName appName, ObsHistType obsType) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE);
        setText(appName.name() + ":24-Hour Observation History Table");
        this.tableData = tableData;
        this.appName = appName;
        this.stationId = stationID;
        this.lat = lat;
        this.lon = lon;
        this.obsType = obsType;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialog#initializeComponents(org.eclipse
     * .swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        visColsXML = getHistConfigFile();
        createTopControls();
        createObsHistTable();
    }

    private void createObsHistTable() {
        obsHistTable = new ObsHistTableComp(shell, tableData, appName, obsType);
        if (visColsXML != null) {
            if (obsType.equals(ObsHistType.METAR)) {
                obsHistTable.showHideTableColumns(visColsXML.getMetar());
            } else {
                obsHistTable.showHideTableColumns(visColsXML.getMaritime());
            }
        }
        obsHistTable.pack();
    }

    private void createTopControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(4, false));
        controlComp.setLayoutData(gd);

        gd = new GridData(SWT.DEFAULT, SWT.CENTER, false, true);
        Label stnLbl = new Label(controlComp, SWT.NONE);
        stnLbl.setText("Station ID: " + stationId);
        stnLbl.setLayoutData(gd);

        gd = new GridData(SWT.FILL, SWT.CENTER, true, true);
        Label latLonLbl = new Label(controlComp, SWT.NONE);
        latLonLbl.setText(getFormattedLatLon());
        latLonLbl.setLayoutData(gd);

        Button configureBtn = new Button(controlComp, SWT.PUSH);
        configureBtn.setText("Configure");
        configureBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                configAction();
            }
        });

        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    private void configAction() {
        ObsHistConfigDlg obsHistConfigDlg = new ObsHistConfigDlg(shell,
                appName, obsHistTable, obsType);
        obsHistConfigDlg.open();
    }

    private String getFormattedLatLon() {
        String format = "(%.2f, %.2f)";
        return String.format(format, lat, lon);
    }

    private HistConfigXML getHistConfigFile() {
        // Open user file if exist.
        visColsXML = null;
        IPathManager pm = PathManagerFactory.getPathManager();
        LocalizationContext context = pm.getContext(
                LocalizationType.COMMON_STATIC, LocalizationLevel.USER);
        String visFileName = getHistPath() + "visFieldsFile.xml";
        File filepath = pm.getFile(context, visFileName).getAbsoluteFile();
        if (filepath.exists()) {
            try {
                visColsXML = JAXB.unmarshal(filepath, HistConfigXML.class);
            } catch (RuntimeException e) {
                e.printStackTrace();
            }
        }
        return visColsXML;
    }

    private String getHistPath() {
        String fs = String.valueOf(File.separatorChar);
        StringBuilder sb = new StringBuilder();
        sb.append(appName).append(fs);
        sb.append("history").append(fs);
        return sb.toString();
    }
}
