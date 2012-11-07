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
package com.raytheon.uf.viz.monitor.trendplot;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Widget;

import com.raytheon.uf.viz.monitor.data.ObMultiHrsReports;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Trend Dialog
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 2009-12-02			   vkorolev    Initial creation.
 * 2010-01-21	4268	   vkorolev	   Fixed Trend Plot
 * 2012-10-15   1229       vkorolev    Changes for non-blocking TrendPlotDlg
 * </pre>
 * 
 * @author vkorolev
 * @version 1.0
 */
public class TrendPlotDlg extends CaveSWTDialog {
    private String selectedZone;

    private String station;

    private ArrayList<String> product;

    private String dataName;

    private ObMultiHrsReports obData;

    public Date curdate;

    public String var;

    public TrendPlotDlg(Shell parent, String selectedZone, String station,
            ArrayList<String> product, String dataName) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);
        setText(getTrendPlotName(product) + " Trend Plot for " + station + "#"
                + dataName);

        this.selectedZone = selectedZone;
        this.product = product;
        this.dataName = dataName;
        this.station = station;
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 0;
        mainLayout.marginHeight = 0;
        mainLayout.verticalSpacing = 0;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        setReturnValue(product);
        // Initialize all layouts
        Iterator<String> prodVar = product.iterator();
        while (prodVar.hasNext()) {
            String var = prodVar.next();
            new TrendPlotCanvas(shell, selectedZone, station, var, dataName,
                    obData);
        }
        addCloseBtn();
    }

    private void addCloseBtn() {
        Composite c = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        c.setLayout(gl);
        c.setLayoutData(gd);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Button closeBtn = new Button(c, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    public Widget getCurrentShell() {
        return shell;
    }

    public void setData(ObMultiHrsReports obData) {
        this.obData = obData;
    }

    private String getTrendPlotName(ArrayList<String> prod) {
        String varName = null;
        String name = (String) prod.get(0);
        int stInd = name.indexOf("_");
        if (prod.size() > 1) {
            varName = name.substring(0, stInd);
            if (varName.equals("SCA")) {
                varName = "Small Craft Advisory";
            } else if (varName.equals("GALE")) {
                varName = "Gale Warning";
            } else if (varName.equals("STORM")) {
                varName = "Storm Warning";
            } else if (varName.equals("HURRICANE")) {
                varName = "Hurricane Force Wind Warning";
            } else if (varName.equals("BLIZ")) {
                varName = "Blizzard Warning";
            } else if (varName.equals("FRZ")) {
                varName = "Frizing Precipitation";
            } else if (varName.equals("HSW")) {
                varName = "Heavy Snow Warning";
            }
        } else {
            varName = name.substring(stInd + 1);
        }

        return varName;
    }
}
