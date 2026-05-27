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

import java.util.Date;
import java.util.Iterator;
import java.util.List;

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
 * 2009-12-02			   skorolev    Initial creation.
 * 2010-01-21	4268	   skorolev	   Fixed Trend Plot
 * Oct 15,2012  1229       skorolev    Changes for non-blocking TrendPlotDlg
 * Nov 11,2012  1297       skorolev    Added title parameter and cleaned code
 * </pre>
 * 
 * @author vkorolev
 * @version 1.0
 */
public class TrendPlotDlg extends CaveSWTDialog {
    private String selectedZone;

    private String station;

    private List<String> product;

    private String dataName;

    private ObMultiHrsReports obData;

    public Date curdate;

    /**
     * Constructor
     * 
     * @param parent
     * @param selectedZone
     * @param station
     * @param product
     * @param dataName
     * @param title
     */
    public TrendPlotDlg(Shell parent, String selectedZone, String station,
            List<String> product, String dataName, String title) {
        super(parent, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK
                | CAVE.INDEPENDENT_SHELL);

        setText(title);
        setReturnValue(this.getText());
        this.selectedZone = selectedZone;
        this.product = product;
        this.dataName = dataName;
        this.station = station;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#constructShellLayout()
     */
    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginWidth = 0;
        mainLayout.marginHeight = 0;
        mainLayout.verticalSpacing = 0;
        return mainLayout;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        // Initialize all layouts
        Iterator<String> prodVar = product.iterator();
        while (prodVar.hasNext()) {
            String var = prodVar.next();
            new TrendPlotCanvas(shell, selectedZone, station, var, dataName,
                    obData);
        }
        addCloseBtn();
    }

    /**
     * Adds Close button.
     */
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
                close();
            }
        });
    }

    /**
     * @return shell
     */
    public Widget getCurrentShell() {
        return shell;
    }

    /**
     * Sets data for table.
     * 
     * @param obData
     */
    public void setData(ObMultiHrsReports obData) {
        this.obData = obData;
    }
}
