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
package com.raytheon.viz.gfe.dialogs.sbu;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.site.requests.GetActiveSitesRequest;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.requests.ThriftClient;
import com.raytheon.viz.gfe.core.internal.IFPClient;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bphillip     Initial creation
 * 
 * </pre>
 * 
 * @author bphillip
 * @version 1.0
 */

public class ChooseDomainDlg extends CaveJFACEDialog {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(IFPClient.class);

    private String[] activeSites;

    private String selectedSite;

    /**
     * @param parentShell
     */
    protected ChooseDomainDlg(Shell parentShell) {
        super(parentShell);
        GetActiveSitesRequest request = new GetActiveSitesRequest();
        try {
            Object obj = ThriftClient.sendRequest(request);
            if (obj instanceof String[]) {
                activeSites = (String[]) obj;
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "getActiveSites received " + obj);
            }
        } catch (VizException e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error processing get active sites request", e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets
     * .Shell)
     */
    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Choose Domain");
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.
     * eclipse.swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);
        GridData gd = new GridData();
        gd.horizontalAlignment = GridData.CENTER;
        top.setLayoutData(gd);

        Label label = new Label(top, SWT.CENTER);
        label.setText("Available Domains:");

        for (int i = 0; i < activeSites.length; i++) {
            final Button button = new Button(top, SWT.RADIO);
            button.setText(activeSites[i]);
            button.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                    selectedSite = button.getText();
                }
            });
            if (i == 0){
                button.setSelection(true);
                selectedSite = button.getText();
            }
        }

        return top;
    }

    /**
     * @return the selectedSite
     */
    public String getSelectedSite() {
        return selectedSite;
    }

}
