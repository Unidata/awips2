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

import java.util.List;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

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
 * Aug 12, 2011            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class FailedSiteDlg extends CaveJFACEDialog {

    private String ownSite;

    private List<String> knownSites;

    private Text failedSiteText;

    private Label message;

    private String failedSite;

    /**
     * @param parentShell
     * @param ownSite
     * @param knownSites
     */
    protected FailedSiteDlg(Shell parentShell, String ownSite,
            List<String> knownSites) {
        super(parentShell);
        this.ownSite = ownSite;
        this.knownSites = knownSites;
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
        newShell.setText("Failed Site");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveJFACEDialog#createDialogArea(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite top = (Composite) super.createDialogArea(parent);

        Label label = new Label(top, SWT.NONE);
        label.setText("Enter the 3-letter site id for the failed site, then press OK");
        GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        label.setLayoutData(layoutData);

        failedSiteText = new Text(top, SWT.BORDER);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        failedSiteText.setLayoutData(layoutData);
        failedSiteText.setText("failed site");
        failedSiteText.selectAll();
        failedSiteText.addVerifyListener(new VerifyListener() {

            @Override
            public void verifyText(VerifyEvent e) {
                e.text = e.text.toUpperCase();

                String newText = ((Text) e.widget).getText();
                newText = newText.substring(0, e.start) + e.text
                        + newText.substring(e.end);

                String msg = "";
                if (newText.equals(ownSite)) {
                    msg = "You cannot select your own site";
                } else if (!knownSites.contains(newText)) {
                    msg = "\"" + newText + "\" is not a known site";
                }

                message.setText(msg);
                FailedSiteDlg.this.getButton(IDialogConstants.OK_ID)
                        .setEnabled(msg.isEmpty());
            }
        });

        message = new Label(top, SWT.NONE);
        layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        message.setLayoutData(layoutData);

        return top;
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.ui.dialogs.CaveJFACEDialog#okPressed()
     */
    @Override
    protected void okPressed() {
        this.failedSite = this.failedSiteText.getText();
        super.okPressed();
    }

    public String getFailedSite() {
        return failedSite;
    }
}
