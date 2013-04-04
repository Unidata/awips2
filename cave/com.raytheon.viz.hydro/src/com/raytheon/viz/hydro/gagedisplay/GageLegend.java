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
package com.raytheon.viz.hydro.gagedisplay;

import org.eclipse.core.runtime.Status;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.core.CorePlugin;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * The Gage Legend Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Jul 3, 2008				mpduff	Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class GageLegend extends CaveSWTDialog {

    /**
     * Protected constructor.
     * 
     * @param parentShell
     *            Shell of the opener
     */
    protected GageLegend(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM, CAVE.DO_NOT_BLOCK);
        setText("Gage Legend");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(final Shell shell) {
        setReturnValue(false);

        GridData gd = new GridData(150, 350);
        GridLayout layout = new GridLayout(1, false);
        Browser browser;
        try {
            browser = new Browser(shell, SWT.NONE);
        } catch (SWTError e) {
            CorePlugin
                    .getDefault()
                    .getLog()
                    .log(new Status(Status.ERROR, CorePlugin.PLUGIN_NAME,
                            "Could not instantiate Browser", e));
            return;
        }
        browser.setLayout(layout);
        browser.setLayoutData(gd);
        browser.setText(getHtml());

        /* Close button */
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(layout);
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonComp.setLayoutData(gd);

        gd = new GridData(90, SWT.DEFAULT);
        Button closeBtn = new Button(buttonComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                close();
            }
        });
    }

    /**
     * Get the HTML that generates the legend.
     * 
     * @return The HTML string
     */
    private String getHtml() {
        StringBuilder sb = new StringBuilder("<html><head>");
        sb.append("<title>Gage Color Legend</title>");
        sb.append("</head><body>");
        sb.append("<table align=\"center\" width=\"90\" cellpadding=\"2\" cellspacing=\"0\" border=\"2\">");
        sb.append("<tr>");
        sb.append(" <th align=\"center\" bgcolor=\"#E2E3FE\">Gage Color Legend</td>");
        sb.append("</tr><tr>");
        sb.append(" <td align=\"center\" bgcolor=\"#FF0000\">Above Flood Stage</td>");
        sb.append("</tr><tr>");
        sb.append(" <td align=\"center\" bgcolor=\"#FFFF00\">Above Action Stage</td>");
        sb.append("</tr><tr>");
        sb.append(" <td align=\"center\" bgcolor=\"#00FF00\">Below Action Stage</td>");
        sb.append("</tr><tr>");
        sb.append(" <td align=\"center\" bgcolor=\"#C0C0C0\">Missing Data</td>");
        sb.append("</tr><tr>");
        sb.append(" <td align=\"center\" bgcolor=\"#228B22\">Missing Stage Data</td>");
        sb.append("</tr></table></html>");

        return sb.toString();
    }
}
