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
package com.raytheon.uf.viz.d2d.ui.time.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.ShellAdapter;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.d2d.core.time.D2DTimeMatcher;
import com.raytheon.uf.viz.d2d.core.time.TimeMatchingConfiguration;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * Abstract class for time matching dialogs
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2010            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public abstract class AbstractTimeMatchingDialog extends CaveSWTDialog {

    protected TimeMatchingConfiguration config;

    protected LoadProperties loadProperties;

    protected D2DTimeMatcher timeMatcher;

    protected DataTime[] availableTimes;

    protected IDescriptor descriptor;

    protected AbstractTimeMatchingDialog(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE | SWT.MODELESS);
    }

    public abstract void init() throws VizException;

    @Override
    protected void initializeComponents(Shell shell) {
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(2, true));

        Button btn = new Button(buttonComp, SWT.PUSH);
        btn.setText("OK");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 100;
        btn.setLayoutData(gd);
        btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                setSelectedDataTimes();
                getShell().dispose();
            }
        });

        btn = new Button(buttonComp, SWT.PUSH);
        btn.setText("Cancel");
        gd = new GridData(SWT.CENTER, SWT.CENTER, true, true);
        gd.widthHint = 100;
        btn.setLayoutData(gd);
        btn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                config.setCancel(true);
                getShell().dispose();
            }
        });
        shell.addShellListener(new ShellAdapter() {

            @Override
            public void shellClosed(ShellEvent e) {
                config.setCancel(true);
            }
        });
    }

    /**
     * 
     */
    protected void setSelectedDataTimes() {
        // Method to use when OK was pressed and datatimes should be selected
    }

    @Override
    protected void disposed() {
        setReturnValue(config);
    }

    public TimeMatchingConfiguration getConfig() {
        return config;
    }

    public void setLoadProperties(LoadProperties loadProperties) {
        this.loadProperties = loadProperties;
    }

    public void setTimeMatcher(D2DTimeMatcher timeMatcher) {
        this.timeMatcher = timeMatcher;
    }

    public void setResourceData(DataTime[] availableTimes) {
        this.availableTimes = availableTimes;
    }

    public void setDescriptor(IDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    protected void refreshDisplay() {
        if (descriptor.getRenderableDisplay() != null) {
            descriptor.getRenderableDisplay().refresh();
        }
    }

}
