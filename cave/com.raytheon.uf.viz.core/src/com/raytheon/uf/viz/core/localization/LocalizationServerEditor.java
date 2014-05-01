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
package com.raytheon.uf.viz.core.localization;

import org.eclipse.jface.preference.StringFieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

import com.raytheon.uf.viz.core.comm.ConnectivityManager;
import com.raytheon.uf.viz.core.comm.IConnectivityCallback;
import com.raytheon.uf.viz.core.comm.ConnectivityManager.ConnectivityResult;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 13, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class LocalizationServerEditor extends StringFieldEditor implements
        IConnectivityCallback {

    protected boolean good = true;

    protected boolean httpCheck;

    public LocalizationServerEditor() {
        super();
        init();
    }

    public LocalizationServerEditor(String name, String labelText,
            Composite parent, boolean httpCheck) {
        super(name, labelText, parent);
        this.httpCheck = httpCheck;
        init();
    }

    public LocalizationServerEditor(String name, String labelText, int width,
            Composite parent) {
        super(name, labelText, width, parent);
        init();
    }

    public LocalizationServerEditor(String name, String labelText, int width,
            int strategy, Composite parent) {
        super(name, labelText, width, strategy, parent);
        init();
    }

    private void init() {
        this.setErrorMessage("Unable to connect to localization server");
        setValidateStrategy(VALIDATE_ON_FOCUS_LOST);
    }

    @Override
    protected boolean doCheckState() {
        if (httpCheck) {
            ConnectivityManager.checkHttpServer(this.getTextControl().getText(),
                    this);
        } else {
            ConnectivityManager.checkAlertService(this.getTextControl().getText(),
                    this);
        }
        if (!good) {
            this.getTextControl().setBackground(
                    Display.getDefault().getSystemColor(SWT.COLOR_RED));
            this.showErrorMessage();
        } else {
            this.getTextControl().setBackground(
                    Display.getDefault().getSystemColor(SWT.COLOR_WHITE));
            this.clearMessage();
        }

        return good;
    }

    @Override
    public void connectionChecked(ConnectivityResult results) {
        good = results.hasConnectivity;
    }

}
