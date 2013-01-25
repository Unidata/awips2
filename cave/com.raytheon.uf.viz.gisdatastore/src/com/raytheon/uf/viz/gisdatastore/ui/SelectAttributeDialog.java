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
package com.raytheon.uf.viz.gisdatastore.ui;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.gisdatastore.rsc.DataStoreResource;
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
 * Dec 13, 2012            randerso     Initial creation
 * 
 * </pre>
 * 
 * @author randerso
 * @version 1.0
 */

public class SelectAttributeDialog extends CaveJFACEDialog {

    private DataStoreResource rsc;

    private String originalSampleAttribute;

    public SelectAttributeDialog(Shell parentShell, DataStoreResource rsc) {
        super(parentShell);
        this.rsc = rsc;
        this.originalSampleAttribute = rsc.getSampleAttribute();
    }

    @Override
    protected void configureShell(Shell newShell) {
        super.configureShell(newShell);
        newShell.setText("Select Sample Attribute");
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);

        List attrList = new List(comp, SWT.BORDER | SWT.SINGLE | SWT.V_SCROLL
                | SWT.H_SCROLL);
        GridData layoutData = new GridData(SWT.FILL, SWT.FILL, true, true);
        attrList.setItems(rsc.getAttributeNames());
        if (attrList.getItemCount() > 20) {
            layoutData.heightHint = attrList.getItemHeight() * 20;
        }
        attrList.setLayoutData(layoutData);

        if (originalSampleAttribute != null) {
            attrList.setSelection(new String[] { originalSampleAttribute });
        }
        attrList.addSelectionListener(new SelectionAdapter() {

            @Override
            public void widgetSelected(SelectionEvent e) {
                List list = (List) e.widget;
                String attr = list.getSelection()[0];
                rsc.setSampleAttribute(attr);
            }

            @Override
            public void widgetDefaultSelected(SelectionEvent e) {
                List list = (List) e.widget;
                String attr = list.getSelection()[0];
                rsc.setSampleAttribute(attr);
                okPressed();
            }
        });

        return comp;
    }

    @Override
    protected void cancelPressed() {
        rsc.setSampleAttribute(originalSampleAttribute);
        super.cancelPressed();
    }

}
