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

/**
 * 
 */
package com.raytheon.viz.shapefile.wizard;

import java.io.File;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

/**
 * Shapefile Wizard: select the file
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 7/1/06                   chammack    Initial Creation.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ShapefileWizardFilePage extends WizardPage {

    private Text theDirectory;

    public ShapefileWizardFilePage() {
        super("Shapefile Wizard: Select File");
        this.setTitle("Shapefile Wizard: Select File");
        this.setDescription("Import a Shapefile into Viz");

        setPageComplete(false);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets
     * .Composite)
     */
    public void createControl(final Composite aParent) {

        Composite composite = new Composite(aParent, SWT.None);
        GridLayout gl = new GridLayout(3, false);
        composite.setLayout(gl);

        Label label = new Label(composite, SWT.None);
        label.setText("&Filename: ");
        theDirectory = new Text(composite, SWT.BORDER);
        theDirectory.setText("");
        theDirectory.setEditable(false);
        theDirectory.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        Button browse = new Button(composite, SWT.None);
        browse.setText("&Browse");
        browse.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent anE) {
                FileDialog selectDialog = new FileDialog(aParent.getShell(),
                        SWT.OPEN);
                selectDialog.setFilterExtensions(new String[] { "*.shp" });
                String filePath = selectDialog.open();
                if (filePath != null) {
                    File file = new File(filePath);
                    if (file.exists() && file.canRead()) {
                        theDirectory.setText(filePath);
                        ((ShapefileWizard) getWizard()).setShpFile(filePath);
                        setPageComplete(true);
                    }

                }

            }

        });

        setControl(composite);

    }

}
