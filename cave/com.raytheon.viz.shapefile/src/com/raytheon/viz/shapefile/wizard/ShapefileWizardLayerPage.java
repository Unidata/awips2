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

package com.raytheon.viz.shapefile.wizard;

import java.io.File;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.shapefile.rsc.ShapefileUtil;

/**
 * Shapefile Wizard: select the layer
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
public class ShapefileWizardLayerPage extends WizardPage {

    private List theResourceList;

    public ShapefileWizardLayerPage() {
        super("Shapefile Wizard: Select Label Attribute");
        this.setTitle("Shapefile Wizard: Select Label Attribute");
        this.setDescription("Select the labelling attribute");
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets
     * .Composite)
     */
    public void createControl(Composite aParent) {
        setPageComplete(false);

        Composite composite = new Composite(aParent, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        composite.setLayout(gl);

        Label label = new Label(composite, SWT.NONE);
        label.setText("&Attributes:");
        theResourceList = new List(composite, SWT.V_SCROLL | SWT.BORDER
                | SWT.SINGLE);
        theResourceList.setLayoutData(new GridData(GridData.FILL_BOTH));

        composite.addPaintListener(new PaintListener() {

            public void paintControl(PaintEvent anE) {
                if (theResourceList.getItemCount() == 0) {
                    try {
                        String shpName = ((ShapefileWizard) getWizard())
                                .getShpFile();
                        File f = new File(shpName);
                        String[] attribs = ShapefileUtil
                                .getAttributes(f, false);
                        theResourceList.setItems(attribs);
                    } catch (VizException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }

        });

        theResourceList.addSelectionListener(new SelectionAdapter() {

            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse
             * .swt.events.SelectionEvent)
             */
            @Override
            public void widgetSelected(SelectionEvent anE) {
                int idx = theResourceList.getSelectionIndex();
                ((ShapefileWizard) getWizard())
                        .setLabelAttribute(theResourceList.getItem(idx));

                setPageComplete(true);

            }

        });

        setControl(composite);

    }

}
