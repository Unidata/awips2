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
package com.raytheon.uf.viz.d2d.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;

import com.raytheon.uf.viz.core.IDisplayPaneContainer;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.d2d.core.ImageCombiner;
import com.raytheon.uf.viz.d2d.core.ImageCombiner.IImageCombinerListener;
import com.raytheon.viz.ui.dialogs.ImagingDialog;

/**
 * D2D extension of the {@link ImagingDialog} that adds an option for image
 * combination.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- ----------------------------------------
 * Sep 13, 2016  3241     bsteffen  Initial Creation.
 * Aug 25, 2017  6401     bsteffen  Call super in disposed.
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class D2DImagingDialog extends ImagingDialog {

    private Button combineNextImage;

    private IImageCombinerListener combineNextImageListener = new IImageCombinerListener() {
        @Override
        public void preferenceChanged(boolean newPref) {
            combineNextImage.setSelection(newPref);
        }
    };

    public D2DImagingDialog(Shell parentShell,
            AbstractVizResource<?, ?> rscToEdit) {
        super(parentShell, rscToEdit);
    }

    public D2DImagingDialog(Shell parentShell,
            IDisplayPaneContainer initialEditor) {
        super(parentShell, initialEditor);
    }

    @Override
    protected void addCustomControls(Composite bodyComp,
            Composite checkBoxComp) {
        combineNextImage = new Button(checkBoxComp, SWT.CHECK);
        combineNextImage.setText("Combine Next Image Load");
        GridData gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
        combineNextImage.setLayoutData(gd);
        combineNextImage.setSelection(ImageCombiner.isCombineImages());
        combineNextImage.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
                // Lets call our command
                IHandlerService handlerService = PlatformUI.getWorkbench()
                        .getActiveWorkbenchWindow()
                        .getService(IHandlerService.class);
                try {
                    handlerService.executeCommand(
                            "com.raytheon.uf.viz.d2d.ui.actions.imageCombination",
                            null);
                } catch (Exception ex) {
                    // Eat exception
                }
                combineNextImage.setSelection(ImageCombiner.isCombineImages());
            }
        });

        ImageCombiner.addListener(combineNextImageListener);
    }

    @Override
    protected void disposed() {
        super.disposed();
        ImageCombiner.removeListener(combineNextImageListener);
    }

}
