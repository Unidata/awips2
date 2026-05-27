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
package com.raytheon.viz.gfe.dialogs;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.EditorType;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisMode;
import com.raytheon.viz.gfe.core.parm.ParmDisplayAttributes.VisualizationType;
import com.raytheon.viz.gfe.rsc.GFEResource;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * TODO Add Description DisplayAttributesDialog.java Jun 11, 2008
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 	Jun 11, 2008					Eric Babin Initial Creation
 * Oct 22, 2012 1287       rferrel     Make dialog application modal
 *                                      and non-blocking.
 * 
 * </pre>
 * 
 * @author ebabin
 * @version 1.0
 */

public class DisplayAttributesDialog extends CaveJFACEDialog {

    private static final int APPLY_ID = IDialogConstants.CLIENT_ID + 1;

    private Composite top;

    private Set<VisualizationType> imageVisualTypes;

    private Set<VisualizationType> graphicVisualTypes;

    private Set<VisualizationType> startGraphicTypes;

    private Set<VisualizationType> startImageTypes;

    private EditorType editorType;

    private VisMode visMode;

    private ArrayList<Button> imageButtons;

    private ArrayList<Button> graphicButtons;

    private Parm parm;

    public DisplayAttributesDialog(Shell parent, GFEResource rsc) {
        super(parent);
        this.setShellStyle(SWT.TITLE | SWT.APPLICATION_MODAL | SWT.CLOSE);

        this.parm = rsc.getParm();

        ParmDisplayAttributes da = parm.getDisplayAttributes();
        this.editorType = da.getEditorType();
        this.visMode = da.getVisMode();

        // Get the available visual types.
        this.imageVisualTypes = da.getAvailableVisualizationType(editorType,
                VisMode.IMAGE);
        this.graphicVisualTypes = da.getAvailableVisualizationType(editorType,
                VisMode.GRAPHIC);

        startImageTypes = null;
        startGraphicTypes = null;
        setBlockOnOpen(false);
    }

    @Override
    protected Control createDialogArea(Composite parent) {
        top = (Composite) super.createDialogArea(parent);

        GridData data = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout layout = (GridLayout) top.getLayout();
        layout.numColumns = 2;
        top.setLayoutData(data);

        /* Top label */
        String subTitle = "";

        if (editorType.equals(EditorType.SPATIAL)) {
            subTitle = "Spatial Editor";
        } else if (editorType.equals(EditorType.TEMPORAL)) {
            subTitle = "Temporal Editor";
        }

        if (parm.getDisplayAttributes().getVisMode() == VisMode.IMAGE) {
            subTitle += ", Image Mode";
        } else if (parm.getDisplayAttributes().getVisMode() == VisMode.GRAPHIC) {
            subTitle += ", Graphic Editor";
        }

        Label label = new Label(top, SWT.CENTER);
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        label.setLayoutData(gd);
        label.setText(subTitle);

        // Create the check buttons.
        // Create the button box for Image Visual Types
        if (visMode.equals(VisMode.IMAGE)) {
            createImageVisualTypeComponent(top);
        }

        // Create the button box for Graphic Visual Types
        createGraphicVisualTypeComponent(top);

        return top;
    }

    @Override
    protected Control createContents(Composite parent) {
        Control contents = super.createContents(parent);

        getShell().pack(true);
        getShell().setLocation(getInitialLocation(getShell().getSize()));

        return contents;
    }

    private void createImageVisualTypeComponent(Composite parent) {
        if (imageVisualTypes != null && imageVisualTypes.size() > 0) {
            // Create the button box for Image Visual Types.
            Group imageComp = new Group(parent, SWT.BORDER);
            GridLayout layout = new GridLayout(1, false);
            imageComp.setLayout(layout);
            GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
            imageComp.setLayoutData(gd);
            imageComp.setText("Image Visuals");

            // Get the current visual types.
            Set<VisualizationType> currentVisualTypes = parm
                    .getDisplayAttributes().getVisualizationType(editorType,
                            visMode);
            startImageTypes = currentVisualTypes;

            // Create the check buttons for each possible visual type
            imageButtons = new ArrayList<Button>();
            for (VisualizationType visType : imageVisualTypes) {
                Button btn = new Button(imageComp, SWT.CHECK);
                btn.setText(visType.getVizualizationType());
                btn.setData(visType);

                // Determine which available visuals are current visuals
                btn.setSelection(currentVisualTypes.contains(visType));

                imageButtons.add(btn);
            }
        }

    }

    private void createGraphicVisualTypeComponent(Composite parent) {
        // Create the button box for Graphic Visual Types. Return the object.
        Group graphicGroup = new Group(parent, SWT.BORDER);
        GridLayout layout = new GridLayout(1, false);
        graphicGroup.setLayout(layout);
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        graphicGroup.setLayoutData(gd);
        graphicGroup.setText("Graphic Visuals");

        // Get the current visual types.
        Set<VisualizationType> currentVisualTypes = parm.getDisplayAttributes()
                .getVisualizationType(editorType, visMode);
        startGraphicTypes = currentVisualTypes;

        // Create the check buttons for each possible visual type
        graphicButtons = new ArrayList<Button>();
        for (VisualizationType visType : graphicVisualTypes) {
            Button btn = new Button(graphicGroup, SWT.CHECK);
            btn.setText(visType.getVizualizationType());
            btn.setData(visType);

            // Determine which available visuals are current visuals
            btn.setSelection(currentVisualTypes.contains(visType));

            graphicButtons.add(btn);
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
    protected void configureShell(Shell shell) {
        super.configureShell(shell);
        shell.setText("Display Attributes Dialog");
    }

    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        super.createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, false);
        super.createButton(parent, APPLY_ID, "Apply", false);
        super.createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

    }

    @Override
    protected void buttonPressed(int buttonId) {
        if (buttonId == IDialogConstants.OK_ID) {
            if (apply()) {
                super.buttonPressed(buttonId);
            }

        } else if (buttonId == APPLY_ID) {
            apply();

        } else if (buttonId == IDialogConstants.CANCEL_ID) {
            cancel();
            super.buttonPressed(buttonId);
        }
    }

    private boolean apply() {
        // Send out notifications for changes.

        // Calculate the change to the parms image visual types
        Set<VisualizationType> imageTypes = new HashSet<VisualizationType>();
        if (imageButtons != null) {
            for (Button btn : imageButtons) {
                if (btn.getSelection()) {
                    imageTypes.add((VisualizationType) btn.getData());
                }
            }
        }

        // Calculate the change to the parms graphic visual types
        Set<VisualizationType> graphicTypes = new HashSet<VisualizationType>();
        for (Button btn : graphicButtons) {
            if (btn.getSelection()) {
                graphicTypes.add((VisualizationType) btn.getData());
            }
        }

        // Ensure exactly 1 value is set for image mode
        if (visMode.equals(VisMode.IMAGE) && imageTypes.size() != 1) {
            MessageDialog.openError(getShell(), "Image Attributes Error",
                    "You must select exactly one image type");
            return false;
        }

        // Image is okay, check for at least 1 graphic type
        if (visMode.equals(VisMode.GRAPHIC) && graphicTypes.size() == 0) {
            MessageDialog.openError(getShell(), "Graphic Attributes Error",
                    "You must select at least one graphic type");
            return false;
        }

        // set the types (only IMAGE for image mode, GRAPHIC for graphic mode)
        if (visMode.equals(VisMode.IMAGE)) {
            imageTypes.addAll(graphicTypes);
            parm.getDisplayAttributes().setVisualizationType(editorType,
                    VisMode.IMAGE, imageTypes);

        } else {
            parm.getDisplayAttributes().setVisualizationType(editorType,
                    VisMode.GRAPHIC, graphicTypes);
        }
        // LogStream.logUse("OK img=", imageTypesStr, "gph=", graphicTypesStr)
        return true;
    }

    private void cancel() {
        // restore initial settings
        if (visMode.equals(VisMode.IMAGE)) {
            Set<VisualizationType> imageTypes = new HashSet<VisualizationType>();
            imageTypes.addAll(startImageTypes);
            imageTypes.addAll(startGraphicTypes);

            parm.getDisplayAttributes().setVisualizationType(editorType,
                    VisMode.IMAGE, imageTypes);

        } else {
            parm.getDisplayAttributes().setVisualizationType(editorType,
                    VisMode.GRAPHIC, startGraphicTypes);
        }
    }
}
