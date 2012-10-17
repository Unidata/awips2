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

package com.raytheon.viz.ui.dialogs;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;

/**
 * Provides an interface to modify the colormap parameters
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Feb 5, 2007              chammack    Initial Creation.
 * Aug 20, 2008             dglazesk    Updated for the new ColorMap interface
 * Oct 31, 2010             ryu         use Text widgets for alternative entry
 * Oct 17, 2016 1229        rferrel     Changes to allow non-blocking dialog.
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */
public class ColormapDialog extends CaveJFACEDialog {

    private String title;

    private Button okButton;

    private ColorMapSliderComp cmapSlider;

    private ColorMapCapability cap;

    private ColorMapParameters cmap;

    private IColorMap prevCmap;

    public ColormapDialog(Shell parentShell, String dialogTitle,
            ColorMapCapability cap) {
        this(parentShell, dialogTitle, cap, 0);
    }

    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ColormapDialog(Shell parentShell, String dialogTitle,
            ColorMapCapability cap, int precision) {
        super(parentShell);
        this.title = dialogTitle;
        this.cap = cap;
        this.cmap = cap.getColorMapParameters();
        this.prevCmap = cmap.getColorMap();
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
        if (title != null) {
            shell.setText(title);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse
     * .swt.widgets.Composite)
     */
    @Override
    protected void createButtonsForButtonBar(Composite parent) {
        okButton = createButton(parent, IDialogConstants.OK_ID,
                IDialogConstants.OK_LABEL, false);
        okButton.setVisible(true);

        createButton(parent, IDialogConstants.CANCEL_ID,
                IDialogConstants.CANCEL_LABEL, false);

    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(final Composite parent) {
        Composite composite = (Composite) super.createDialogArea(parent);

        Composite header = new Composite(composite, SWT.NONE);
        header.setLayout(new GridLayout(2, false));
        header.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        Label label = new Label(header, SWT.BOLD);
        label.setText("Colormap: ");
        GridData gd = new GridData(SWT.RIGHT, SWT.CENTER, true, true);
        label.setLayoutData(gd);

        new ColormapComp(header, cmap, cap);

        Group minMaxValues = new Group(composite, SWT.SHADOW_ETCHED_IN);
        minMaxValues.setLayout(new GridLayout(1, true));
        minMaxValues.setText("Colormap Range:");
        minMaxValues.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true,
                true));
        cmapSlider = new ColorMapSliderComp(minMaxValues, cmap);
        cmapSlider.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

        applyDialogFont(composite);
        return composite;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.dialogs.Dialog#cancelPressed()
     */
    @Override
    protected void cancelPressed() {
        cmapSlider.restore();
        cmap.setColorMap(prevCmap);
        cap.notifyResources();
        super.cancelPressed();
    }

    public ColorMapCapability getColorMapCapability() {
        return cap;
    }
}
