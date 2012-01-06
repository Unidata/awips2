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
package com.raytheon.viz.volumebrowser.vbui;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.core.slice.request.HeightScale;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.LeftRightMenu;
import com.raytheon.viz.volumebrowser.vbui.VBMenuBarItemsMgr.ViewMenu;

/**
 * Clone dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 9, 2009            jelkins     Initial creation
 * 
 * </pre>
 * 
 * @author jelkins
 * @version 1.0
 */

public class CloneDialog extends CaveSWTDialog {

    public final String DIALOG_TITLE = "Volume Browser Clone";

    private CloneProductTableComposite productTableComponent;

    private final List<ProductTableData> selectedProducts;

    /**
     * @param parentShell
     * @param selectedProducts
     */
    protected CloneDialog(Shell parentShell,
            List<ProductTableData> selectedProducts) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.DO_NOT_BLOCK); // Win32
        setText(DIALOG_TITLE);
        this.selectedProducts = selectedProducts;
    }

    @Override
    protected void opened() {
        productTableComponent.resizeTableColumns();
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.FILL, true, true);
    }

    @Override
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 2;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected void initializeComponents(Shell shell) {
        initializeComponents();
        setupListeners();
    }

    @Override
    protected void preOpened() {
        shell.setMinimumSize(shell.getSize());
    }

    @Override
    protected boolean shouldOpen() {
        if (selectedProducts.size() == 0) {
            return false;
        }
        return true;
    }

    /**
     * Initialize the components
     */
    private void initializeComponents() {
        createDialogSettingsComponent();

        productTableComponent = new CloneProductTableComposite(shell,
                selectedProducts);

    }

    /**
     * setup the listeners
     */
    private void setupListeners() {
        shell.addControlListener(new ControlAdapter() {
            /*
             * (non-Javadoc)
             * 
             * @see
             * org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse
             * .swt.events.ControlEvent)
             */
            @Override
            public void controlResized(ControlEvent e) {
                productTableComponent.resizeTableColumns();
            }
        });
    }

    private void createDialogSettingsComponent() {
        Composite composite = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(4, false);
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        gl.horizontalSpacing = 20;
        composite.setLayout(gl);
        composite
                .setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        VolumeBrowserDialogSettings dialogSettings = selectedProducts.get(0)
                .getCatalogEntry().getDialogSettings();

        ViewMenu viewSelection = dialogSettings.getViewSelection();
        LeftRightMenu timeDirectionSelection = dialogSettings
                .getTimeDirectionSelection();
        HeightScale heightScale = dialogSettings
                .getHeightScaleSelection();

        // there will always be a view selection
        (new Label(composite, SWT.NONE)).setText(viewSelection
                .getDisplayString());

        // the space/time selection isn't static, it only effects what menus are
        // available in the main dialog

        // time direction
        if (timeDirectionSelection != null) {
            (new Label(composite, SWT.NONE)).setText(timeDirectionSelection
                    .getDisplayString());
        }

        // the points menu doesn't matter because the selected point will be
        // obvious in the names of the products

        // the vertical resolution
        if (heightScale != null) {
            (new Label(composite, SWT.NONE)).setText(heightScale.getName());
        }

    }

}
