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

import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.core.mode.CAVEMode;
import com.raytheon.viz.ui.perspectives.AbstractVizPerspectiveManager;
import com.raytheon.viz.ui.perspectives.IPerspectiveSpecificDialog;
import com.raytheon.viz.ui.perspectives.VizPerspectiveListener;

/**
 * CaveJFACEDialog.
 * 
 * Extends the org.eclipse.jface.dialogs.Dialog to be able to change the
 * background color when CAVE is in training or practice mode.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ----------	----------	-----------	--------------------------
 * 12/20/07     561         Dan Fitch    Initial Creation.
 * 04/22/08     1088        chammack     Added dialog event propagation fix
 * </pre>
 * 
 * @author Dan Fitch
 * @version 1
 */
public class CaveJFACEDialog extends Dialog implements
        IPerspectiveSpecificDialog {

    protected Point lastLocation;

    private boolean wasVisible = true;

    /**
     * 
     * @param parentShell
     */
    protected CaveJFACEDialog(Shell parentShell) {
        this(parentShell, true);
    }

    /**
     * 
     * @param parentShell
     */
    protected CaveJFACEDialog(Shell parentShell, boolean perspectiveSpecific) {
        super(parentShell);
        if (perspectiveSpecific) {
            AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                    .getCurrentPerspectiveManager();
            if (mgr != null) {
                mgr.addPerspectiveDialog(this);
            }
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createContents(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createContents(Composite parent) {
        // Fix the transition delay between dialog and workbench
        // IContextService svc = (IContextService) PlatformUI.getWorkbench()
        // .getService(IContextService.class);
        // svc.registerShell(this.getShell(), IContextService.TYPE_WINDOW);

        Composite comp = (Composite) super.createContents(parent);
        comp.setBackground(CAVEMode.getBackgroundColor());

        Point size = getInitialSize();
        getShell().setSize(size);
        getShell().setLocation(getInitialLocation(size));
        getShell().addDisposeListener(new DisposeListener() {

            @Override
            public void widgetDisposed(DisposeEvent e) {
                AbstractVizPerspectiveManager mgr = VizPerspectiveListener
                        .getCurrentPerspectiveManager();
                if (mgr != null) {
                    mgr.removePespectiveDialog(CaveJFACEDialog.this);
                }
            }

        });
        return comp;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createButtonBar(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createButtonBar(Composite parent) {
        Composite buttonBar = (Composite) super.createButtonBar(parent);
        new ModeListener(buttonBar);

        ((GridData) buttonBar.getLayoutData()).horizontalAlignment = SWT.CENTER;
        return buttonBar;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets
     * .Composite)
     */
    @Override
    protected Control createDialogArea(Composite parent) {
        Composite comp = (Composite) super.createDialogArea(parent);
        new ModeListener(comp);
        return comp;
    }

    @Override
    public final void hide() {
        Shell shell = getShell();
        if (shell != null && shell.isDisposed() == false) {
            wasVisible = shell.isVisible();
            lastLocation = shell.getLocation();
            shell.setVisible(false);
        }
    }

    @Override
    public final void restore() {
        Shell shell = getShell();
        if (shell != null && shell.isDisposed() == false) {
            shell.setVisible(wasVisible);
            shell.setLocation(lastLocation);
        }
    }
}
