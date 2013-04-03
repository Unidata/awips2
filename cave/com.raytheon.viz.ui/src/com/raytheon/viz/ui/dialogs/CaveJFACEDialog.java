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

import java.util.concurrent.RejectedExecutionException;

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
 * 09/13/12     1165        lvenable     Update for the initial process
 *                                       of removing the dialog blocking capability.
 * 09/20/12     1196        rferrel      Changes to setBlockOnOpen.
 * 09/27/12     1196        rferrel      Added bringToTop
 * 11/13/12     1298        rferrel      Override open to work in a similar manner
 *                                        to CaveSWTDialogBase's open.
 * 
 * </pre>
 * 
 * @author Dan Fitch
 * @version 1
 */
public class CaveJFACEDialog extends Dialog implements
        IPerspectiveSpecificDialog {

    /** Dialog last location on the screen. */
    protected Point lastLocation;

    /** Flag indicating of the dialog was visible. */
    private boolean wasVisible = true;

    /** Callback called when the dialog is disposed. */
    private ICloseCallback closeCallback = null;

    /** Flag indicating if the dialog was blocked when opened. */
    private boolean blockedOnOpen = false;

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
        // Eventually this will be the default but for now do not know what this
        // will break.
        // setBlockOnOpen(false);
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

                callCloseCallback();
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

    /**
     * Gives the dialog focus.
     */
    public final void bringToTop() {
        Shell shell = getShell();
        if (shell != null && shell.isDisposed() == false) {
            shell.setVisible(true);
            shell.forceFocus();
            shell.forceActive();
        }
    }

    /**
     * Call the callback method as this dialog has been disposed.
     */
    private void callCloseCallback() {
        if (closeCallback != null) {
            closeCallback.dialogClosed(new Integer(getReturnCode()));
        }
    }

    /**
     * Returns whether the dialog has been opened yet or not
     * 
     * @return True if the dialog was opened, false otherwise.
     */
    public final boolean isOpen() {
        return (getShell() != null && !getShell().isDisposed());
    }

    /**
     * Returns if the dialog is disposed, a null dialog will not mean it is
     * disposed as it may not have been opened yet.
     * 
     * @return
     */
    public final boolean isDisposed() {
        Shell shell = getShell();
        return (shell != null && shell.isDisposed());
    }

    /**
     * Add a callback to the dialog. This callback will be called when the
     * dialog is disposed.
     * 
     * @param callback
     *            Callback to be called when the dialog is disposed.
     */
    public void setCloseCallback(ICloseCallback callback) {

        /*
         * Since JFACE allows you to call setBlockOnOpen() after the
         * constructor, if the open() method is called before setBlockOnOpen
         * then the block is ignored. Here we are checking if the block was set
         * and if the dialog is already open because that makes the callback
         * pointless.
         */
        if (blockedOnOpen && isOpen()) {
            StringBuilder sb = new StringBuilder();
            sb.append("The method setBlockOnOpen() was called and set to true.  The callback method ");
            sb.append("will not run correctly as the dialog has been opened and blocked before this ");
            sb.append("method was called.");
            throw new RejectedExecutionException(sb.toString());
        }

        this.closeCallback = callback;
    }

    /**
     * This method overrides the existing setBlockOnOpen() method. This will
     * eventually be a catch method that will prevent blocking the dialog on
     * open. At this time it serves as a placeholder for upcoming work.
     * 
     * @param blockOnOpen
     *            Flag indicating if the dialog should block when opened.
     */
    @Override
    public void setBlockOnOpen(boolean blockOnOpen) {
        // TODO investigate eventually should never allow blocking?
        // /*
        // * If the dialog is already opened then just return because setting
        // the
        // * block won't work. In JFACE the setBlockOnOpen needs to be set
        // before
        // * the open() call, otherwise it is ignored.
        // */
        // if (isOpen()) {
        // return;
        // }

        super.setBlockOnOpen(blockOnOpen);
        // blockedOnOpen = blockOnOpen;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.jface.window.Window#open()
     */
    @Override
    public int open() {
        if (isOpen()) {
            bringToTop();
            return getReturnCode();
        }

        return super.open();
    }
}
