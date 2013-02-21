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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.RejectedExecutionException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

/**
 * 
 * Base class for CaveSWTDialog, does not require the eclipse workbench to have
 * started (cave does not have to be running to use). 99% of time, do not extend
 * this class except for rapid prototyping or if you have perspective
 * independent standalone components
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 2, 2010             mschenke    Initial creation
 * Sep 12, 2012 #1165      lvenable    Update for the initial process
 *                                     of removing the dialog blocking capability.
 * Oct 11, 2012  1229      jkorman     Factored out "mustCreate" method from subclasses.                                    
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
public abstract class CaveSWTDialogBase extends Dialog {

    /** Cave dialog attributes */
    public static class CAVE {

        /**
         * No CAVE style.
         */
        public static int NONE = 0;

        /**
         * Does not block when open() is called. Normally the open() will wait
         * until the shell is disposed before leaving the open() method. This
         * style will skip the waiting part and will leave the open method.
         */
        public static int DO_NOT_BLOCK = 1 << 1;

        /**
         * The dialog will always be displayed even when the perspectives are
         * changed (switching between GFE & D2D for example).
         */
        public static int PERSPECTIVE_INDEPENDENT = 1 << 2;

        /**
         * This style will skip all of the default control painting for
         * Operational, Practice, and Test. Some dialogs have to have customized
         * control colors and the ModeListener will override the changes. This
         * style prevents the ModeListener from being called.
         */
        public static int MODE_INDEPENDENT = 1 << 3;

        /**
         * This style will create the dialog off of the display and not parent
         * shell. This will allow the dialog to display the minimize/maximize
         * buttons in the title bar. The dialog will also be displayed in the
         * task bar separately from CAVE.
         */
        public static int INDEPENDENT_SHELL = 1 << 4;

        /**
         * Dialog should not pack the shell and the controls before opening.
         * Normally this should not be called. If a dialog needs to be a certain
         * size the controls should be sized properly and the shell packed.
         */
        public static int NO_PACK = 1 << 5;
    }

    private static class ListenerPair {
        private int eventType;

        private Listener listener;

        private ListenerPair(int eventType, Listener listener) {
            this.eventType = eventType;
            this.listener = listener;
        }
    }

    /** Style used to determine how the dialog will function. */
    private int caveStyle = CAVE.NONE;

    /** Display reference. */
    private Display display;

    /** Dialog last location on the screen. */
    protected Point lastLocation;

    /** Flag indicating of the dialog was visible. */
    protected boolean wasVisible = true;

    /** Return value. */
    private Object returnValue;

    /** Shell reference. */
    protected Shell shell;

    private List<ListenerPair> listenersToAdd;

    /** Callback called when the dialog is disposed. */
    private ICloseCallback closeCallback = null;

    /**
     * Construct default cave dialog
     * 
     * @param parentShell
     */
    protected CaveSWTDialogBase(Shell parentShell) {
        this(parentShell, SWT.DIALOG_TRIM, CAVE.NONE);
    }

    /**
     * Construct dialog with parent shell and specific swt style
     * 
     * @param parentShell
     * @param swtStyle
     */
    protected CaveSWTDialogBase(Shell parentShell, int swtStyle) {
        this(parentShell, swtStyle, CAVE.NONE);
    }

    /**
     * Construct dialog with parent shell and swt style and cave style
     * 
     * @param parentShell
     * @param style
     */
    protected CaveSWTDialogBase(Shell parentShell, int style, int caveStyle) {
        super(parentShell, style);
        this.caveStyle = caveStyle;
        listenersToAdd = new ArrayList<ListenerPair>();
    }

    /**
     * Open method used to display the dialog. Once open is called, it cannot be
     * called again until it has been closed. Subsequent open calls will give
     * the dialog focus and return null
     * 
     * @return Dialog specific object
     */
    public final Object open() {
        if (isOpen()) {
            bringToTop();
            return getReturnValue();
        }

        if (shouldOpen() == false) {
            return getReturnValue();
        }

        Shell parent = getParent();
        display = parent.getDisplay();
        if (hasAttribute(CAVE.INDEPENDENT_SHELL)) {
            shell = new Shell(display, getStyle());
            final DisposeListener disposeListener = new DisposeListener() {
                @Override
                public void widgetDisposed(DisposeEvent e) {
                    shell.dispose();
                }
            };
            parent.addDisposeListener(disposeListener);
            shell.addDisposeListener(new DisposeListener() {

                @Override
                public void widgetDisposed(DisposeEvent e) {
                    getParent().removeDisposeListener(disposeListener);

                }
            });
        } else {
            shell = new Shell(parent, getStyle());
        }

        shell.setText(getText());

        if (doesNotHaveAttribute(CAVE.MODE_INDEPENDENT)) {
            new ModeListener(shell);
        }

        // Create the main layout for the shell.
        shell.setLayout(constructShellLayout());
        shell.setLayoutData(constructShellLayoutData());

        // Initialize all of the controls and layouts
        initializeComponents(shell);

        // pack and open the dialog
        if (doesNotHaveAttribute(CAVE.NO_PACK)) {
            shell.pack();
        }

        shell.addDisposeListener(new DisposeListener() {
            @Override
            public void widgetDisposed(DisposeEvent e) {
                disposed();
                callCloseCallback();
            }
        });

        for (ListenerPair lp : listenersToAdd) {
            shell.addListener(lp.eventType, lp.listener);
        }
        listenersToAdd.clear();

        preOpened();

        shell.open();

        opened();

        if (!hasAttribute(CAVE.DO_NOT_BLOCK)) {
            while (!shell.isDisposed()) {
                if (!display.readAndDispatch()) {
                    display.sleep();
                }
            }
        }

        // Get the return value
        return getReturnValue();
    }

    /**
     * Gives the dialog focus
     */
    public final void bringToTop() {
        if (shell != null && shell.isDisposed() == false) {
            shell.setVisible(true);
            shell.forceFocus();
            shell.forceActive();
        }
    }

    /**
     * Method called before open() and after pack() is called on shell. Default
     * implementation does nothing
     */
    protected void preOpened() {

    }

    /**
     * Method called after open() is called on shell. Default implementation
     * does nothing
     */
    protected void opened() {

    }

    /**
     * Method called when shell is diposed, right before returning from open()
     * if blocking or through a dispose listener if not blocking, default is do
     * nothing
     */
    protected void disposed() {

    }

    /**
     * Call the callback method as this dialog has been disposed. This action is
     * in a separate method since the disposed method can be overridden.
     */
    private void callCloseCallback() {
        if (closeCallback != null) {
            closeCallback.dialogClosed(returnValue);
        }
    }

    /**
     * Construct the layout for the shell. Defaults to a GridLayout with one
     * column and margins set to 3
     * 
     * @return the shell layout
     */
    protected Layout constructShellLayout() {
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        return mainLayout;
    }

    /**
     * Construct the shell layout data, default to null
     * 
     * @return
     */
    protected Object constructShellLayoutData() {
        return null;
    }

    /**
     * Subclasses must implement this method to initialize the dialog components
     * 
     * @param shell
     */
    protected abstract void initializeComponents(Shell shell);

    /**
     * Closes the shell.
     * 
     */
    public final boolean close() {
        if (shell != null && shell.isDisposed() == false) {
            shell.dispose();
        }
        return true;
    }

    /**
     * @return the shell
     */
    public final Shell getShell() {
        return shell;
    }

    /**
     * Get the display object.
     * 
     * @return The display object.
     */
    public final Display getDisplay() {
        return display;
    }

    /**
     * Get the last location of the shell.
     * 
     * @return The last location as a point (X/Y coordinate).
     */
    public final Point getLastLocation() {
        return lastLocation;
    }

    /**
     * Set the return value, should be used by subclasses only
     * 
     * @param returnValue
     */
    protected final void setReturnValue(Object returnValue) {
        this.returnValue = returnValue;
    }

    /**
     * Get the return value to return from open()
     * 
     * @return
     */
    public final Object getReturnValue() {
        return returnValue;
    }

    /**
     * Set the dialog title.
     */
    @Override
    public final void setText(String string) {
        super.setText(string);
        if (shell != null) {
            shell.setText(string);
        }
    }

    /**
     * Returns whether the dialog has been opened yet or not
     * 
     * @return
     */
    public final boolean isOpen() {
        return (shell != null && !shell.isDisposed());
    }

    /**
     * Returns if the dialog is disposed, a null dialog will not mean it is
     * disposed as it may not have been opened yet.
     * 
     * @return boolean
     */
    public final boolean isDisposed() {
        return (shell != null && shell.isDisposed());
    }

    /**
     * Can be overridden by the subclass to determine if the dialog should open
     * or not.
     * 
     * @return True if the dialog is to be opened, false otherwise.
     */
    protected boolean shouldOpen() {
        return true;
    }

    /**
     * Check if the caveStyle contains a specified attribute.
     * 
     * @param attribute
     *            Attribute to check for.
     * @return True if caveStyle contains the attribute. False if it doesn't.
     */
    protected boolean hasAttribute(int attribute) {
        return (caveStyle & attribute) == attribute;
    }

    /**
     * Check if the caveStyle does not contain a specified attribute.
     * 
     * @param attribute
     *            Attribute to check for.
     * @return True if caveStyle does not contain the attribute. False if it
     *         does.
     */
    protected boolean doesNotHaveAttribute(int attribute) {
        return (caveStyle & attribute) != attribute;
    }

    /**
     * Add a listener to the dialog's shell for the specified event type
     * 
     * @param type
     * @param listener
     */
    public void addListener(int eventType, Listener listener) {
        if (shell != null) {
            shell.addListener(eventType, listener);
        } else {
            listenersToAdd.add(new ListenerPair(eventType, listener));
        }
    }

    /**
     * Recursively set the enabled flag on the control. Will set on children if
     * control is a composite
     * 
     * @param control
     * @param enable
     */
    public static void enable(Control control, boolean enable) {
        control.setEnabled(enable);
        if (control instanceof Composite) {
            Control[] children = ((Composite) control).getChildren();
            for (Control c : children) {
                enable(c, enable);
            }
        }
    }

    /**
     * Add a callback to the dialog. This callback will be called when the
     * dialog is disposed. Also, the caveStyle is updated to include
     * DO_NOT_BLOCK.
     * 
     * @param callback
     *            Callback to be called when the dialog is disposed.
     * @throws Throws
     *             a RejectedExecutionException with a message indicating that
     *             this method needs to be called before the open method.
     */
    public void setCloseCallback(ICloseCallback callback) {

        if (isOpen()) {
            StringBuilder sb = new StringBuilder();
            sb.append("The method addCloseCallback() needs to be called before the open().  ");
            sb.append("This is due to addCloseCallback setting the caveStyle to DO_NOT_BLOCK");
            throw new RejectedExecutionException(sb.toString());
        }

        // Set the DO_NOT_BLOCK on the cave style
        this.caveStyle = caveStyle | CAVE.DO_NOT_BLOCK;
        this.closeCallback = callback;
    }

    /**
     * Determines if the supplied reference should be created.
     * 
     * @param dialog
     *            A dialog reference.
     * @return Should the supplied reference should be created.
     */
    public boolean mustCreate(CaveSWTDialogBase dialog) {
        return (dialog == null) || (dialog.getShell() == null)
                || (dialog.isDisposed());
    }
    
}
