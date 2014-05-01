/*
 * gov.noaa.nws.ncep.ui.pgen.products.ProductControlDialog
 * 
 * August 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */
package gov.noaa.nws.ncep.ui.pgen.productmanage;

import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.elements.Product;
import gov.noaa.nws.ncep.ui.pgen.producttypes.PgenClass;
import gov.noaa.nws.ncep.ui.pgen.producttypes.ProductType;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;

import java.awt.Color;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

/**
 * This class is the common dialog for PGEN product management in PGEN.
 * 
 * Note: This might be consolidated with PgenLayeringDialog.java in the future.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 08/09  		#151		J. Wu		Initial creation. 
 * 02/12  		#656		J. Wu		Retain the last location of the dialog. 
 * 07/12  		#822		J. Wu		Add createShell() so it can be overrided 
 *                                      to create shell with different styles.
 * 
 * </pre>
 * 
 * @author jwu
 * @version 1.0
 * 
 */

public class ProductDialog extends Dialog {

    /**
     * Return object.
     */
    protected final Boolean returnValue = false;

    /**
     * Dialog shell.
     */
    protected Shell shell = null;

    protected Display display = null;

    /**
     * PgenResource, active product, and active layer.
     */
    protected PgenResource drawingLayer = null;

    protected Product currentProduct = null;

    protected Layer currentLayer = null;

    protected Point shellLocation;

    /**
     * Constructor.
     */
    public ProductDialog(Shell parentShell) {

        super(parentShell);

    }

    /**
     * Open method used to display the product control dialog.
     * 
     * @return Return object (can be null).
     */
    public Object open() {

        // Link to the drawing layer's active product & active layer;
        drawingLayer = PgenSession.getInstance().getPgenResource();
        currentProduct = drawingLayer.getActiveProduct();
        currentLayer = drawingLayer.getActiveLayer();

        // Reset the UNDO/REDO;
        PgenSession.getInstance().disableUndoRedo();

        // Create the main shell;
        Shell parent = this.getParent();
        shell = createShell(parent);

        // Set the title of the dialog.
        setTitle();

        // Create the main layout for the shell.
        setLayout();

        // Set the default location.
        setDefaultLocation(parent);

        // Create and initialize all of the controls and layouts
        initializeComponents();

        /*
         * Add a "CLOSE" listenser to the shell to handle the event when the
         * user clicks the "X" in the shell window.
         */
        Listener[] closeListeners = shell.getListeners(SWT.Close);
        if (closeListeners != null && closeListeners.length > 0) {
            for (Listener ls : closeListeners) {
                shell.removeListener(SWT.Close, ls);
            }
        }

        shell.addListener(SWT.Close, new shellCloseListener());

        // Pack and open
        shell.pack();
        shell.open();

        /*
         * Pops up a second window, if needed. Note: must be popped up before
         * the event handling loop.
         */
        popupSecondDialog();

        /*
         * Exit directly from CAVE may get parent disposed and we need to catch
         * if here.
         */
        if (parent == null || parent.isDisposed()) {
            return returnValue;
        }

        // Event handling loop for this dialog
        display = parent.getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnValue;
    }

    /**
     * Create a new modeless hell by default.
     * 
     * @return Return shell.
     */
    protected Shell createShell(Shell parent) {
        Shell newShell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MODELESS);
        return newShell;
    }

    /**
     * Sets the title of the dialog.
     */
    public void setTitle() {
        shell.setText("");
    }

    /**
     * Creates the main layout for the shell.
     */
    public void setLayout() {

        GridLayout mainLayout = new GridLayout(1, true);

        mainLayout.marginHeight = 1;
        mainLayout.marginWidth = 1;
        mainLayout.verticalSpacing = 2;
        mainLayout.horizontalSpacing = 1;

        shell.setLayout(mainLayout);
    }

    /**
     * Set the default location.
     * 
     * @param parent
     */
    public void setDefaultLocation(Shell parent) {

        if (shellLocation == null) {
            Point pt = parent.getLocation();
            shell.setLocation(pt.x, pt.y);
        } else {
            shell.setLocation(shellLocation);
        }

    }

    /**
     * Initialize the dialog components.
     */
    public void initializeComponents() {
    }

    /**
     * Check the dialog is opened or not
     */
    public boolean isOpen() {
        return (shell != null && !shell.isDisposed());
    }

    /**
     * Pops up a second dialog
     */
    protected void popupSecondDialog() {
    }

    /**
     * Set a button's color
     */
    public void setButtonColor(Button btn, Color clr) {

        btn.setBackground(new org.eclipse.swt.graphics.Color(display, clr
                .getRed(), clr.getGreen(), clr.getBlue()));

    }

    /*
     * Close the dialog
     */
    public void close() {

        if (shell != null && !shell.isDisposed()) {
            Rectangle bounds = shell.getBounds();
            shellLocation = new Point(bounds.x, bounds.y);
            shell.dispose();
        }

    }

    /**
     * Get a list of names for selected button in a ProductType, regardless of
     * if it is control, action, class, or object.
     */
    public List<String> getButtonList(ProductType ptyp) {

        List<String> btnList = null;

        if (ptyp != null) {

            btnList = new ArrayList<String>();

            if (ptyp.getPgenControls() != null) {
                btnList.addAll(ptyp.getPgenControls().getName());
            }

            if (ptyp.getPgenActions() != null) {
                btnList.addAll(ptyp.getPgenActions().getName());
            }

            for (PgenClass cls : ptyp.getPgenClass()) {
                btnList.add(cls.getName());

                if (cls != null && cls.getPgenObjects() != null) {
                    btnList.addAll(cls.getPgenObjects().getName());
                }
            }
        }

        return btnList;
    }

    /**
     * Reset the PGEN palette based on the settings of a product type.
     */
    public void refreshPgenPalette(ProductType ptyp) {
        if (PgenSession.getInstance().getPgenPalette() != null)
            PgenSession.getInstance().getPgenPalette()
                    .resetPalette(getButtonList(ptyp));
    }

    /*
     * A listener to handle the event when the user clicks on the "X" on the
     * dialog.
     */
    private class shellCloseListener implements Listener {
        public void handleEvent(Event e) {
            switch (e.type) {
            case SWT.Close:
                exit();
                break;
            }
        }
    }

    /*
     * Exit the dialog - default is to close the dialog.
     */
    protected void exit() {
        close();
    }

    /*
     * Check if need to save changes.
     */
    protected boolean needSaving() {
        return PgenSession.getInstance().getPgenResource().getResourceData()
                .isNeedsSaving();
    }

}
