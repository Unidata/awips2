package com.raytheon.uf.viz.datadelivery.help;

import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTError;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Find dialog for the Notification Table.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 18, 2012   645      jpiatt     Initial creation.
 * 
 * </pre>
 * 
 * @author jpiatt
 * @version 1.0
 */

public abstract class DataDeliveryHelp {
	
	/** Parent shell */
    protected Shell parentShell;
    
	/** Browser object */
    protected Browser browser;
    
	/** Help Text */
    protected String helpText;
	
	/** Parent shell. */
	Shell shell;
    
    /**
     * Open the browser to display the help text.
     */
    public void open() {
		
		Display display = parentShell.getDisplay();
		shell = new Shell(display);
		shell.setLayout(new FillLayout());
		Browser browser;
		try {
			browser = new Browser(shell, SWT.NONE);
		} catch (SWTError e) {
			System.out.println("Could not instantiate Browser: " + e.getMessage());
			display.dispose();
			return;
		}
		
		browser.setText(helpText);
		shell.open();
		while (!shell.isDisposed()) {
			if (!display.readAndDispatch())
				display.sleep();
		}
		
		close();
		shell.dispose();
		
    }
    
    /**
     * Set isDisposed boolean.
     * 
     * @return boolean
     */
    public boolean isDisposed() {
        return (shell != null && shell.isDisposed());
    }
    
    /**
     * Bring the dialog to top.
     */
    public final void bringToTop() {
        if (shell != null && shell.isDisposed() == false) {
            shell.setVisible(true);
            shell.forceFocus();
            shell.forceActive();
        }
    }
    
    /**
     * Closes the shell.
     * @return 
     *      boolean
     * 
     */
    public final boolean close() {
        if (shell != null && shell.isDisposed() == false) {
            shell.dispose();
        }
        return true;
    }

}
