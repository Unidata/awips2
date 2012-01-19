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
package com.raytheon.uf.viz.alertviz.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

/**
 * This is a special confirmation dialog to use with dialogs that have an SWT.ON_TOP
 * style.  Any dialog with the SWT.ON_TOP style will appear over a MessageBox (which is
 * modal) and will put the GUI in a dead-lock.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02 Apr 2009  #2194      lvenable    Fix for TTR 577
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class ConfirmationDlg extends Dialog {

    /**
     * Parent shell.
     */
    private Shell parentShell;

    /**
     * Local shell.
     */
    private Shell shell;

    /**
     * The display control.
     */
    private Display display;

    /**
     * Return value when the shell is disposed.
     */
    private int returnValue = SWT.CANCEL;
    
    /**
     * Message to be displayed.
     */
    private String message;
    
    /**
     * SWT (System) Icon to be displayed
     */
    private int iconId;
    
    /**
     * Area to avoid placing the dialog over
     */
    private Rectangle avoidedArea;

    /**
     * Constructor.
     * @param parentShell Parent shell.
     * @param message Message to be displayed.
     */
    public ConfirmationDlg(Shell parentShell, String message, int iconId)
    {
        super(parentShell, 0);
        this.parentShell = parentShell;
        
        this.message = message;
        
        this.iconId = iconId;
    }
    
    /**
     * Open method to display the dialog.
     * @return True if OK was selected, false if Cancel was selected.
     */
    public int open(){
        display = parentShell.getDisplay();

        shell = new Shell(parentShell, SWT.ON_TOP | SWT.APPLICATION_MODAL);
        shell.setText("AlertViz: Acknowledge Message(s)");
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        mainLayout.verticalSpacing = 0;
        shell.setLayout(mainLayout);

        // Initialize all of the controls and layouts
        initializeComponents();        
        
        shell.pack();
        
        setDialogLocation();
        
        shell.open();
        
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        return returnValue;
    }

    /**
     * Initialize the components.
     */
    private void initializeComponents()
    {
        createMessageLabel();
        createBottomButtons();
    }
    
    /**
     * Create the message label.
     */
    private void createMessageLabel()
    {
        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(2, false));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));
        
        Label imageLbl = new Label(labelComp, SWT.NONE);
        imageLbl.setImage(display.getSystemImage(iconId));
        
        Label msgLbl = new Label(labelComp, SWT.NONE);
        msgLbl.setText(message);
    }
    
    /**
     * Create the bottom buttons.
     */
    private void createBottomButtons()
    {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(shell, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        // Add a separator label.
        Label sepLbl = new Label(mainButtonComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, false, false);
        Composite buttonComp = new Composite(shell, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false)); 
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(100, SWT.DEFAULT);
        Button okBtn = new Button(buttonComp, SWT.PUSH);
        okBtn.setText("Yes");
        okBtn.setLayoutData(gd);
        okBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                returnValue = SWT.YES;
                shell.dispose();
            }
        });
        
        gd = new GridData(100, SWT.DEFAULT);
        Button noBtn = new Button(buttonComp, SWT.PUSH);
        noBtn.setText("No");
        noBtn.setLayoutData(gd);
        noBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                returnValue = SWT.NO;
                shell.dispose();
            }
        });
        
        gd = new GridData(100, SWT.DEFAULT);
        Button cancelBtn = new Button(buttonComp, SWT.PUSH);
        cancelBtn.setText("Cancel");
        cancelBtn.setLayoutData(gd);
        cancelBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                returnValue = SWT.CANCEL;
                shell.dispose();
            }
        });
    }
    
    /**
     * Set the location of the dialog.
     */
    private void setDialogLocation()
    {
        Rectangle parentRect = parentShell.getBounds();
        Rectangle shellRect = shell.getBounds();
        
        int newWidth = (parentRect.width - shellRect.width) / 2;
        int newHeight = (parentRect.height - shellRect.height) / 2;
        
        int newXCoord = parentRect.x + newWidth;
        int newYCoord = parentRect.y + newHeight;
        
        int screenWidth = display.getBounds().width;
        int screenHeight = display.getBounds().height;
        
        Rectangle displayBounds = shell.getDisplay().getBounds();
        if (avoidedArea != null) {
            int above = avoidedArea.y - 16 - shellRect.height;
            int below = avoidedArea.y + avoidedArea.height + 16;
            if (above >= displayBounds.y)
                newYCoord = above;
            else if (below + shellRect.height <= displayBounds.y
                    + displayBounds.height)
                newYCoord = below;
        }

        // This is a fail-safe check to make sure the dialog doesn't
        // pop-up off screen.
        if (newYCoord > screenHeight - shellRect.height)
        {
            newYCoord = displayBounds.y + displayBounds.height
                    - shellRect.height - 16;
        }
        if (newXCoord + shellRect.width > screenWidth)
            newXCoord = screenWidth - shellRect.width - 8;
        if (newXCoord < displayBounds.x)
            newXCoord = displayBounds.x + 8;
        
        shell.setLocation(newXCoord, newYCoord);
    }

    /**
     * @return the avoidedArea
     */
    public Rectangle getAvoidedArea() {
        return avoidedArea;
    }

    /**
     * @param avoidedArea the avoidedArea to set
     */
    public void setAvoidedArea(Rectangle avoidedArea) {
        this.avoidedArea = avoidedArea;
    }
}
