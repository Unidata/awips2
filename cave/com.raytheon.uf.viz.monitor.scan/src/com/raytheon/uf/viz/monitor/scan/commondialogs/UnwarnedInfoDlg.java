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
package com.raytheon.uf.viz.monitor.scan.commondialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StyledText;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

/**
 * Displays a dialog that contains information about using the unwarned functionality.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 25, 2009 3039       lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public class UnwarnedInfoDlg extends Dialog
{
    /**
     * Dialog shell.
     */
    private Shell shell;
    
    /**
     * The display control.
     */
    private Display display;
    
    /**
     * Text control.
     */
    private StringBuilder infoText;
    
    /**
     * Constructor.
     * @param parent Parent shell.
     */
    public UnwarnedInfoDlg(Shell parent)
    {
        super(parent, 0);
    }
    
    /**
     * Open method.
     * @return Null.
     */
    public Object open()
    {        
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 0;
        shell.setLayout(mainLayout);
        shell.setText("Unwarned Alarm Information");
      
        // Initialize all of the controls and layouts
        initializeComponents();
        
        shell.pack();    
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch())
            {
                display.sleep();
            }
        }
        
        return null;
    }
    
    /**
     * Initialize the components.
     */
    private void initializeComponents()
    {
        createInfoString();
        
        createControls();
    }
    
    /**
     * Create the text control and the close button.
     */
    private void createControls()
    {       
        Composite controlComp = new Composite(shell, SWT.NONE);
        controlComp.setLayout(new GridLayout(1, false));
        
        GridData gd = new GridData(400, 300);
        StyledText stText = new StyledText(controlComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        stText.setWordWrap(true);
        stText.setText(infoText.toString());
        stText.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 80;
        Button closeBtn = new Button(controlComp, SWT.PUSH);
        closeBtn.setText("Close");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e) 
            {
               shell.dispose();
            }            
        });
    }
    
    /**
     * Create the information text.
     */
    private void createInfoString()
    {        
        infoText = new StringBuilder();
        
        infoText.append("SCAN identifies those storm cells that contain a ");
        infoText.append("Tornado Vortex Signature (TVS) and to some extent ");
        infoText.append("severe weather (based on various storm cell parameters).  ");
        infoText.append("Now SCAN can determine which storm cells currently have ");
        infoText.append("an active TOR or SVR warning and which do not. For ");
        infoText.append("those that do not, the SCAN user can set various storm ");
        infoText.append("cell parameter thresholds (see below). If these thresholds ");
        infoText.append("are met or exceeded and no TOR and/or SVR is in effect ");
        infoText.append("in the polygon  where the cell is located, an Unwarned Storm ");
        infoText.append("Alarm will be issued.\n\n");
        infoText.append("To turn this functionality on for TOR and/or SVR warnings, ");
        infoText.append("simply click the toggle below on and then check and specify ");
        infoText.append("the thresholds you would like be used in order to issue a TOR ");
        infoText.append("and/or SVR Unwarned Storm Cell Alarm. You will know that ");
        infoText.append("an Unwarned Storm Alarm has been issued when the storm ");
        infoText.append("cell identifier in the Storm Cell Table changes color to ");
        infoText.append("magenta for TOR warnings and yellow for SVR warnings.");
    }
}
