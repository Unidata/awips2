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
package com.raytheon.uf.viz.monitor.ffmp.test;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.ITimeDurationAction;
import com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.TimeDurScaleComp;

/**
 * TODO Add Description
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 11, 2010            lvenable     Initial creation
 *
 * </pre>
 *
 * @author lvenable
 * @version 1.0	
 */

public class TimeDurationTestDlg implements ITimeDurationAction
{
    private Display display;
    private Shell shell;
    
    private TimeDurScaleComp timeDurScale;
    
    public TimeDurationTestDlg()
    {
        display = new Display();
        shell = new Shell(display);
    }
    
    public void run()
    {
        shell.setLayout(new GridLayout(1, true));
        
        addControls();
        
        shell.setSize(500, 600);
        
//        shell.pack();
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch()) display.sleep();
        }
        
        display.dispose();
    }
    
    private void addControls()
    {        
        timeDurScale = new TimeDurScaleComp(shell, this);
        
        Button resetTimeBtn = new Button(shell, SWT.PUSH);
        resetTimeBtn.setText("Reset Time");
        resetTimeBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                timeDurScale.setTimeDuration(2.26);
            }            
        });
    }

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        TimeDurationTestDlg dlg = new TimeDurationTestDlg();
        dlg.run();

    }

    /* (non-Javadoc)
     * @see com.raytheon.uf.viz.monitor.ffmp.ui.dialogs.ITimeDurationAction#timeDurationUpdated(double, boolean)
     */
    @Override
    public void timeDurationUpdated(double val, boolean split)
    {
        System.out.println("Call back");        
    }

}
