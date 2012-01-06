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
package test;

import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

public class BasinMainWindow
{
    private Display display;
    private Shell shell;
    
    public BasinMainWindow()
    {
         display = new Display();
         shell = new Shell(display);
    }
    
    public void run()
    {
        shell.setLayout(new GridLayout(1, true));
        
        addDisplayButtons();
        
        shell.setSize(500, 600);
        
//        shell.pack();
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch()) display.sleep();
        }
        
        display.dispose();
    }
    
    private void addDisplayButtons()
    {
//        Button basinDlg = new Button(shell, SWT.PUSH);
//        basinDlg.setText("Basin Trend...");
//        basinDlg.addSelectionListener(new SelectionAdapter()
//        {
//            public void widgetSelected(SelectionEvent event)
//            {
//                Date date =  Calendar.getInstance().getTime();
//                BasinTrendDlg btd = new BasinTrendDlg(shell, date);
//                btd.open();
//            }
//        });  
    }
    
    public static void main(String[] args)
    {
        BasinMainWindow bmw = new BasinMainWindow();
        bmw.run();
    }
}
