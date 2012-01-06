package com.raytheon.uf.viz.monitor.scan.test;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.monitor.scan.commondialogs.TimeHeightDlg;

public class TimeHeightMainDlg
{
    private Display display;
    private Shell shell;
    
    public TimeHeightMainDlg()
    {      
        display = new Display();
        shell = new Shell(display);
    }
    
    public void run()
    {
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        shell.setLayout(gl);
        
        shell.setSize(600, 300);
        
        createButtons();
        
//        shell.pack();
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch()) display.sleep();
        }
        
        display.dispose();
    }
    
    private void createButtons()
    {
        Button timeHeightBtn = new Button(shell, SWT.PUSH);
        timeHeightBtn.setText("Time-Height Graph Dialog");
        timeHeightBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
//                TimeHeightDlg thd = new TimeHeightDlg(shell);
//                thd.open();
            }            
        });
    }
    
    public static void main(String[] args)
    {
        TimeHeightMainDlg mwd = new TimeHeightMainDlg();
        mwd.run();
    }
}
