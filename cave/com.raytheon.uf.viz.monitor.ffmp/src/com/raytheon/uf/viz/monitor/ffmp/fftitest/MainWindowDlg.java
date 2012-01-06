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
package com.raytheon.uf.viz.monitor.ffmp.fftitest;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.monitor.ffmp.ffti.FFTIControlDlg;

public class MainWindowDlg
{
    private Display display;
    private Shell shell;
    
    public MainWindowDlg()
    {      
        display = new Display();
        shell = new Shell(display);
    }
    
    public void run()
    {
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        shell.setLayout(gl);
        
        shell.setSize(800, 600);
        
//        createChangeButton();
//        addSeparator(shell);
        createFftiLaunchButton();
        
//        shell.pack();
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch()) display.sleep();
        }
        
        display.dispose();
    }
    
//    private void createChangeButton()
//    {
//        Button changeBtn48 = new Button(shell, SWT.PUSH);
//        changeBtn48.setText("Change Slider Maximum to 48");
//        changeBtn48.addSelectionListener(new SelectionAdapter()
//        {
//            @Override
//            public void widgetSelected(SelectionEvent e)
//            {
//                changeSliderMaximum(48.0, 0.25);
//            }            
//        });
//        
//        Button changeBtn30 = new Button(shell, SWT.PUSH);
//        changeBtn30.setText("Change Slider Maximum to 30");
//        changeBtn30.addSelectionListener(new SelectionAdapter()
//        {
//            @Override
//            public void widgetSelected(SelectionEvent e)
//            {
//                changeSliderMaximum(30.0, 0.10);
//            }            
//        });
//    }
    
    private void createFftiLaunchButton()
    {
        Button launchFftiBtn = new Button(shell, SWT.PUSH);
        launchFftiBtn.setText("Launch FFTI");
        launchFftiBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                FFTIControlDlg fftiDlg = new FFTIControlDlg(shell);
                fftiDlg.open();
            }            
        });
    }
    
    private void addSeparator(Composite parentComp)
    {        
        GridLayout gl = (GridLayout)parentComp.getLayout();
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = gl.numColumns;
        Label sepLbl = new Label(parentComp, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);        
    }
    
//    private void changeSliderMaximum(double val, double inc)
//    {
//        canvas2.setMaxAndIncValues(val, inc);
//    }
    
    public static void main(String[] args)
    {
        MainWindowDlg mwd = new MainWindowDlg();
        mwd.run();
    }
}
