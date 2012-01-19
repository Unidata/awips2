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
package com.raytheon.uf.viz.monitor.ffmp.fffgtest;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.monitor.ffmp.fffg.FFFGDlg;

public class FFFGLauncherDlg
{
    private Display display;
    private Shell shell;
    
    public FFFGLauncherDlg()
    {
        display = new Display();
        shell = new Shell(display);
    }
    
    private void run()
    {
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        shell.setLayout(gl);
        
        shell.setSize(600, 400);
        
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
        Button launchFffgBtn = new Button(shell, SWT.PUSH);
        launchFffgBtn.setText("Launch FFFG");
        launchFffgBtn.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                FFFGDlg fffgDlg = new FFFGDlg(shell);
                fffgDlg.open();
            }            
        });
    }
    
    public static void main(String[] args)
    {
        FFFGLauncherDlg fld = new FFFGLauncherDlg();
        fld.run();
    }
}
