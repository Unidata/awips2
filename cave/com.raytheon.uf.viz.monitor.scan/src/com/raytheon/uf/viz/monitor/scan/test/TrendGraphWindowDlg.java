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
package com.raytheon.uf.viz.monitor.scan.test;

import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.monitor.scan.config.SCANConfig;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.CELLTable;
import com.raytheon.uf.common.monitor.scan.config.SCANConfigEnums.ScanTables;
import com.raytheon.uf.viz.monitor.scan.TrendGraphData;
import com.raytheon.uf.viz.monitor.scan.commondialogs.TrendGraphCanvas;

public class TrendGraphWindowDlg extends Dialog
{
    private Display display;
    private Shell shell;
    
    /*
     * Canvas information
     */
    private TrendGraphCanvas trendCanvas;
    
    private Shell parentShell;
    
    private TrendGraphData trendGraphData;
    
    private Combo trendsCbo;
    
    private Calendar cal;
    
    private Date startTime;
    
    public TrendGraphWindowDlg(Shell parentShell)
    {      
        super(parentShell, 0);
    }
    
    public Object open()
    {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM);
        shell.setText("Trend Sample");
        
        GridLayout gl = new GridLayout(1, false);
        gl.horizontalSpacing = 0;
        shell.setLayout(gl);
        
        shell.setSize(600, 300);
        
        initComponents();
        
        shell.open();
        
        while (!shell.isDisposed())
        {
            if (!display.readAndDispatch()) display.sleep();
        }
        
        return null;
    }
    
    private void initComponents()
    {
        cal = Calendar.getInstance();
        startTime = cal.getTime();
        trendGraphData = new TrendGraphData();
        
        setupPoshData();
//        setupAzmData();
        
        createCanvas();        
        createComboControls(); 
        
        handleComboAction();
    }
    
    private void createCanvas()
    {                    
        trendCanvas = new TrendGraphCanvas(shell, trendGraphData, startTime, ScanTables.CELL, 
                           CELLTable.POSH.getColName(),null,null,null);
        
    }
    
    private void createComboControls()
    {
        trendsCbo = new Combo(shell, SWT.DROP_DOWN | SWT.READ_ONLY);
        trendsCbo.addSelectionListener(new SelectionAdapter()
        {
            @Override
            public void widgetSelected(SelectionEvent e)
            {
                handleComboAction();
            }            
        });
        
        SCANConfig scanCfg = SCANConfig.getInstance();
        
        String[] attrs = scanCfg.getTrendAttributes(ScanTables.CELL);
        
        for (String str : attrs)
        {
            trendsCbo.add(str);
        }
        
        int idx = trendsCbo.indexOf("posh");
        
        trendsCbo.select(idx);
    }
    
    private void handleComboAction()
    {
        
        String attrName = trendsCbo.getItem(trendsCbo.getSelectionIndex());
        
        trendCanvas.updateAttribute(attrName, trendGraphData, startTime); 
    }
    
//    private void setupAzmData()
//    {
//        Date d = null;
//        
//        d = cal.getTime();
//        dataMap.put(d, 222.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 200.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 150.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 170.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 210.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 123.0);
//        
//        cal.add(Calendar.MINUTE, -5);
//        d = cal.getTime();
//        dataMap.put(d, 192.0);
//    }
    
    private void setupPoshData()
    {
        Date d = null;
        LinkedHashMap<Date, Double> dataMap = new LinkedHashMap<Date, Double>();
        d = cal.getTime();
        dataMap.put(d, 90.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 30.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 10.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 50.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 25.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 63.0);
        
        cal.add(Calendar.MINUTE, -5);
        d = cal.getTime();
        dataMap.put(d, 40.0);
        
        trendGraphData.setGraphData(dataMap);
    }
}
