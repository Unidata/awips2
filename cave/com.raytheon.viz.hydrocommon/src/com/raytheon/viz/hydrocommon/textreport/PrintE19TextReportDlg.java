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
package com.raytheon.viz.hydrocommon.textreport;

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
import org.eclipse.swt.graphics.Color;


import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.E19Pages;
/**
 * print menu for report E-19.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 11, 2012 13781      wkwock     Initial creation
 * 
 * </pre>
 * 
 * @author wkwock
 * @version 1.0
 */

public class PrintE19TextReportDlg extends PrintTextReportDlg {
	private E19Report report=null;
    Button converBtn = null;
    Button mapBtn = null;
    Button benchmarkBtn = null;
    Button gagesBtn = null;
    Button historyBtn = null;
    Button crestsBtn = null;
    Button lowWaterBtn = null;
    Button conditionsBtn = null;
    Button damageBtn = null;
    Button riverStageBtn = null;
    Button contactsBtn = null;
    Button allPageBtn = null;
	
	protected PrintE19TextReportDlg(Shell parentShell, E19Report rpt) {
		super(parentShell, rpt);
		this.report=rpt;
		this.setText("E-19 - Print - " + report.lid);
	}
	
	@Override
	protected void initializeComponents(Shell shell) {
		createTableOfContents();
		createBottomButtons();
	}

	private void createTableOfContents(){
		GridData gd = new GridData(GridData.FILL_BOTH);
        Composite comp = new Composite(shell, SWT.NONE);
        GridLayout gridLayout = new GridLayout(1, false);
        comp.setLayout(gridLayout);
        comp.setLayoutData(gd);
        Label header = new Label(comp, SWT.RIGHT);
        header.setText("   E-19 Report Contents:");
        
        Label seperator1=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        seperator1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        converBtn = new Button(comp, SWT.CHECK);
        converBtn.setText(E19Pages.COVER.getPageName().toUpperCase());
        converBtn.addSelectionListener(new E19Selection());
        mapBtn = new Button(comp, SWT.CHECK);
        mapBtn.setText(E19Pages.MAP_GAGE_LOCATION.getPageName().toUpperCase());
        mapBtn.addSelectionListener(new E19Selection());
        benchmarkBtn = new Button(comp, SWT.CHECK);
        benchmarkBtn.setText(E19Pages.BENCHMARKS.getPageName().toUpperCase());
        benchmarkBtn.addSelectionListener(new E19Selection());
        gagesBtn = new Button(comp, SWT.CHECK);
        gagesBtn.setText(E19Pages.GAGES.getPageName().toUpperCase());
        gagesBtn.addSelectionListener(new E19Selection());
        historyBtn = new Button(comp, SWT.CHECK);
        historyBtn.setText(E19Pages.HISTORY.getPageName().toUpperCase());
        historyBtn.addSelectionListener(new E19Selection());
        crestsBtn = new Button(comp, SWT.CHECK);
        crestsBtn.setText(E19Pages.CRESTS.getPageName().toUpperCase());
        crestsBtn.addSelectionListener(new E19Selection());
        lowWaterBtn = new Button(comp, SWT.CHECK);
        lowWaterBtn.setText(E19Pages.LOW_WATER_RECORDS.getPageName().toUpperCase());
        lowWaterBtn.addSelectionListener(new E19Selection());
        conditionsBtn = new Button(comp, SWT.CHECK);
        conditionsBtn.setText(E19Pages.CONDITIONS_AFFECTING_FLOW.getPageName().toUpperCase());
        conditionsBtn.addSelectionListener(new E19Selection());
        damageBtn = new Button(comp, SWT.CHECK);
        damageBtn.setText(E19Pages.DAMAGE.getPageName().toUpperCase());
        damageBtn.addSelectionListener(new E19Selection());
        riverStageBtn = new Button(comp, SWT.CHECK);
        riverStageBtn.setText(E19Pages.RIVER_STAGE_DATA.getPageName().toUpperCase());
        riverStageBtn.addSelectionListener(new E19Selection());
        contactsBtn = new Button(comp, SWT.CHECK);
        contactsBtn.setText(E19Pages.CONTACTS.getPageName().toUpperCase());
        contactsBtn.addSelectionListener(new E19Selection());
        
        Label seperator2=new Label(comp,SWT.NONE);
        seperator2.setText("===========================");

        allPageBtn = new Button(comp, SWT.CHECK);
        allPageBtn.setText("ALL PAGES");
        shell.getDisplay();
		allPageBtn.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW));
        allPageBtn.addSelectionListener(new E19Selection());
        allPageBtn.setSelection(true);

        Label seperator3=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
        seperator3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

	}
	
	/**
	 * Get print data for selected contents from the report
	 */
	protected String getPrintData () {
		String text="";
		if (allPageBtn.getSelection()) {
			text = report.getText(E19Report.E19_ALLPAGES);
		}else {
		    if (converBtn.getSelection()) {
		    	text += report.getText(E19Report.E19_COVER);
		    }
		    if (mapBtn.getSelection()){
		    	text += report.getText(E19Report.E19_MAPPAGE);
		    }
		    if (benchmarkBtn.getSelection()){
		    	text += report.getText(E19Report.E19_BENCHMARKS);
		    }
		    if (gagesBtn.getSelection()){
		    	text += report.getText(E19Report.E19_GAGES);
		    }
		    if (historyBtn.getSelection()){
		    	text += report.getText(E19Report.E19_HISTORY);
		    }
		    if (crestsBtn.getSelection()){
		    	text += report.getText(E19Report.E19_CRESTS);
		    }
		    if (lowWaterBtn.getSelection()){
		    	text += report.getText(E19Report.E19_LOWWATER);
		    }
		    if (conditionsBtn.getSelection()){
		    	text += report.getText(E19Report.E19_CONDITIONS);
		    }
		    if (damageBtn.getSelection()){
		    	text += report.getText(E19Report.E19_DAMAGE);
		    }
		    if (riverStageBtn.getSelection()){
		    	text += report.getText(E19Report.E19_STAFFGAGE);
		    }
		    if (contactsBtn.getSelection()){
		    	text += report.getText(E19Report.E19_CONTACTS);
		    }
		}
		return text.replace("null", "");
	}
	
	/**
	 * Class for the table of contents clicks
	 * @author wkwock
	 *
	 */
	private class E19Selection extends SelectionAdapter{
		public void widgetSelected(SelectionEvent e) {
			Color yellow=Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW);
			Color black=Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
			Color red=Display.getCurrent().getSystemColor(SWT.COLOR_RED);
			
			Button button = (Button) e.getSource();
			if (button.equals(allPageBtn)) {//selected button is not allpage
			    mapBtn.setSelection(false);
			    benchmarkBtn.setSelection(false);
			    benchmarkBtn.setForeground(black);
			    gagesBtn.setSelection(false);
			    gagesBtn.setForeground(black);
			    historyBtn.setSelection(false);
			    historyBtn.setForeground(black);
			    crestsBtn.setSelection(false);
			    crestsBtn.setForeground(black);
			    lowWaterBtn.setSelection(false);
			    lowWaterBtn.setForeground(black);
			    conditionsBtn.setSelection(false);
			    conditionsBtn.setForeground(black);
			    damageBtn.setSelection(false);
			    damageBtn.setForeground(black);
			    riverStageBtn.setSelection(false);
			    contactsBtn.setSelection(false);
			    
			    if (allPageBtn.getSelection()) { //all page select to on
			    	allPageBtn.setForeground(yellow);
			    	converBtn.setSelection(false);
			    } else { //all page select to off
			    	converBtn.setSelection(true);
			    	allPageBtn.setForeground(black);
			    }
			}else {//selected button is not allpage
				allPageBtn.setSelection(false);
				allPageBtn.setForeground(black);
				
				if (button.getSelection()) {//seleted button is on
					if (button.equals(benchmarkBtn) || 
							button.equals(gagesBtn) ||
							button.equals(historyBtn) ||
							button.equals(crestsBtn) ||
							button.equals(lowWaterBtn) ||
							button.equals(conditionsBtn) ||
							button.equals(damageBtn) ) {
						button.setForeground(red);
					}
				} else {//seleted button is off
					button.setForeground(black);
					if (!converBtn.getSelection() && 
							!mapBtn.getSelection() &&
							!benchmarkBtn.getSelection() &&
							!gagesBtn.getSelection() &&
							!historyBtn.getSelection() &&
							!crestsBtn.getSelection() &&
							!lowWaterBtn.getSelection() &&
							!conditionsBtn.getSelection() &&
							!damageBtn.getSelection() &&
							!riverStageBtn.getSelection() &&
							!contactsBtn.getSelection() ) { //every thing is off then allpage must be on
						allPageBtn.setSelection(true);
						allPageBtn.setForeground(yellow);
					}
				}
			}
        }
	}
}
