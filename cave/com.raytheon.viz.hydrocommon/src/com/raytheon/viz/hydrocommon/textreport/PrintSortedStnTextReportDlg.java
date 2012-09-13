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

import com.raytheon.viz.hydrocommon.textreport.TextReportConstants.StationListSort;
/**
 * print menu for Sorted Station text report.
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

public class PrintSortedStnTextReportDlg extends PrintTextReportDlg {
	private StationListReport report=null;
	TextReportDlg textReportDlg;
	
    Button nameBtn = null;
    Button locationID = null;
    Button countyBtn = null;
    Button basinBtn = null;
    Button observerBtn = null;
    Button allPageBtn = null;
	
	protected PrintSortedStnTextReportDlg(Shell parentShell, StationListReport rpt) {
		super(parentShell, rpt);
		this.report=rpt;
		this.setText("Sorted Station List - Print");
	}
	
	public void setTextReportDlg(TextReportDlg txtRptDlg) {
		textReportDlg = txtRptDlg;
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
        header.setText("   Sorted Station List Report, sort by:");
        
        Label seperator1=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        seperator1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        locationID = new Button(comp, SWT.CHECK);
        locationID.setText(StationListSort.LOCATION_ID.getStationSortType().toUpperCase());
        locationID.addSelectionListener(new PrintSelection());
        nameBtn = new Button(comp, SWT.CHECK);
        nameBtn.setText(StationListSort.NAME.getStationSortType().toUpperCase());
        nameBtn.addSelectionListener(new PrintSelection());
        countyBtn = new Button(comp, SWT.CHECK);
        countyBtn.setText(StationListSort.COUNTY.getStationSortType().toUpperCase());
        countyBtn.addSelectionListener(new PrintSelection());
        basinBtn = new Button(comp, SWT.CHECK);
        basinBtn.setText(StationListSort.BASIN.getStationSortType().toUpperCase());
        basinBtn.addSelectionListener(new PrintSelection());
        observerBtn = new Button(comp, SWT.CHECK);
        observerBtn.setText(StationListSort.OBSERVER.getStationSortType().toUpperCase());
        observerBtn.addSelectionListener(new PrintSelection());
        
        Label seperator2=new Label(comp,SWT.NONE);
        seperator2.setText("===========================");

        allPageBtn = new Button(comp, SWT.CHECK);
        allPageBtn.setText("(ALL TYPES)");
        shell.getDisplay();
		allPageBtn.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW));
        allPageBtn.addSelectionListener(new PrintSelection());
        allPageBtn.setSelection(true);

        Label seperator3=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
        seperator3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

	}
	
	/**
	 * Get print data for selected contents from the report
	 */
	protected String getPrintData () {
		int sortIndex=textReportDlg.getPageSelectionIndex();
		int[] sortArray=report.getSortArray(sortIndex);
		
		if (!allPageBtn.getSelection()) {
		for (int i=0; i<sortArray.length; i++) {
			switch (sortArray[i]){
		case StationListReport.STALIST_LID:
			if (!locationID.getSelection())
				sortArray[i]=0;
            break;
        case StationListReport.STALIST_NAM:
        	if (!nameBtn.getSelection())
        		sortArray[i]=0;
            break;
        case StationListReport.STALIST_COU:
        	if (!countyBtn.getSelection())
        		sortArray[i]=0;
            break;
        case StationListReport.STALIST_BAS:
        	if (!basinBtn.getSelection())
        		sortArray[i]=0;
            break;
        case StationListReport.STALIST_OBS:
        	if (!this.observerBtn.getSelection())
        		sortArray[i]=0;
            break;
        }
		}
		}
		
		String text="";
		text =  report.generateSpecialReport(sortIndex,sortArray) ;
		text = text.replace("null", "");

		return text;
	}
	
	/**
	 * Responses to the table of contents clicks
	 * @author wkwock
	 *
	 */
	private class PrintSelection extends SelectionAdapter{
		public void widgetSelected(SelectionEvent e) {
			Color yellow=Display.getCurrent().getSystemColor(SWT.COLOR_YELLOW);
			Color black=Display.getCurrent().getSystemColor(SWT.COLOR_BLACK);
			Color red=Display.getCurrent().getSystemColor(SWT.COLOR_RED);
			
			Button button = (Button) e.getSource();
			if (button.equals(allPageBtn)) {//selected button is not allpage
			    nameBtn.setSelection(false);
			    nameBtn.setForeground(black);
			    countyBtn.setSelection(false);
			    countyBtn.setForeground(black);
			    basinBtn.setSelection(false);
			    basinBtn.setForeground(black);
			    observerBtn.setSelection(false);
			    
			    if (allPageBtn.getSelection()) { //all page select to on
			    	allPageBtn.setForeground(yellow);
			    	locationID.setSelection(false);
			    } else { //all page select to off
			    	locationID.setSelection(true);
			    	allPageBtn.setForeground(black);
			    }
			}else {//selected button is not allpage
				allPageBtn.setSelection(false);
				allPageBtn.setForeground(black);
				
				if (button.getSelection()) {//selected button is on
					if (button.equals(countyBtn) || 
							button.equals(basinBtn) ||
							button.equals(nameBtn) ) {
						button.setForeground(red);
					}
				} else {//selected button is off
					button.setForeground(black);
					if (!nameBtn.getSelection() && 
							!locationID.getSelection() &&
							!countyBtn.getSelection() &&
							!basinBtn.getSelection() &&
							!observerBtn.getSelection()) { //every thing is off then allpage must be on
						allPageBtn.setSelection(true);
						allPageBtn.setForeground(yellow);
					}
				}
			}
        }
	}
}
