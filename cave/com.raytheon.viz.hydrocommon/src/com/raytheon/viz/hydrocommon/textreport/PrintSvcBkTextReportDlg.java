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
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
/**
 * print menu for report Service backup.
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

public class PrintSvcBkTextReportDlg extends PrintTextReportDlg {
private ServiceBackupReport report=null;
private TextReportDlg textReportDlg=null;
	
	protected PrintSvcBkTextReportDlg(Shell parentShell, ServiceBackupReport rpt) {
		super(parentShell, rpt);
		this.report=rpt;
		this.setText("Service Backup - Print"); //dialog title
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
        header.setText("   Service Backup Report, sort by:");
        
        Label seperator1=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL);
        seperator1.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));
        
        Label singlePageLbl = new Label(comp, SWT.None);
        singlePageLbl.setText("Service Backup Information\nfor All stations");
        Label seperator2=new Label(comp,SWT.NONE);
        seperator2.setText("===========================");

        Label seperator3=new Label(comp, SWT.SEPARATOR | SWT.HORIZONTAL | SWT.BOLD);
        seperator3.setLayoutData(new GridData(GridData.FILL_HORIZONTAL));

	}
	
	/**
	 * Get print data for selected contents from the report
	 */
	protected String getPrintData () {
		return report.getText(textReportDlg.getPageSelectionIndex());
	}

	public void setTextReportDlg(TextReportDlg txtRptDlg) {
		this.textReportDlg=txtRptDlg;
	}
}
