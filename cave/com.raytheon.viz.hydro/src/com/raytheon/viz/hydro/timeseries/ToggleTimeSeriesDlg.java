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
package com.raytheon.viz.hydro.timeseries;

import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydro.CaveHydroSWTDialog;
import com.raytheon.viz.hydro.timeseries.util.TraceData;

/**
 * This class is the popup menu when right/middle click on the Time Series Display.
 * There should be only one toggle time series dialog at a time.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       Ticket# Engineer    Description
 * ---------- ------- ----------- --------------------------
 * 1/16/2013  15695   wkwock      Initial                         
 * @author wkwock
 * @version 1.0
 * 
 */
public class ToggleTimeSeriesDlg extends CaveHydroSWTDialog {
	private static final String DLG_TITLE="Toggle Time Series";
	private List<TraceData> traceList=null;
	private TimeSeriesDisplayCanvas tsDisCanvas=null;
	private static ToggleTimeSeriesDlg oneTTSD=null; //singleton
	
	public static ToggleTimeSeriesDlg getInstance(Shell parentShell,List<TraceData> traceLst, TimeSeriesDisplayCanvas tsdc) {
		if (oneTTSD!=null){
			oneTTSD.close();
			oneTTSD.disposed();
		}
		oneTTSD = new ToggleTimeSeriesDlg(parentShell, traceLst, tsdc) ;
		return oneTTSD;
	}
	
	private ToggleTimeSeriesDlg(Shell parentShell,List<TraceData> traceLst, TimeSeriesDisplayCanvas tsdc) {
		super(parentShell);
		this.setText(DLG_TITLE);
		
		this.traceList=traceLst;
		this.tsDisCanvas=tsdc;
	}

	@Override
	protected void initializeComponents(Shell shell) {
	       Composite beginningTimeComp = new Composite(shell, SWT.NONE);
	        RowLayout topLabelCompRl = new RowLayout();
	        beginningTimeComp.setLayout(topLabelCompRl);

	        Button[] checkBtns = new Button[traceList.size()];
	    	
	        for (int i = 0; i < traceList.size(); i++) {
	            TraceData td = traceList.get(i);
		    	checkBtns[i] = new Button(shell, SWT.CHECK);
	            String s = null;
	            if (td.isForecast()) {
	                s = this.tsDisCanvas.getFcstPEDTSE(td);
	            } else {
	                s = this.tsDisCanvas.getPEDTSE(td);
	            }
	            if (td.getLineData()!=null && td.getLineData().length>0) {
	                if (td.isTraceOn())
	                    checkBtns[i].setSelection(true);
	                else
	                	checkBtns[i].setSelection(false);
	            } else {
	            	checkBtns[i].setSelection(false);
	                s = s.concat("" + "NO DATA");
	            }

		    	checkBtns[i].setText(s);
		    	checkBtns[i].setLocation(50*i,250);
		    	checkBtns[i].pack();
		    	checkBtns[i].setData(td);
	            
	            
		    	checkBtns[i].addListener(SWT.Selection, new Listener() {
	                public void handleEvent(Event event) {
	                    handleSelection(event);
	                }
	            });
	        }
	        // We need to make the menu visible
	        beginningTimeComp.setVisible(true);
	    }

	    private void handleSelection(Event event) {
	    	this.tsDisCanvas.handleSelection(event);
	    }
}
