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
package com.raytheon.uf.viz.monitor.ui.dialogs;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.TabFolder;

import com.raytheon.uf.common.monitor.data.ObConst.DataUsageKey;
import com.raytheon.uf.viz.monitor.data.MonitorAreaThresholds;
import com.raytheon.uf.viz.monitor.thresholds.AbstractThresholdMgr.ThresholdKey;

/**
 * Abstract class is the foundation for a Tab Folders tab item control (Composite).
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 6, 2009            lvenable     Initial creation
 * Aug 5, 2010  6396       wkwock      Change the layout of threshold edit dialog
 * Nov 7, 2013  DR 16703   gzhang	   Check in code for Lee for FFMP and Safeseas
 * 
 * </pre>
 *
 * @author lvenable
 * @version 1.0
 */
public abstract class TabItemComp extends Composite
{
    /**
     * Parent tabl folder.
     */
    private TabFolder parent;
    
    private boolean involveSwell = false;
    protected boolean rankSwellPeriodHigh = false;
    /**
     * Data list control.
     */
    protected List dataList;
    
    /**
     * Small font.
     */
    protected Font smFont;
    
    /**
     * Big font.
     */
    protected Font bigFont;
    
    /**
     * Edit button.
     */
    private Button editBtn;
    
    /**
     * List composite.
     */
    private Composite listComp;
    
    protected DataUsageKey duKey;
    protected ThresholdKey threshKeyR = ThresholdKey.RED;
    protected ThresholdKey threshKeyY = ThresholdKey.YELLOW;
    
    protected String dataFmt = " %5s";
    protected String areaIdFmt = "%6S   ";
    
    /**
     * Constructor.
     * @param parent Parent - tab folder.
     */
    public TabItemComp(TabFolder parent, DataUsageKey duKey)
    {
        super(parent, 0);
        
        this.parent = parent;
        
        this.duKey = duKey;
        
        init();
    }
    
    public TabItemComp(TabFolder parent, DataUsageKey duKey, Boolean involveSwell) {
    	super(parent, 0);
    	this.parent = parent; 
    	this.duKey = duKey; 
    	this.involveSwell = involveSwell;
    	init();
    }
    
    /**
     * Initialize method to setup the canvas and the fonts.
     */
    private void init()
    {
        smFont = new Font(this.getDisplay(), "Monospace", 9, SWT.NORMAL);
        bigFont = new Font(this.getDisplay(), "Monospace", 10, SWT.NORMAL);
        
        GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        GridLayout gl = new GridLayout(1, false);
        gl.verticalSpacing = 2;
        gl.marginHeight = 2;
        gl.marginWidth = 2;
        this.setLayout(gl);
        this.setLayoutData(gd);
        
        createDataList();
        if ( involveSwell ) {
        	createRankSwellPeriodHighLowRadios();
        }
        createControlButtons();
        
        this.addDisposeListener(new DisposeListener()
        {
            public void widgetDisposed(DisposeEvent arg0)
            {
                smFont.dispose();
                bigFont.dispose();
            }
        });
    }
    
    private void createRankSwellPeriodHighLowRadios() {
    	GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
		Composite radioComp = new Composite(this, SWT.NONE); 
		radioComp.setLayout(new GridLayout(1,false));
		radioComp.setLayoutData(gd);
		
		Button rankLowRdo = new Button(radioComp, SWT.RADIO); 
		rankLowRdo.setText("Rank Swell Period Low");
		if ( !MonitorAreaThresholds.isRankHighSwellPeriods() ) {
			rankLowRdo.setSelection(true);
		} else {
			rankLowRdo.setSelection(false);
		}
		rankLowRdo.addSelectionListener( new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event){
                rankSwellPeriodHigh = false;
                populateList();
            }
		});
		
		Button rankHighRdo = new Button(radioComp, SWT.RADIO); 
		rankHighRdo.setText("Rank Swell Period High");
		if ( MonitorAreaThresholds.isRankHighSwellPeriods() ) {
			rankHighRdo.setSelection(true);
		} else {
			rankHighRdo.setSelection(false);
		}
		rankHighRdo.addSelectionListener( new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event){
                rankSwellPeriodHigh = true;
                populateList();
            }
		});
		
	}

	/**
     * Create the threshold data list control.
     */
    private void createDataList()
    {
        listComp = new Composite(this, SWT.NONE);
        GridLayout gl = new GridLayout(1, false);
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        listComp.setLayout(gl);
        listComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, false, false));
        
        createListHeader(listComp);
        
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.heightHint = 250;
        dataList = new List(listComp, SWT.BORDER | SWT.MULTI | SWT.V_SCROLL);
        dataList.setFont(bigFont); //dataList.setFont(smFont);
        dataList.setLayoutData(gd);
        
        populateList();
    }
    
    /**
     * Create the Select/Deselect/Edit control buttons.
     */
    private void createControlButtons()
    {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Composite mainButtonComp = new Composite(this, SWT.NONE);
        mainButtonComp.setLayout(new GridLayout(1, false));
        mainButtonComp.setLayoutData(gd);
        
        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        Composite buttonComp = new Composite(mainButtonComp, SWT.NONE);
        buttonComp.setLayout(new GridLayout(3, false));
        buttonComp.setLayoutData(gd);
        
        gd = new GridData(120, SWT.DEFAULT);
        Button selectAllBtn = new Button(buttonComp, SWT.PUSH);
        selectAllBtn.setText("Select All");
        selectAllBtn.setLayoutData(gd);
        selectAllBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                dataList.selectAll();
            }
        });
        
        gd = new GridData(120, SWT.DEFAULT);
        Button deselectAllBtn = new Button(buttonComp, SWT.PUSH);
        deselectAllBtn.setText("Deselect All");
        deselectAllBtn.setLayoutData(gd);
        deselectAllBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                dataList.deselectAll();
            }
        });
        
        gd = new GridData(170, SWT.DEFAULT);
        editBtn = new Button(buttonComp, SWT.PUSH);
        editBtn.setText("Edit Selected Areas...");
        editBtn.setLayoutData(gd);
        editBtn.addSelectionListener(new SelectionAdapter()
        {
            public void widgetSelected(SelectionEvent event)
            {
                if (dataList.getSelectionCount() == 0)
                {
                    MessageBox mb = new MessageBox(parent.getShell(), SWT.ICON_WARNING | SWT.OK);
                    mb.setText("Warning");
                    mb.setMessage("Please select data from the list to be edited.");
                    mb.open();
                    
                    return;
                }
                
                editDataAction();
            }
        });
    }
    
    /**
     * Create a composite that will contain groups data.
     * @param parentComp Parent composite.
     * @param cols Number of columns.
     * @param title Group title.
     * @return Composite to contain a group of thresholds.
     */
    protected Composite createGroupComposite(Composite parentComp, int cols, String title)
    {
        Composite dataGroupComp = new Composite(parentComp, SWT.BORDER);
        GridLayout gl = new GridLayout(cols, false);
        gl.horizontalSpacing = 0;
        gl.marginHeight = 0;
        gl.marginWidth = 0;
        dataGroupComp.setLayout(gl);
        
        if (title != null)
        {
            GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
            gd.horizontalSpan = cols;
            Label topLbl = new Label(dataGroupComp, SWT.CENTER);
            topLbl.setText(title);
            topLbl.setFont(bigFont);
            topLbl.setLayoutData(gd);
        }
        
        return dataGroupComp;
    }
    
    /**
     * Create a title label and the R/Y labels. 
     * @param parentComp Parent composite.
     * @param topStr Top label string.
     * @param bottomStr Bottom label string.
     * @param isVis Flag indicating if the label is for visibility.
     */
    protected void createLabelComp(Composite parentComp, String topStr, String bottomStr, boolean isVis)
    {
        GridData gd;
        
        if (bottomStr.indexOf("\n") == -1)
        {
            bottomStr += "\n";
        }
            
        
        if (isVis == true)
        {
            gd = new GridData(90, SWT.DEFAULT);
        }
        else
        {
            gd = new GridData(82, SWT.DEFAULT);
        }
        
        Composite lblComp = new Composite(parentComp, SWT.BORDER);
        GridLayout gl = new GridLayout(2, true);
        gl.marginWidth = 2;
        gl.horizontalSpacing = 2;
        lblComp.setLayout(gl);
        lblComp.setLayoutData(gd);
        
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label topLbl = new Label(lblComp, SWT.CENTER);
        topLbl.setText(topStr);
        topLbl.setFont(smFont);
        topLbl.setLayoutData(gd);
        
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.horizontalSpan = 2;
        Label bottomLbl = new Label(lblComp, SWT.CENTER);
        bottomLbl.setText(bottomStr);
        bottomLbl.setFont(smFont);
        bottomLbl.setLayoutData(gd);
        
        String colorChar="y";
        int color = SWT.COLOR_YELLOW;
        if (bottomStr.indexOf("from") == -1)
        {	colorChar="r";
        	color = SWT.COLOR_RED;
        }
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label rLbl = new Label(lblComp, SWT.CENTER);
        rLbl.setText(colorChar);
        rLbl.setBackground(getParent().getDisplay().getSystemColor(color));
        rLbl.setFont(smFont);
        rLbl.setLayoutData(gd);
        
        colorChar="r";
        color=SWT.COLOR_RED;
        if (bottomStr.indexOf("from") == -1)
        {
        	colorChar="y";
        	color=SWT.COLOR_YELLOW;
        }
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label yLbl = new Label(lblComp, SWT.CENTER);
        yLbl.setText(colorChar);
        yLbl.setBackground(getParent().getDisplay().getSystemColor(color));
        yLbl.setFont(smFont);
        yLbl.setLayoutData(gd);
    }
    
    /**
     * Pack the list controls.
     */
    protected void packListControls()
    {
        listComp.layout();
        listComp.pack();
        getParent().getShell().layout();
        getParent().getShell().pack();
    }
    
    /**
     * Format and append the red and yellow values to the string builder.
     * @param sb String builder.
     * @param rValue Red value.
     * @param yVal Yellow value.
     */
    protected void appendIntData(StringBuilder sb, double rValue, double yVal)
    {
        int intVal = (int)rValue;            
        sb.append(String.format(dataFmt, String.valueOf(intVal)));
        
        intVal = (int)yVal;            
        sb.append(String.format(dataFmt, String.valueOf(intVal)));
    }
    
    protected void appendDecimalData(StringBuilder sb, double rValue, double yVal)
    {       
        sb.append(String.format(dataFmt, String.valueOf(rValue)));
        
        sb.append(String.format(dataFmt, String.valueOf(yVal)));
    }
    
    /**
     * Create a list header.
     * @param comp Composite.
     */
    protected abstract void createListHeader(Composite comp);
    
    /**
     * Action for the edit data button.
     */
    protected abstract void editDataAction();
    
    /**
     * Populate the data list.
     */
    protected abstract void populateList();
    
    public abstract void reloadData();    
    public abstract void commitDataToXML();
}
