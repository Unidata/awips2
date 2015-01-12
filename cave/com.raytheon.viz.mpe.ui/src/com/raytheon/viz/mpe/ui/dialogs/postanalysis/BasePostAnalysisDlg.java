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
package com.raytheon.viz.mpe.ui.dialogs.postanalysis;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.ui.dialogs.CaveSWTDialog;

/**
 * This is the base dialog class for the Post Analysis dialogs.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public abstract class BasePostAnalysisDlg extends CaveSWTDialog {

    /**
     * Maps composite.
     */
    protected MapsComp mapsComp = null;
    protected Label leftMapLbl = null;
    protected Label rightMapLbl = null;

    /**
     * Legend canvas.
     */
    protected Canvas legendCanvas = null;
    protected ColorLegendMgr colorLegendMgr = null;

    /**
     * Canvas height.
     */
    protected int canvasHeight = 75;

    /**
     * Canvas width.
     */
    protected int canvasWidth = 700;

    
    /**
     * Data that belongs to the internal maps
     */
    private String dataFileName1 = null;
    private String dataFileName2 = null;
    
    private float[] dataArray1 = null;
    private float[] dataArray2 = null;
    
    private java.awt.Rectangle extent1 = null;
    private java.awt.Rectangle extent2 = null;
    
    private NamedColorUseSet namedColorUseSet1 = null;
    private NamedColorUseSet namedColorUseSet2 = null;
    
    protected abstract NamedColorUseSet createNamedColorUseSet1();
    protected abstract NamedColorUseSet createNamedColorUseSet2();
   
    private PAResourceType resourceType1 = PAResourceType.XMRG;
    private PAResourceType resourceType2 = PAResourceType.XMRG;
    
    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public BasePostAnalysisDlg(Shell parentShell) {
        super(parentShell, SWT.DIALOG_TRIM | SWT.RESIZE, CAVE.INDEPENDENT_SHELL | CAVE.DO_NOT_BLOCK);
        
    }

    @Override
    protected Layout constructShellLayout() {
        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 0;
        mainLayout.marginWidth = 2;
        mainLayout.verticalSpacing = 2;
        return mainLayout;
    }

    @Override
    protected Object constructShellLayoutData() {
        return new GridData(SWT.FILL, SWT.DEFAULT, true, false);
    }

    @Override
    protected void disposed() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.viz.ui.dialogs.CaveSWTDialogBase#initializeComponents(org
     * .eclipse.swt.widgets.Shell)
     */
    @Override
    protected void initializeComponents(Shell shell) {
        createMenus();
        createMapLabels();
        createMapComposite();
        createLegendCanvas();
        addBottomControls();
    }

    /**
     * Create the menubar and the menu items.
     */
    public void createMenus() {
        Menu menuBar = new Menu(shell, SWT.BAR);

        createControlMenu(menuBar);
        createOptionsMenu(menuBar);
        createOverlaysMenu(menuBar);

        shell.setMenuBar(menuBar);
    }

    /**
     * Create the Control menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createControlMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Control menu
        // -------------------------------------
        MenuItem controlMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        controlMenuItem.setText("&Control");

        // Create the Control menu item with a Control "dropdown" menu
        Menu controlMenu = new Menu(menuBar);
        controlMenuItem.setMenu(controlMenu);

        /*
         * Create the custom control menu items
         */
        createControlMenuItem(controlMenu);

        /*
         * Create all the items in the Control dropdown menu
         */
        MenuItem closeMI = new MenuItem(controlMenu, SWT.NONE);
        closeMI.setText("Close");
        closeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                shell.dispose();
            }
        });
    }

    /**
     * Create the Options menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOptionsMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Options menu
        // -------------------------------------
        MenuItem optionsMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        optionsMenuItem.setText("&Options");

        // Create the Options menu item with a Options "dropdown" menu
        Menu optionsMenu = new Menu(menuBar);
        optionsMenuItem.setMenu(optionsMenu);

        /*
         * Create all the items in the Options dropdown menu
         */
        MenuItem zoomResetMI = new MenuItem(optionsMenu, SWT.NONE);
        zoomResetMI.setText("Zoom Reset");
        zoomResetMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                mapsComp.resetZoom();
            }
        });

        MenuItem samplingMI = new MenuItem(optionsMenu, SWT.CHECK);
        samplingMI.setText("Sampling");
        samplingMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {

            }
        });
    }

    /**
     * Create the Overlay menu.
     * 
     * @param menuBar
     *            Menu bar.
     */
    private void createOverlaysMenu(Menu menuBar) {
        // -------------------------------------
        // Create the Overlays menu
        // -------------------------------------
        MenuItem overlaysMenuItem = new MenuItem(menuBar, SWT.CASCADE);
        overlaysMenuItem.setText("O&verlays");

        // Create the Overlays menu item with a Overlays "dropdown" menu
        Menu overlaysMenu = new Menu(menuBar);
        overlaysMenuItem.setMenu(overlaysMenu);

        /*
         * Create all the items in the Overlays dropdown menu
         */
        MenuItem statesMI = new MenuItem(overlaysMenu, SWT.CHECK);
        statesMI.setText("States");
        statesMI.setSelection(true);
        statesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem countyMI = new MenuItem(overlaysMenu, SWT.CHECK);
        countyMI.setText("County");
        countyMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem citiesTownsMI = new MenuItem(overlaysMenu, SWT.CHECK);
        citiesTownsMI.setText("Cities/Towns");
        citiesTownsMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem basinBoundariesMI = new MenuItem(overlaysMenu, SWT.CHECK);
        basinBoundariesMI.setText("Basin Boundaries");
        basinBoundariesMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem riversMI = new MenuItem(overlaysMenu, SWT.CASCADE);
        riversMI.setText("Rivers");

        Menu riversSubMenu = new Menu(shell, SWT.DROP_DOWN);
        riversMI.setMenu(riversSubMenu);

        MenuItem allRiversMI = new MenuItem(riversSubMenu, SWT.CHECK);
        allRiversMI.setText("All Rivers");
        allRiversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });

        MenuItem majorRiversMI = new MenuItem(riversSubMenu, SWT.CHECK);
        majorRiversMI.setText("Major Rivers");
        majorRiversMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                toggleOverlay((MenuItem) event.getSource());
            }
        });
    }

    /**
     * Create the labels that will appear above the maps.
     */
    private void createMapLabels() {
        String[] lblNames = getMapLabelNames();

        if (lblNames == null || lblNames.length == 0) {
            return;
        }

        Composite labelComp = new Composite(shell, SWT.NONE);
        labelComp.setLayout(new GridLayout(2, true));
        labelComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        leftMapLbl = new Label(labelComp, SWT.CENTER);
        leftMapLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        rightMapLbl = new Label(labelComp, SWT.CENTER);
        rightMapLbl.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        if (lblNames.length >= 2) {
            leftMapLbl.setText(lblNames[0]);
            rightMapLbl.setText(lblNames[1]);
        } else {
            leftMapLbl.setText(lblNames[0]);
            rightMapLbl.setText("Unknown");
        }
    }

    /**
     * Create the map composite.
     */
    private void createMapComposite() {
    	System.out.println(" BasePostAnalysisDl:createMapComposite(): ");
        mapsComp = new MapsComp(shell, this);
    }


    /**
     * Create the color bar legend canvas.
     */
    private void createLegendCanvas() {
        Composite canvasComp = new Canvas(shell, SWT.NONE);
        canvasComp.setLayout(new GridLayout(1, false));
        canvasComp.setLayoutData(new GridData(SWT.CENTER, SWT.DEFAULT, true, false));

        canvasHeight = canvasHeight * getNumberOfColorLegends();

        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = canvasWidth;
        gd.heightHint = canvasHeight;
        legendCanvas = new Canvas(canvasComp, SWT.DOUBLE_BUFFERED | SWT.BORDER);
        legendCanvas.setSize(canvasWidth, canvasHeight);
        legendCanvas.setLayoutData(gd);

        //let the colorLegendMgr handle the actual painting
        //The ColorLegendMgr sets up its own PaintListener()
        setColorLegendMgr(createColorLegendMgr(legendCanvas));

        /* 
        legendCanvas.addPaintListener(new PaintListener() {
            public void paintControl(PaintEvent e) {
                drawCanvas(e.gc);
            }
        });
        */
        
        
        
        
    }

    /**
     * Toggle the map overlay.
     * 
     * @param mi
     *            Menu item selected.
     */
    private void toggleOverlay(MenuItem mi) {
        mapsComp.toggleOverlay(mi.getSelection(), mi.getText());
    }

    /**
     * Draw the legend canvas.
     * 
     * @param gc
     */
    private void drawCanvas(GC gc)
    {
    	getColorLegendMgr().paintLegend(gc);
    	
    //    gc.setAntialias(SWT.ON);
    //    gc.setBackground(getDisplay().getSystemColor(SWT.COLOR_BLACK));

    //    gc.fillRectangle(0, 0, canvasWidth, canvasHeight);
    }

    /**
     * Create custom menu items for the control menu.
     * 
     * @param controlMenu
     *            The control menu.
     */
    abstract protected void createControlMenuItem(Menu controlMenu);

    /**
     * Get the names for the left and right map labels.
     * 
     * @return String aray of names.
     */
    abstract protected String[] getMapLabelNames();

    /**
     * This will allow classes extending this class to add controls/composites
     * at the bottom of the dialog.
     */
    abstract protected void addBottomControls();

    /**
     * Get the number of color bars to determine the height of the color legend.
     * 
     * @return Number of color legend color bars.
     */
    abstract protected int getNumberOfColorLegends();
    
   
	private ColorLegendMgr createColorLegendMgr(Canvas canvas)
	{
		NamedColorUseSet namedColorUseSet1 = getNamedColorUseSet1();
    	NamedColorUseSet namedColorUseSet2 =  getNamedColorUseSet2();
	
    	Date selectedDateTime = PostAnalysisManager.getSelectedDate();
    	
    	String dateTimeString = "ending at " + getDateTimeStringFromLongTime(selectedDateTime.getTime());
    	
    	ColorLegendMgr legendMgr = new ColorLegendMgr(canvas, namedColorUseSet1,
    												  namedColorUseSet2, dateTimeString);

    	return legendMgr;
	}
	
	private static String getStringFromLongTime(long time, String dateFormat)
	{
	    String timeString  = null;
	
		//System.out.println("timeString = !" + timeString + "!");
		SimpleDateFormat utcSdf2 = new SimpleDateFormat(dateFormat);
		utcSdf2.setTimeZone(TimeZone.getTimeZone("UTC"));
		timeString = utcSdf2.format(new java.util.Date(time));
	
		return timeString;
	}
	
	protected static String getDateTimeStringFromLongTime(long time)
	{
		String timeString  = getStringFromLongTime(time, "yyyy-MM-dd HHz");
	
		return timeString;
	}


	protected void setDataFileName1(String dataFileName1) {
		this.dataFileName1 = dataFileName1;
	}

	protected String getDataFileName1() {
		return dataFileName1;
	}

	protected void setDataFileName2(String dataFileName2) {
		this.dataFileName2 = dataFileName2;
	}

	protected String getDataFileName2() {
		return dataFileName2;
	}

	protected void setDataArray1(float[] dataArray1) {
		this.dataArray1 = dataArray1;
	}

	protected float[] getDataArray1() {
		return dataArray1;
	}

	protected void setDataArray2(float[] dataArray2) {
		this.dataArray2 = dataArray2;
	}

	protected float[] getDataArray2() {
		return dataArray2;
	}

	protected void setExtent1(java.awt.Rectangle extent1) {
		this.extent1 = extent1;
	}

	protected java.awt.Rectangle getExtent1() {
		return extent1;
	}

	protected void setExtent2(java.awt.Rectangle extent2) {
		this.extent2 = extent2;
	}

	protected java.awt.Rectangle getExtent2() {
		return extent2;
	}
	
	
	public NamedColorUseSet getNamedColorUseSet1() {
	
		if (namedColorUseSet1 == null)
		{
			namedColorUseSet1 = createNamedColorUseSet1();
		}
		return namedColorUseSet1;
	}

	public NamedColorUseSet getNamedColorUseSet2() {
		
		if (namedColorUseSet2 == null)
		{
			namedColorUseSet2 = createNamedColorUseSet2();
		}
		return namedColorUseSet2;
	}

	protected void setColorLegendMgr(ColorLegendMgr colorLegendMgr) {
		this.colorLegendMgr = colorLegendMgr;
	}

	public ColorLegendMgr getColorLegendMgr() {
	
		return colorLegendMgr;
	}
	
	public PAResourceType getResourceType1() {
		return resourceType1;
	}
	public void setResourceType1(PAResourceType resourceType1) {
		this.resourceType1 = resourceType1;
	}
	public PAResourceType getResourceType2() {
		return resourceType2;
	}
	public void setResourceType2(PAResourceType resourceType2) {
		this.resourceType2 = resourceType2;
	}



}
