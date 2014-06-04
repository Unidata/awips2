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

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;

/**
 * Merged and Grid Bias Fields dialog.
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

public class MergedGridBiasDlg extends BasePostAnalysisDlg {

    /**
     * Best Estimate dialog.
     */
	
	java.awt.Rectangle extent = null;
	
    private BestEstimate1HrQpeDlg bestEstimateDlg = null;
    
    private double[][] mergeArray = null;
    private double[][] ratioArray = null;
    private double[][] disaggArray = null;
    
    private boolean showMergedData = true;
    
    
    private Combo fieldTypeCBO = null;
    private static final String MERGED_FIELD = "Merged Field";
    private static final String DISAGG_FIELD = "Disaggregated Field";
    
    public MergedGridBiasDlg(Shell parentShell) {
        super(parentShell);
        
        setResourceType1(PAResourceType.XMRG);
        setResourceType2(PAResourceType.XMRG);

        
        setText("Merged and Grid Bias Fields");

        PostAnalysisManager paMgr = new PostAnalysisManager();
        
        try
        {
        	
        	String filePath = PostAnalysisManager.get24HourGageOnlyFilePath();
        	
        	//units are hundredths of inches
       // 	double[][] gageOnly24HourGrid = paMgr.createGridArray(filePath, false, true, false); 

        	double[][] gageOnly24HourGrid = paMgr.readAsciiXmrgGridData(filePath, false, true, false);
        	
        	mergeArray = paMgr.create2DGridArray();
        	ratioArray = paMgr.create2DGridArray();
        	disaggArray = paMgr.create2DGridArray();
    	
        	float rhat = paMgr.getRhat();
        	//rhat = 0.5f;
        	
        	float estimatedScale = paMgr.getEstimatedScale();
                	
        	//paMgr.printDouble2DArray(qpeAccum24Hr[DailyQcUtils.pcpn_day]);
        	
          	Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        	int maxCols = hrap_grid.maxi;
    		int maxRows = hrap_grid.maxj;
    	
    		double scaleFactor = 1.0;
    		
        	//units are Inches
        	double[][] accum24HourGrids  =
        				PostAnalysisManager.get24HourTotalPrecip(maxRows, maxCols, scaleFactor);
        	
        	paMgr.mergeData(accum24HourGrids, gageOnly24HourGrid, 
        					mergeArray, ratioArray, 
        					disaggArray, estimatedScale, rhat);
        	
      // 	System.out.println(header + "ratioArray = ");
      //  	paMgr.printDouble2DArray(ratioArray[DailyQcUtils.pcpn_day]);

        	//set up merged field      	
        	     	
        	float[] dataArray1 = null;
         	
        	
        	if (showMergedData)
        	{
        		dataArray1 =  getSingleDataArrayFrom2DArray(mergeArray);
        	}
        	else //show disagged Data
        	{
        		dataArray1 = getSingleDataArrayFrom2DArray(disaggArray);
        	}
        	
        	setExtent1(paMgr.getExtent());
        //	paMgr.printFloatArray(header +  dataArray1);    	
        	setDataArray1(dataArray1);

        	//set up grid bias (ratio) field  	
        	
        	float[] dataArray2 = paMgr.convertToSingleArray(ratioArray, false, true); 
        	setExtent2(paMgr.getExtent());
        	setDataArray2(dataArray2);
    	     //paMgr.printFloatArray(header +  dataArray2);
        }

        catch (Throwable t)
        {
        	t.printStackTrace();
        }
        
    }

  
    
    private float[] getSingleDataArrayFrom2DArray(double[][] gridArray)
    {
    	PostAnalysisManager paMgr = new PostAnalysisManager();
    	
    	float[] dataArray = paMgr.convertToSingleArray(gridArray, false, true);
    //	float[] dataArray = paMgr.convertToSingleArray(gridArray, false, false);
    	return dataArray;
    }
    
    
	/*
     * Create the Display 1hr QPE Fields menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {
        /*
         * Create custom items in the Control dropdown menu
         */
        MenuItem displayQpeMI = new MenuItem(controlMenu, SWT.NONE);
        displayQpeMI.setText("Display 1hr QPE Fields");
        displayQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                display1HrQpeAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "Merged Field", "Grid Bias Field" };
        return names;
    }

    private void display1HrQpeAction() {
        if (bestEstimateDlg == null || bestEstimateDlg.isDisposed() == true) {
            Shell platformShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
         	
        	double[][] dailyRatioArray = ratioArray;
        	double[][] dailyDisaggArray = disaggArray;
        	
            bestEstimateDlg = new BestEstimate1HrQpeDlg(platformShell, dailyRatioArray, dailyDisaggArray);
            bestEstimateDlg.open();
        }
    }

    /**
     * Not used
     */
    @Override
    protected void addBottomControls() {
    	  GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
          gd.widthHint = 300;
          fieldTypeCBO = new Combo(shell, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
          fieldTypeCBO.setLayoutData(gd);
          
         
          fieldTypeCBO.add("Merged Field");        
          fieldTypeCBO.add("Disaggregated Field");
          
          fieldTypeCBO.addSelectionListener(new SelectionAdapter() 
          {
          	public void widgetSelected(SelectionEvent event) {
         		
          		String fieldType = fieldTypeCBO.getText();

          		if (fieldType.equals(MERGED_FIELD))
          		{
          			//System.out.println("addBottomControls().widgetSelected() - MERGED FIELD CHOSEN");
          			showMergedData = true;
          		    leftMapLbl.setText(MERGED_FIELD);
          			setDataArray1(getSingleDataArrayFrom2DArray(mergeArray));
          		}
          		else
          		{
          			//System.out.println("addBottomControls().widgetSelected() - DISAGGED FIELD CHOSEN");
          			//System.out.println("fieldType = " + fieldType);
          			
          			
          			showMergedData = false;
          		    leftMapLbl.setText(DISAGG_FIELD);
          		    
          		    if (disaggArray != null)
          		    {
          		        setDataArray1(getSingleDataArrayFrom2DArray(disaggArray));
          		    }
          		    else
          		    {
          		        //System.out.println("disaggArray is null!!!!");
          		        setDataArray1(null);
          		    }
          		}

          		loadImage(event.data);
          	}
     	
          } );

          //make an initial selection
          fieldTypeCBO.select(0);
       
    }
    
 private void loadImage(Object data) {
		
    	
		//refresh the maps
		mapsComp.refresh();
	}

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.mpe.ui.dialogs.postanalysis.BasePostAnalysisDlg#
     * getNumberOfColorLegends()
     */
    @Override
    protected int getNumberOfColorLegends() {
        return 2;
    }

	@Override
	protected NamedColorUseSet createNamedColorUseSet1() 
	{
		
		return PostAnalysisManager.getNamedColorUseSet("PRECIP_ACCUM");
	}


	@Override
	protected NamedColorUseSet createNamedColorUseSet2() {
		
		return PostAnalysisManager.getNamedColorUseSet("PRECIP_RATIO");
		
		//NamedColorUseSet namedColorUseSet2 = TestDriver.getNamedColorUseSetForBias();
		//return namedColorUseSet2;
	}

}
