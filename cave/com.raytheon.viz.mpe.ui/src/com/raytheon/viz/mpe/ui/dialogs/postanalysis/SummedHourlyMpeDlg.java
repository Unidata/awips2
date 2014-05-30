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


import java.awt.Rectangle;

import java.util.List;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.dataplugin.shef.tables.Colorvalue;
import com.raytheon.viz.hydrocommon.HydroDisplayManager;
import com.raytheon.viz.hydrocommon.util.MPEColors;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.GetColorValues;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;
import com.raytheon.viz.mpe.util.DailyQcUtils;
import com.raytheon.viz.mpe.util.DailyQcUtils.Hrap_Grid;



/**
 * Summed Hourly MPE Field & Gage Only Field dialog.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 12, 2011            lvenable     Initial creation
 * December 2013           cgobs        Completion
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class SummedHourlyMpeDlg extends BasePostAnalysisDlg 
{

	private static final int SECONDS_PER_HOUR = 3600;
	private static final int SECONDS_PER_DAY = 24 * SECONDS_PER_HOUR;	
	
	   /** Bundle file location */
    //private static final String BUNDLE_LOC = "bundles/MPE/postAnalysisBundle.xml";

    /**
     * Merge Data dialog.
     */
    MergedGridBiasDlg mergeDataDlg = null;
  
    public SummedHourlyMpeDlg(Shell parentShell) {
        super(parentShell);

        setText("24 HR Summed Hourly MPE Field & 24-HR Gage Only Field");
      
        setResourceType1(PAResourceType.XMRG);
        setResourceType2(PAResourceType.ASCII_XMRG);
        
        // 24 accumulated  1-hour precip grids
      	
        Hrap_Grid grid = DailyQcUtils.getHrap_grid(); 
        int wfoMinX = grid.hrap_minx;
        int wfoMinY = grid.hrap_miny;
        int width = grid.maxi;
        int height = grid.maxj;

        //System.out.println(header + " wfoMinX = " + wfoMinX + " wfoMinY = " + wfoMinY + 
        //					" width = " + width + " height = " + height);
        java.awt.Rectangle extent = new Rectangle(wfoMinX, wfoMinY, width, height);
        setExtent1(extent);

        
        float scaleFactor = 25.4f * 100.0f;
        double[][] totalPrecipGrid = PostAnalysisManager.get24HourTotalPrecip(height, width, scaleFactor);
       
        
        //floatArray units are hundredths of MM
        float[] floatArray = convertToFloatArray(totalPrecipGrid, 1.0f);      	

        setDataArray1(floatArray);

        //24 hour gage only
        String dataFilePath2 = PostAnalysisManager.get24HourGageOnlyFilePath();
        setDataFileName2(dataFilePath2);

        return;
    }
    
    
    private float[] convertToFloatArray(double[][] totalPrecipGrid, float unitConversionFactor)
    {
    	String header = "SummedHourlyMpeDlg.convertToFloatArray(qpeAccum24hr): ";
    	Hrap_Grid hrap_grid = DailyQcUtils.getHrap_grid();
        int maxCols = hrap_grid.maxi;
        int maxRows = hrap_grid.maxj;
        int precipDay = DailyQcUtils.pcpn_day;
        
        System.out.println(header + "DailyQcUtils.pcpn_day = " + DailyQcUtils.pcpn_day);
        System.out.println(header + "precipDay = " + precipDay);
        float[] valueArray = new float[maxRows*maxCols];
        
        
        int index = 0;
        
        for (int row = 0; row < maxRows; row++) {
        	for (int col = 0; col < maxCols; col++) {
        		
        		float value = (float) totalPrecipGrid[row][col];
  
        		valueArray[index] = value * unitConversionFactor;
        		index++;
        	}
        }

     	
		return valueArray;
	}
    
    
    

	/*
     * Create the Merge Data menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {
        /*
         * Create custom items in the Control dropdown menu
         */
        MenuItem mergeDataMI = new MenuItem(controlMenu, SWT.NONE);
        mergeDataMI.setText("Merge Data");
        mergeDataMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                mergeDataAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "Sum of 24 1-HR MPE Fields", "24 HR Gage Only Field" };
        return names;
    }

    /**
     * Merge Data action.
     */
    private void mergeDataAction() {
        if (mergeDataDlg == null || mergeDataDlg.isDisposed() == true) {
            Shell platformShell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
            mergeDataDlg = new MergedGridBiasDlg(platformShell);
            mergeDataDlg.open();
        }
    }

    /**
     * Not used
     */
    @Override
    protected void addBottomControls()
    {

    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.mpe.ui.dialogs.postanalysis.BasePostAnalysisDlg#
     * getNumberOfColorLegends()
     */
    @Override
 
    protected int getNumberOfColorLegends() {
        return 1;
    }


    protected NamedColorUseSet createNamedColorUseSet1() 
	{
    	return PostAnalysisManager.getNamedColorUseSet("PRECIP_ACCUM");
	
	}
    
	protected NamedColorUseSet createNamedColorUseSet1Old() 
	{

	     String user_id = System.getProperty("user.name");
	     
	     int duration = SECONDS_PER_DAY; //24 hours
		
		 List<Colorvalue> colorList = GetColorValues.get_colorvalues(user_id,
				HydroDisplayManager.MPE_APPLICATION_NAME,"PRECIP_ACCUM", duration,
				"E", MPEColors.build_mpe_colors());

		 
		 int listSize = colorList.size() -2;
		 
		 double[] thresholdValues = new double[listSize];
		 String[] colorNameArray = new String[listSize];
		 
		 int index = 0;
		 
		 double thresholdValue = 0.0;
		 String colorNameValue = null;
		 
		 String missingColorName = "GREY50";
		 String lowValueColorName = "BLACK";
		 
		 for (Colorvalue colorValue : colorList)
		 {
			 thresholdValue = colorValue.getId().getThresholdValue();
			 colorNameValue = colorValue.getColorname().getColorName();
			 
	//		 System.out.printf("%s thresholdValue = %f colorNameValue = :%s: \n", header,
	//				 			thresholdValue, colorNameValue);
			 
			 if (thresholdValue == -9999.0 )
			 {
				 missingColorName = colorNameValue;
			 }
			 else if (thresholdValue == -8888.0)
			 {
				 lowValueColorName = colorNameValue;
				 
			 }
			 else //regular value
			 {
				 // -8888.0
				 colorNameArray[index] = colorNameValue;
				 thresholdValues[index] = thresholdValue;
				 index++;
				 if (index >= listSize)
				 {
					 break;
				 }
			 }
		 }

		 NamedColorUseSet namedColorUseSet1 =
				 new NamedColorUseSet("PRECIP_ACCUM", "PRECIP_ACCUM", thresholdValues, 
						 colorNameArray, missingColorName, lowValueColorName, duration);
		return namedColorUseSet1;
	}


	@Override
	protected NamedColorUseSet createNamedColorUseSet2()
	{
		NamedColorUseSet namedColorUseSet2 = createNamedColorUseSet1();
		return namedColorUseSet2;
	}
	
}
