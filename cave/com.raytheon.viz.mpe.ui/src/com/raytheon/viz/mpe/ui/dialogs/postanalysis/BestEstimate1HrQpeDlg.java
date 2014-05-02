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

import java.io.File;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.mpe.util.XmrgFile;
import com.raytheon.viz.hydrocommon.whfslib.colorthreshold.NamedColorUseSet;

/**
 * 1hr Best Estimate QPE Fields dialog.
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

public class BestEstimate1HrQpeDlg extends BasePostAnalysisDlg {

    private static final int HOURS_PER_DAY = 24;
	/**
     * File combo box.
     */
    private Combo fileCbo = null;
    private String selectedFileName = null;
    
    private List<String> xmrgFileList = null;
    private PostAnalysisManager paMgr = null;
	private String qpeDirectory = null;
	private String adjustedDirectory = null;
    
    
    private double[][] biasRatioGrid = null;
    private double[][] disaggGrid = null;
    
    
    /**
     * Constructor.
     * 
     * @param parentShell
     *            Parent shell.
     */
    public BestEstimate1HrQpeDlg(Shell parentShell, double[][] biasRatioGrid, double[][] disaggGrid) {
        super(parentShell);

        setText("1hr Best Estimate QPE Fields");
        paMgr = new PostAnalysisManager();
        xmrgFileList = paMgr.getListOfAvailableXmrgFiles();
        
        this.biasRatioGrid = biasRatioGrid;
        this.disaggGrid = disaggGrid;
        
    	qpeDirectory = paMgr.getXmrgFileDirectory().getAbsolutePath();
    	
    	File paFileDirectory = paMgr.getPostAnalysisFileDirectory();
    	
    	if (paFileDirectory != null)
    	{
    		adjustedDirectory = paFileDirectory.getAbsolutePath();
    	}
    	
    }

    /*
     * Create the Display 1hr QPE Fields menu item.
     */
    @Override
    protected void createControlMenuItem(Menu controlMenu) {

        MenuItem saveQpeItem = new MenuItem(controlMenu, SWT.CASCADE);
        saveQpeItem.setText("Save 1hr QPE Fields");

        Menu saveQpeSubMenu = new Menu(shell, SWT.DROP_DOWN);
        saveQpeItem.setMenu(saveQpeSubMenu);

        MenuItem saveSeparateQpeMI = new MenuItem(saveQpeSubMenu, SWT.NONE);
        saveSeparateQpeMI.setText("Save/Separate");
        saveSeparateQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveSeparateAction();
            }
        });

        MenuItem saveOverwriteQpeMI = new MenuItem(saveQpeSubMenu, SWT.NONE);
        saveOverwriteQpeMI.setText("Save/Overwrite");
        saveOverwriteQpeMI.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                saveOverwriteAction();
            }
        });
    }

    /*
     * Get the names for the labels that appear over the maps.
     */
    @Override
    protected String[] getMapLabelNames() {
        String[] names = new String[] { "1hr Best Estimate QPE", "1hr Best Estimate QPE(Grid Bias Applied)" };
        return names;
    }
    
    
    private void loadAdjustAndSaveAllQPEFiles(String destinationDirectoryName)
    {
    	for (String fileName: xmrgFileList)
    	{
    		loadAdjustAndSave(fileName, destinationDirectoryName);
    	}
    }
    
    private void loadAdjustAndSave(String fileName, String destinationDirectoryName)
    {
    	String header = "BestEstimate1HrQPEDlg.loadAdjustAndSave(): ";
    	
    	String originalFilePath = qpeDirectory + '/' + fileName;
    	String destinationFilePath = destinationDirectoryName + '/' + fileName;
    	
    	double[][] adjustedGrid = paMgr.readGridData(originalFilePath, true, true);
    	
    	if (adjustedGrid != null)
    	{
    	    applyGridAdjustments(adjustedGrid, biasRatioGrid, disaggGrid);

    	    float[] dataArray = paMgr.convertToSingleArray(adjustedGrid, false, true);
    	    short[] shortArray= paMgr.convertToShortArray(dataArray, 100.0f);

    	    XmrgFile file = new XmrgFile();  

    	    file.setData(shortArray);
    	    file.setHrapExtent(paMgr.getExtent());
    	    file.setHeader(paMgr.getXmrgHeader());

    	    try
    	    {
    	        file.save(destinationFilePath);
    	        System.out.println(header + "Saved xmrg file to " + destinationFilePath);
    	    }
    	    catch (Exception e)
    	    {
    	        e.printStackTrace();
    	    }
    	}
    	
    }

    /**
     * Save separate action.
     */
    private void saveSeparateAction() {
   	
    	loadAdjustAndSaveAllQPEFiles(adjustedDirectory);
    
    }

    /**
     * Save overwrite action.
     */
    private void saveOverwriteAction()
    {

    	loadAdjustAndSaveAllQPEFiles(qpeDirectory);
    }

    /**
     * Add a combo control to the bottom of the dialog.
     */
    @Override
    protected void addBottomControls() {
        GridData gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        gd.widthHint = 300;
        fileCbo = new Combo(shell, SWT.DROP_DOWN | SWT.BORDER | SWT.READ_ONLY);
        fileCbo.setLayoutData(gd);
        
        for (String fileName : xmrgFileList)
        {
        	  fileCbo.add(fileName);
        }
        
        fileCbo.addSelectionListener(new SelectionAdapter() 
        {
        	public void widgetSelected(SelectionEvent event) {
        		
        		System.out.println("fileCbo.addSelectionListener().widgetSelected() ");
        		
        		selectedFileName = fileCbo.getText();
        		
                loadImage(event.data);
            }
        } );
        
        //make an initial selection    	
        fileCbo.select(0);
        selectedFileName = fileCbo.getText();
        loadImage(null);
      
    }

    
    private void loadImage(Object data) {
		
    	String header = "BestEstimate1HrQpeDir.loadImage(): ";
    	
    	System.out.println(header + " method called.");
    	
   
		String filePath1 = qpeDirectory + '/' + selectedFileName;
	//	String filePath2 = adjustedDirectory + '/' + selectedFileName;
		
		
		System.out.println(header + " filePath1 = " + filePath1);
	//	System.out.println(header + " filePath2 = " + filePath2);
		
		//do this temporarily until I have enough good fake data
	//	filePath1 = "/home/cgobs2/xmrg_data/MMOSAIC2011081901z";
		
		setDataFileName1(filePath1);
		
	//	filePath2 = "/home/cgobs2/xmrg_data/MMOSAIC2011081901z";  //do this temporarily until I build the adjusted data
		
		//save the first image as a grid (a 2D array) so that I can then hold the adjusted grid in memory and then adjust it
		double[][] adjustedGrid = paMgr.readGridData(filePath1, false, false);
		applyGridAdjustments(adjustedGrid, biasRatioGrid, disaggGrid);
		
		float[] dataArray2 = paMgr.convertToSingleArray(adjustedGrid, false, false);
		setDataArray2(dataArray2);
		setExtent2(paMgr.getExtent());
		
		long newEndTime = paMgr.getTimeFromFileName(selectedFileName);
		String newEndTimeString = "ending at " + getDateTimeStringFromLongTime(newEndTime);
		//refresh the ColorLegend
		
		colorLegendMgr.setDateTimeStringForLegend(newEndTimeString);
		
		//refresh the maps
		mapsComp.refresh();
	}
    
    private void applyGridAdjustments(double[][] adjustedGrid, 
    								  double[][] biasGrid,
    								  double[][] disaggGrid)
    {
    	
    		try
    		{
    	
        	int rowCount = adjustedGrid.length;
        	int colCount = adjustedGrid[0].length;
        		
        	for (int row = 0; row < rowCount; row++)
        	{
        		for (int col = 0; col < colCount; col++)
        		{
        			double biasValue = biasGrid[col][row];
        			double disaggValue = disaggGrid[col][row] / HOURS_PER_DAY;
        			
        			if (biasValue > 0.0)
        			{
        			    adjustedGrid[row][col] *= biasValue;
        			}
        			else if (disaggValue > 0.0)
        			{
        				adjustedGrid[row][col] = disaggValue;
        			}
         		
        		}
        		
        	}
    		}
    		catch (Throwable t)
    		{
    			t.printStackTrace();
    		}
      
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


	@Override
	protected NamedColorUseSet createNamedColorUseSet1() {
		
		return PostAnalysisManager.getNamedColorUseSet("PRECIP_ACCUM");
		//NamedColorUseSet namedColorUseSet1 = TestDriver.getNamedColorUseSet1Hr();
		//return namedColorUseSet1;
	}


	@Override
	protected NamedColorUseSet createNamedColorUseSet2() {
		return PostAnalysisManager.getNamedColorUseSet("PRECIP_ACCUM");
		//NamedColorUseSet namedColorUseSet2 = TestDriver.getNamedColorUseSet1Hr();
		//return namedColorUseSet2;
	}
	
}
