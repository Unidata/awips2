/*
 * PgenFilterDlg
 * 
 * Date created: 22 March 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrDialog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.filter.ElementFilter;
import gov.noaa.nws.ncep.ui.pgen.filter.ForecastHourFilter;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
//import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

import org.dom4j.Document;
import org.dom4j.Node;
import org.dom4j.io.SAXReader;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * Implementation of a dialog to control filters.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#256		B. Yin   	      Initial Creation.
 * 10/10        #289        Archana.S      Added FilterButtonKeyListener 
 * 12/11		#522		B. Yin		Made the linkage between the filter dialog and the GFA dialog.
 * 12/11                    Q.Zhou      Read HOURS from filterHour.xml instead of fixed string.
 * </pre>
 * 
 * @author	B. Yin
 */

public class PgenFilterDlg extends CaveJFACEDialog {
	
	/**
	 * single instance
	 */
	static private PgenFilterDlg INSTANCE = null;
	
	//Pgen resource
	private PgenResource drawingLayer;
	
	//Map editor
//	private NCMapEditor mapEditor;
	private AbstractEditor mapEditor;
	
	//top level container for all widgets
	private Composite top;
	
	//check box for forecast hours
	private Button hourBtns[];
	
	//Hour buttons
	private static String HOURS[] = null; 
	private static Document filterHourTbl = null;
	private static String HOUR_XPATH = "/root/filterHour/label";
	//filter map
	private HashMap<String, ElementFilter> filterMap;
	
	//filter to block all hours
	private ForecastHourFilter blockAll;
	
    class FilterButtonKeyListener implements KeyListener{
    	int pivotalIndex;
    	int prevIndex;
    	int nextIndex;
    	int indexArraySize = 0;
    	int index;
    	int buttonEnabledCounter = 0;
    	int hoursArrayLength = HOURS.length;
    	List<Integer> enabledButtonIndexList = new ArrayList<Integer>(0);
    	
		 @Override
		  public void keyPressed(KeyEvent e) {

              /*Check if any button is already enabled*/
              for(index=0;index <hoursArrayLength; index++ ){
             
            	  /*If the button is enabled...*/
              	if(hourBtns[index].getSelection()){
              		
              		/*increment a counter*/
              		buttonEnabledCounter++;
              		
              		/*and store the index of the enabled button in a list*/
              		enabledButtonIndexList.add(index);
              	}
              }
              
              /*Return if not even a single button is enabled*/
              if(buttonEnabledCounter < 1){
              	return;
              }

              /*Else - proceed to get the size of the stored index Array*/
              indexArraySize = enabledButtonIndexList.size();
				
              /*Process the key-stroke...*/
              switch(e.keyCode){
				
				case '[':

					   if ( indexArraySize > 0){
						   /*If at least one button is enabled, get the index of the left-most enabled button, i.e. the first element in the list*/
						   pivotalIndex = enabledButtonIndexList.get(0);
					   }
					  
					   /*Set the index for the preceding button*/
				        prevIndex = pivotalIndex-1;
                      
				        /*If it is less than 0, set it to 0*/
				        if(prevIndex < 0){
                      	 
                      	 prevIndex = 0;
                       }

				        /*Except for the button whose index is prevIndex
				         * disable all the other buttons and notify the corresponding selection listener
				         * */
                       for( index = 1; index < hoursArrayLength; index ++){
				                if(index != prevIndex){
				                	hourBtns[index].setSelection(false);
				                	hourBtns[index].notifyListeners(SWT.Selection, new Event());
				                }
				        }
                       
                      /*Activate only the button whose index is prevIndex and notify its selection listener*/ 
				        hourBtns[prevIndex].setSelection(true);
				        hourBtns[prevIndex].notifyListeners(SWT.Selection, new Event());

					break;

				case ']':

					 /*If at least one button is enabled, get the index of the right-most enabled  button, i.e. the last element in the list*/
					     if(indexArraySize > 0){
					    	 pivotalIndex = enabledButtonIndexList.get(indexArraySize - 1);
					     }
					     
					     /*Set the index for the next button to be enabled*/
				        nextIndex = pivotalIndex+1;
				       
				        /*If the nextIndex is greater than the length of the button array, decrement it*/
				        if(nextIndex == hoursArrayLength){
				        	nextIndex --;
				        }

				        /*Except for the button whose index is nextIndex
				         * disable all the other buttons and notify the corresponding selection listener
				         * */    				        
				        for(index = 0; index < hoursArrayLength; index ++){
			                if ( index != nextIndex){
			                	hourBtns[index].setSelection(false);
			                	hourBtns[index].notifyListeners(SWT.Selection, new Event());
			                }
			            }
				       
                      /*Activate only the button whose index is prevIndex and notify its selection listener*/ 
				        hourBtns[nextIndex].setSelection(true);
				        hourBtns[nextIndex].notifyListeners(SWT.Selection, new Event());
					break;

				   default:
					break;
				}
              
              /*reset the array-list before the next key pressed event occurs*/
              enabledButtonIndexList = new ArrayList<Integer>(0);

		}

		@Override
		public void keyReleased(KeyEvent e) {
			// no-op
			
		}
	
  }; 
	/*
	 * constructor
	 */	/**
	 * Creates a forecast hour filter dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static PgenFilterDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new PgenFilterDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		return INSTANCE;
		
	} 	
	
	/**
	 * private constructor
	 * @param parentShell
	 * @throws VizException
	 */
	private PgenFilterDlg(Shell parentShell )throws VizException {
		super(parentShell);
        this.setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE );
        
        filterHourTbl = readFilterHourTbl();
        HOURS = getFilterHour();
        
        filterMap = new HashMap<String,ElementFilter>();
        
        for ( String str : HOURS ){
        	filterMap.put(str, new ForecastHourFilter(str));
        }
        
        blockAll = new ForecastHourFilter("blockAll");

	}

	
	/**
	 * Creates the dialog area
	 */
	@Override
	public Control createDialogArea(Composite parent) {
		
		// Set title
		getShell().setText("Forecast Hour Filter");
		
		top = (Composite) super.createDialogArea(parent);
		
        /*
         *  Create the main layout for the dialog area.
         */
        GridLayout mainLayout = new GridLayout(HOURS.length, false);

        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
       
        top.setLayout(mainLayout);
        
        hourBtns = new Button[HOURS.length];
     
        
        for (int ii = 0; ii < HOURS.length; ii++ ){
        	hourBtns[ii] = new Button(top,SWT.CHECK);
        	hourBtns[ii].setText(HOURS[ii]);
        	hourBtns[ii].addKeyListener(new FilterButtonKeyListener());
        	hourBtns[ii].addSelectionListener(new SelectionAdapter(){

				@Override
				public void widgetSelected(SelectionEvent e) {
					if ( ((Button)e.widget).getSelection() ){
						drawingLayer.getFilters().addFilter(filterMap.get(((Button)e.widget).getText()));

					}
					else {
						drawingLayer.getFilters().removeFilter(filterMap.get(((Button)e.widget).getText()));
					}
					mapEditor.refresh();
				}
        	});

        }
		
        return top;
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void okPressed() {
		
		if ( this.getButton(IDialogConstants.OK_ID).getText().equalsIgnoreCase("All On" )) {
			//add all filters
			for ( Button btn : hourBtns ){
				btn.setSelection(true);
				drawingLayer.getFilters().addFilter(filterMap.get(btn.getText()));
			}
			this.getButton(IDialogConstants.OK_ID).setText("All Off");
		}
		else {
			//remove all filters
			for ( Button btn : hourBtns ){
				btn.setSelection(false);
				drawingLayer.getFilters().removeFilter(filterMap.get(btn.getText()));
			}
			this.getButton(IDialogConstants.OK_ID).setText("All On");
		}
		
		mapEditor.refresh();
		
	}
	
	/*
	 * 
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void cancelPressed() {

		//remove all fliters
		for ( Button btn : hourBtns ){
			btn.setSelection(false);
			drawingLayer.getFilters().removeFilter(filterMap.get(btn.getText()));
		}
		
		drawingLayer.getFilters().removeFilter(blockAll);
		
		mapEditor.refresh();
		
		super.cancelPressed();
		
	}
	
	@Override
	/**
	 * Set the location of the dialog
	 * Set the OK button to Save
	 */
	public int open(){

		if ( this.getShell() == null ){
			this.create();
		}
		
   	    this.getShell().setLocation(this.getShell().getParent().getLocation());
  	    this.getButton(IDialogConstants.OK_ID).setText("All On");
  	    this.getButton(IDialogConstants.CANCEL_ID).setText("Close");

  	    this.getButtonBar().pack();
  	    
  	    //add filter to block all hours
  	    drawingLayer.getFilters().addFilter(blockAll);
  	    
   	    return super.open();
		
	}
	
	/**
	 * Sets the PGEN resource and the map editor
	 * @param resource
	 * @param editor
	 */
	public void setResource(PgenResource resource, AbstractEditor editor){
//	public void setResource(PgenResource resource, NCMapEditor editor){
		this.drawingLayer = resource;
		this.mapEditor = editor;
	}

	
	/**
	 * Read filter (hour) document
	 * @return - filter document
	 */
	public static Document readFilterHourTbl() {
		
		if (HOURS == null) {
			try {
				String filterHourFile =		PgenStaticDataProvider.getProvider().getFileAbsolutePath(
						   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + "filterHour.xml" );

				SAXReader reader = new SAXReader();
				filterHourTbl = reader.read(filterHourFile);
			} catch (Exception e) {
				e.printStackTrace();				
			}
		}
		
		return filterHourTbl;
	}
	
	/**
	 * Get hours from the filter (hour) document
	 */
	private String[] getFilterHour() {
		if (filterHourTbl == null)
			HOURS = new String[] {"0","0+", "3", "3+", "6","9","12","0-0","3-3","6-6","0-3","0-6","3-6","6-9",
				"6-12","9-12","AIRM","OTLK"};
		else {
			List<String> list = new ArrayList<String>();		
			List<Node> nodes = filterHourTbl.selectNodes(HOUR_XPATH);
		
			for (Node node : nodes) {
				list.add( node.valueOf("@text").toString());
			}
		
			HOURS = new String[list.size()];
			HOURS = list.toArray(HOURS);
		}
		
		return HOURS;
	}

	
	/**
	 * Sets the filter check box
	 * @param hours - text of the check box
	 * @param flag - check or un-check
	 */
	public void setHourChkBox(String hours, boolean flag){
		
		//search the check box for the input hour string
		Button hrBtn = null;
		for ( Button btn : hourBtns ){
			if ( btn.getText().equalsIgnoreCase(hours )){
				hrBtn = btn;
				break;
			}
		}
		
		//If the input hour does not match and check box,
		//check if the input hour is in format H:MM, and then check if it is between 0 and 3,
		if ( hrBtn == null ){
			double fHrs = -1;
			double fMinutes = -1;
			if ( hours.contains(":")){
				String hm[] = hours.split(":");
				try {
					fHrs = Integer.valueOf( hm[0]);
					fMinutes = Integer.valueOf( hm[1]);
				}
				catch ( Exception e){
					e.printStackTrace();
				}
			}
			else {
				try {
					fHrs = Integer.valueOf( hours );
				}
				catch ( Exception e){
					e.printStackTrace();
				}
			}
			
			fMinutes = (fMinutes >0)? (fMinutes/60.):0;
			if ( fHrs > 0 ){
				String hrStr = "";
				fHrs += fMinutes;
				if ( fHrs <= 0.001 ) hrStr = "0";
				else if ( fHrs > 0.001 && fHrs < 3 ) hrStr = "0+";
				else if ( Math.abs(fHrs-3) < 0.001 ) hrStr = "3";
				else if ( fHrs > 3.001 ) hrStr ="3+";
				if (! hrStr.isEmpty()){
					for ( Button btn : hourBtns ){
						if ( btn.getText().equalsIgnoreCase( hrStr )){
							hrBtn = btn;
							break;
						}
					}
				}
			}
			
		}
		
		//check or un-check the box
		if ( hrBtn != null ){
			if ( flag ){
				hrBtn.setSelection(true);
				drawingLayer.getFilters().addFilter(filterMap.get(hrBtn.getText()));
			}
			else {
				hrBtn.setSelection(false);
				drawingLayer.getFilters().removeFilter(filterMap.get(hrBtn.getText()));
			}
		}
			
		
	}
	
	/**
	 * Checks if the PGEN filter dialog is open
	 * @return
	 */
	static public boolean isFilterDlgOpen(){
		if ( INSTANCE != null && INSTANCE.getShell() != null){
			return true;
		}
		return false;
	}

}
