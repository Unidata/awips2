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

import gov.noaa.nws.ncep.ui.pgen.filter.ElementFilter;
import gov.noaa.nws.ncep.ui.pgen.filter.ForecastHourFilter;
import gov.noaa.nws.ncep.ui.pgen.rsc.PgenResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;

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

/**
 * Implementation of a dialog to control filters.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/10		#256		B. Yin   	      Initial Creation.
 * 10/10       #289        Archana.S      Added FilterButtonKeyListener 
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
	private NCMapEditor mapEditor;
	
	//top level container for all widgets
	private Composite top;
	
	//check box for forecast hours
	private Button hourBtns[];
	
	//Hour buttons
	private static String HOURS[] = {"0","0+", "3", "3+", "6","9","12","0-0","3-3","6-6","0-3","0-6","3-6","6-9",
		"6-12","9-12","AIRM","OTLK"};
	
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
	
	public void setResource(PgenResource resource, NCMapEditor editor){
		this.drawingLayer = resource;
		this.mapEditor = editor;
	}
}
