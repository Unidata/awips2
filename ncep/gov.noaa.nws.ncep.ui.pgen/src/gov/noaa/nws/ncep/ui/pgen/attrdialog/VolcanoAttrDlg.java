/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.SymbolAttrDlg
 * 
 * 20 June 2009
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import gov.noaa.nws.ncep.edex.common.stationTables.Station;
import gov.noaa.nws.ncep.ui.pgen.elements.Symbol;
import gov.noaa.nws.ncep.ui.pgen.sigmet.SigmetInfo;

import java.text.DecimalFormat;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.dialogs.CaveJFACEDialog;

/**
 * Singleton attribute dialog with the volcano list window.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 06/09		#116			B. Yin   	Initial Creation.
 * 12/11		#366			B. Yin		Added volcano list.
 *
 * </pre>
 * 
 * @author	B. Yin
 */

public class VolcanoAttrDlg extends LabeledSymbolAttrDlg {
	
	static private VolcanoAttrDlg INSTANCE = null;
	static private VolcanoListDlg volList = null;
	
	private String volText = "";
	private String volLocation = "";

	/**
	 * Private constructor
	 * @param parShell
	 * @throws VizException
	 */
	private VolcanoAttrDlg(Shell parShell) throws VizException {
		
        super(parShell);
        volList = new VolcanoListDlg(parShell);

    }
	
	/**
	 * Creates a volcano attribute dialog if the dialog does not exist 
	 * and returns the instance. If the dialog exists, return the instance.
	 *  
	 * @param parShell
	 * @return
	 */
	public static SymbolAttrDlg getInstance( Shell parShell){
		
		if ( INSTANCE == null ){
					
			try {
				
				INSTANCE = new VolcanoAttrDlg( parShell );
				
			} catch (VizException e) {
				
				e.printStackTrace();
				
			}	
		}
		
		return INSTANCE;
		
	} 	
	
	@Override
	public int open(){
		
		int rt = super.open();
		
    	volList.setBlockOnOpen( false );
    	
    	Point loc = this.getShell().getLocation();
    	
    	if ( de != null && de instanceof Symbol ){
    		latitudeText.setText(new DecimalFormat("###.000").format(((Symbol)de).getLocation().y));
    		longitudeText.setText(new DecimalFormat("###.000").format(((Symbol)de).getLocation().x));
    	}
    	labelChkBox.setSelection(true);
    	
    	volList.create();
    	volList.getShell().setLocation(loc.x+this.getShell().getSize().x, loc.y);
		volList.open();
		
		return rt;
		
	}
	
	@Override
	public boolean close(){
		
		volList.close();
		return super.close();
		
	}
	
	/**
	 * Sets the text in the latitude field of the dialog
	 * @param lat
	 */
	@Override
	public void setLatitude( double lat ){
		
		//latitudeText.setText( new DecimalFormat("###.000").format( lat ));
	  		
	}
	
	/**
	 * Sets the text in the longitude field of the dialog
	 * @param lon
	 */
	@Override
	public void setLongitude( double lon ){
		
		//longitudeText.setText(new DecimalFormat("####.000").format(lon));
		
	}
	
	public String getVolText(){
		return volLocation.isEmpty()? volText : volText + "\n" + volLocation;
	}
	
	/**
	 * Dialog class for volcano list
	 * @author bingfan
	 *
	 */
	private class VolcanoListDlg  extends CaveJFACEDialog {

		private Text volName;
		
		protected VolcanoListDlg(Shell parentShell) {
			super(parentShell);
	        setShellStyle(SWT.TITLE | SWT.MODELESS | SWT.CLOSE);
		}
		
		/**
		 * Creates the dialog area
		 */
		@Override
		public Control createDialogArea(Composite parent) {
			
		        top = (Composite) super.createDialogArea(parent);

		        // Create the main layout for the shell.
		        GridLayout mainLayout = new GridLayout(2, false);
		        mainLayout.marginHeight = 3;
		        mainLayout.marginWidth = 3;
		        top.setLayout(mainLayout);

		        // Initialize all of the menus, controls, and layouts
		        this.initializeComponents();
		 
		        return top;
		        
		}
		
		/**
		 * Creates buttons, menus, and other controls in the dialog area
		 */
		protected void initializeComponents() {
			
	        this.getShell().setText("Volcano List");
	        
	        volName = new Text(top, SWT.LEFT);
	        volName.setLayoutData(new GridData(180, 20));
	        
	        volName.addModifyListener( new ModifyListener(){

				@Override
				public void modifyText(ModifyEvent e) {
					volText = ((Text)e.widget).getText();
					volLocation = "";
				}
	        	
	        });

	        createVolList(top);
	   //     Combo volMenu = new Combo(top, SWT.DROP_DOWN|SWT.READ_ONLY);
	   /*     Menu vol = new Menu( volMenu );
	        
	        MenuItem notL = new MenuItem(SWT.CASCADE);
	        volMenu.add
	        */
	   //     volMenu.add("Not listed");
	   //     volMenu.add("Aa-Am");
	   //     volMenu.add("An-Az");

	   //     volMenu.select(0);
	   //     volMenu.pack();
	        
		}
		
		@Override
		public Control createButtonBar(Composite parent){
			return parent;
			
		}
		
		private void createVolList(Composite comp){
			Shell shell = this.getShell();
			
	        final ToolBar tb = new ToolBar(comp, SWT.HORIZONTAL);
	    	final ToolItem ti = new ToolItem(tb, SWT.DROP_DOWN);
	    	    	
	    	final Menu mu = new Menu(shell, SWT.POP_UP);
	    	
	    	for(int i=0; i<SigmetInfo.VOL_NAME_BUCKET_ARRAY.length; i++){	
				if(i==0){// first option is entering name
					MenuItem mi1 = new MenuItem(mu, SWT.PUSH);
		    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);
		    		
		    		mi1.addListener(SWT.Selection, new Listener() {
		    			public void handleEvent(Event e){	    				
		    				//resetTxts();
		    				//setTxtsEditable(true);	    				
		    			}
		    		});
				}else{
		    		MenuItem mi1 = new MenuItem(mu, SWT.CASCADE);
		    		mi1.setText(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);
		    		Menu mi1Menu = new Menu(shell,SWT.DROP_DOWN);
		    		mi1.setMenu(mi1Menu);
		    	
				    java.util.List<String> list = SigmetInfo.VOLCANO_BUCKET_MAP.get(SigmetInfo.VOL_NAME_BUCKET_ARRAY[i]);	
				    int size = list.size();
				    for(int j=0; j<size; j++){
				    	final MenuItem mi1MenuMi1 = new MenuItem(mi1Menu,SWT.PUSH);
				    	mi1MenuMi1.setText(list.get(j));
				    	mi1MenuMi1.addListener(SWT.Selection, new Listener(){
				    		public void handleEvent(Event e){			    			
				    			//resetTxts();
				    			//setTxtsEditable(false);
				    			//setTextToTxts(mi1MenuMi1.getText());
				    			volName.setText(mi1MenuMi1.getText());
				    			setLatLonFields(mi1MenuMi1.getText());
				    			placeBtn.setEnabled(true);
				    		}
				    	});	
				    }
		    	}
	    	}
	    	
	    	ti.addListener(SWT.Selection, new Listener() {
	        	/* Main button clicked:  Pop up the menu showing all the symbols. */
	        	public void handleEvent(Event event)
	        	{
	        		Rectangle bounds = ti.getBounds();
	        		Point point = tb.toDisplay(bounds.x, bounds.y + bounds.height);
	        		mu.setLocation(point);
	        		mu.setVisible(true);
	        	}
	        });
			
			
			
			//Label lblNum = new Label(comp, SWT.LEFT);		
			//lblNum.setText("   Number: ");		

			//txtNum = new Text(comp, SWT.READ_ONLY|SWT.BORDER);
			
		}
		
		private void setLatLonFields( String volName ){
			
			java.util.List<Station> list = SigmetInfo.VOLCANO_STATION_LIST;
			
			Station stn = null;
			
			for(Station station : list){
				if( volName != null && volName.equals(station.getStnname()) ) 
						stn = station;
			}
			
			if ( stn != null ){
				longitudeText.setText(String.valueOf(stn.getLongitude()));
				latitudeText.setText(String.valueOf(stn.getLatitude()));
				setVolText(volName, stn.getLongitude(),stn.getLatitude() );
			}
		}
		
		private void setVolText(String name, double lon, double lat){
			volText = name;
			volLocation = ((lat >= 0 ) ? new DecimalFormat("###.0").format(lat) + "N":
									   new DecimalFormat("###.0").format(lat*-1) + "S")
						+ " " 
					    +((lon >= 0 ) ? new DecimalFormat("###.0").format(lon) + "E":
							   new DecimalFormat("###.0").format(lon*-1) + "W");
		}
	}
	
}
