package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.common.ui.NmapCommon;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.dgdriv.GridDBConstants;
import gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc.EnsembleComponentData.EnsComp;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.VerifyEvent;
import org.eclipse.swt.events.VerifyListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.catalog.LayerProperty;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceType;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/2010  	277        	M. Li    	Initial creation 
 * 12/2011      578         G. Hull     Change to Composite on the Edit Attrs Dlg
 * 12/2011      578         G. Hull     create from seld cycle time.
 * 01/10/12                 X. Guo      Updated Attrs Dlg editor
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class EnsembleSelectComposite extends Composite {    
    private final int WINDOW_WIDTH = 680;
    private final int HEIGHT_PER_LINE = 41;
    private final String NA_CYCLE = "  N/A  ";

    private DataTime seldEnsCycleTime;
    
//    private String availModels;
    
    private Text selectedModelText;
        
//    private String modelListString;    
//    private String modelListString0;
    
    public final static int MaxNumOfEnsembleCycles = 4;
    
    private List<EnsComp> ensCompList;
    

    private EnsembleComponentData ensCompData;

    private ScrolledComposite scrolledComposite;
    // like GDFILE but with relative cycletimes
//    private String ensembleComponentWeightsStr;
    
    // update this whenever a change is made for any selection.
    //
    private RscAttrValue ensembleComponentWeightsAttr; 
    
	private class EnsembleCompGuiData {
//		private int cycleNumber = 4;
		private String modelName;  // create the ensC
		private String memberName;  // 
		public  String getEnsCompName() { return( memberName != null ? modelName+":"+memberName : modelName ); }
		private boolean isPrimary;
		private boolean hasMembers;
		private boolean isExpanded = true;
		private int memberCount;
		EnsembleCompGuiData[] members;
		private String[] cyclesLblStr = new String[MaxNumOfEnsembleCycles];
		private Date[] cycleTimes = new Date[MaxNumOfEnsembleCycles];
		
//		Composite composite;
		Composite memberComp;
		Button expandMembersButton;
		Button modelNameButton;
		Button isPrimaryButton;
		
		Text[] weightText = new Text[MaxNumOfEnsembleCycles];
		Button[] cycleButtons = new Button[MaxNumOfEnsembleCycles];
		
	}
	    
	public EnsembleSelectComposite( Composite parent ) {
		super( parent, SWT.SHADOW_NONE );
	}

	public void init( NcEnsembleResourceData rscData,
					  ResourceAttrSet editedRscAttrSet ) throws VizException {
		seldEnsCycleTime = rscData.getResourceName().getCycleTime();
		
		
		ensembleComponentWeightsAttr = editedRscAttrSet.getRscAttr("ensembleComponentWeights");

		if( ensembleComponentWeightsAttr.getAttrClass() != String.class ) {
    		throw new VizException( "GDFILE is not of expected class? "+ 
    				ensembleComponentWeightsAttr.getAttrClass().toString() );
		}
		
    	createSelectableEnsembleComponentGuiData( rscData.getAvailableModels() );
    	
    	ensembleComponentWeightsAttr.setAttrValue( rscData.getEnsembleComponentWeights() );
		
//		this.modelListString = availModels;
//		this.modelListString0 = availModels;
		
		ensCompData = new EnsembleComponentData( seldEnsCycleTime.getRefTime(),
							(String)ensembleComponentWeightsAttr.getAttrValue() );
		
		ensCompList = ensCompData.getEnsembleComponentsList();
		
 		initializeComponents();
	}


	private List<EnsembleCompGuiData> ensCompGuiList;
	
    /**
     * Create and Initialize the widgets
     */
    private void initializeComponents() {

    	
    	Composite comp = new Composite( this, SWT.NONE);
    	GridLayout gl = new GridLayout(1, false);
    	comp.setLayout(gl);
    	
    	Label cycTimeLbl = new Label( comp, SWT.NONE );
    	cycTimeLbl.setText( "     Selected Cycle Time : "+ NmapCommon.getTimeStringFromDataTime( seldEnsCycleTime, "/" ) +"\n");
    	
    	Label label = new Label(comp, SWT.NONE);
    	label.setText("          Model              Primary               Cycle1                   Cycle2                  Cycle3                   Cycle4");
//    	GridData gd = new GridData(WINDOW_WIDTH, SWT.DEFAULT);
//    	label.setLayoutData(gd);

    	createModelListControls();
    	createModelSelectionControls();
    	setEnsembleSelectionGUI();
    	
    	updateSelectedModels();

    }
    
    
    private void createSelectableEnsembleComponentGuiData( String availModels ) throws VizException {
    	 
    	if( availModels == null || availModels.isEmpty() ) {
    		throw new VizException("No Models are specified for the Ensemble Components?");
    	}
    	
    	String ensCompsModels[] = availModels.split(";"	);

		ensCompGuiList = new ArrayList<EnsembleCompGuiData>();

    	for( String ensCompModel : ensCompsModels ) {
    		ensCompModel = ensCompModel.trim();
    		
    		int memberIndx = ensCompModel.indexOf(':'); 
    		
    		// if this has no ensemble members
    		if( memberIndx == -1 ) {
    			EnsembleCompGuiData ensData2 = new EnsembleCompGuiData();
    			ensData2.modelName = ensCompModel;
    			ensData2.memberName  = null;
    			
    			ensData2.hasMembers = false;
    			ensData2.isPrimary = false;

    			ArrayList<Date> cycles = getAvailCycleTimes( seldEnsCycleTime.getRefTime(), ensCompModel );
    			
    			ensData2.cyclesLblStr = new String[MaxNumOfEnsembleCycles];
    			ensData2.cycleTimes = new Date[MaxNumOfEnsembleCycles];
    			
    			for (int i = 0; i < MaxNumOfEnsembleCycles ; i++ ) {
    				if (i < cycles.size()) {
    					ensData2.cycleTimes[i] = cycles.get(i);
    					ensData2.cyclesLblStr[i] = 
    						EnsembleComponentData.getCycleTimeStrFromDataTime( cycles.get(i) );
    				}
    				else {
    					ensData2.cycleTimes[i] = null;
    					ensData2.cyclesLblStr[i] = NA_CYCLE;
    				}
    			}

    			ensCompGuiList.add(ensData2);
    		}
    		else { // has members
    			String modelName = ensCompModel.substring(0,memberIndx).trim();
    			String membersStr = ensCompModel.substring(memberIndx+1).trim();
    			
    			if( membersStr == null || membersStr.isEmpty() ) {
    				throw new VizException("Error parsing members for model ("+
    						modelName+") for availableModels : "+ availModels );
    			}
    			String[] memberStrs = membersStr.split(",");

    			EnsembleCompGuiData ensData1 = new EnsembleCompGuiData();
    			ensData1.modelName = modelName;
    			ensData1.memberName = null;
    			
    			ensData1.hasMembers = true;
    			ensData1.isPrimary = false;
    			ensData1.memberCount = memberStrs.length;

    			ArrayList<Date> cycles = getAvailCycleTimes( seldEnsCycleTime.getRefTime(), modelName );
    			
    			ensData1.cycleTimes   = new Date[MaxNumOfEnsembleCycles];
    			ensData1.cyclesLblStr = new String[MaxNumOfEnsembleCycles];
    			
    			for( int i = 0; i < MaxNumOfEnsembleCycles ; i++ ) {
    				if (i < cycles.size()) {
    					ensData1.cycleTimes[i] = cycles.get(i);
    					ensData1.cyclesLblStr[i] = 
    						EnsembleComponentData.getCycleTimeStrFromDataTime( cycles.get(i) );
    				}
    				else {
    					ensData1.cycleTimes[i] = null;
    					ensData1.cyclesLblStr[i] = NA_CYCLE;
    				}
    			}

    			ensData1.members = new EnsembleCompGuiData[ensData1.memberCount];
    			
    			for (int i = 0; i < ensData1.memberCount; i++) {
    				ensData1.members[i] = new EnsembleCompGuiData();
    				ensData1.members[i].modelName = modelName.trim();
    				ensData1.members[i].memberName =  memberStrs[i].trim();
    				ensData1.members[i].hasMembers = false;
    				ensData1.members[i].isPrimary = false;
    				
    			}
    			ensCompGuiList.add(ensData1);
    		}
		}
    }
    
	@SuppressWarnings("null")
	public static ArrayList<Date> getAvailCycleTimes( Date seldCycleTime, String modelName ) {
		
		ArrayList<Date> availCycleTimesList = new ArrayList<Date>();
		
		HashMap<String,RequestConstraint> constraintMap = new HashMap<String,RequestConstraint>();
		constraintMap.put( GridDBConstants.PLUGIN_NAME, new RequestConstraint( GridDBConstants.GRID_TBL_NAME, ConstraintType.EQUALS) );
		constraintMap.put( GridDBConstants.MODEL_NAME_QUERY, 
        		new RequestConstraint( modelName, ConstraintType.EQUALS ) );
		
		LayerProperty property = new LayerProperty();
        property.setDesiredProduct( ResourceType.PLAN_VIEW );
        DataTime[] availableTimes;
        
        try {
			property.setEntryQueryParameters( constraintMap );
	        availableTimes = property.getEntryTimes();

//	        System.out.println("availableTimes.lenght==="+availableTimes.length);
	        
	        for( DataTime dt : availableTimes ) {
		        // 
		        if( seldCycleTime.getTime() >= dt.getRefTime().getTime() ) { 
		        	if( !availCycleTimesList.contains( dt.getRefTime() ) ) {
		        		availCycleTimesList.add( 0, dt.getRefTime() );
		        		if( availCycleTimesList.size() == MaxNumOfEnsembleCycles ) {
		        			break;
		        		}
		        	}
		        }
	        }	      
        } catch (VizException e) {
			System.out.println("Error querying cycle times.");
		}
        
		return availCycleTimesList;		
	}

//	private String getCycleTimeStrFromDataTime( Date dt ) {
//
//    	NumberFormat nf = NumberFormat.getInstance();
//    	nf.setMinimumIntegerDigits(2);
//    	nf.setMinimumFractionDigits(0);
//    	nf.setMaximumFractionDigits(2);
//    	nf.setMaximumIntegerDigits(2);
//
//    	Calendar cal = Calendar.getInstance( TimeZone.getTimeZone("GMT") );
//    	cal.setTime(dt);//.getRefTime());
//    	String dd = nf.format(cal.get(Calendar.DAY_OF_MONTH));
//    	String hh = nf.format(cal.get(Calendar.HOUR_OF_DAY));
//
//    	String cycleTimeStr = String.format("%s/%s", dd, hh);
//    	return cycleTimeStr;
//    }
    
    /**
     * Create the locatorSelection controls.
     */
    private void createModelSelectionControls() {
        Composite modelSelectionComp = new Composite( this, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        modelSelectionComp.setLayout(gl);

        GridData gd = new GridData(120, SWT.DEFAULT);
        Label label = new Label(modelSelectionComp, SWT.NONE);
        label.setText("Selected Models:");
        label.setLayoutData(gd);

        
        selectedModelText = new Text(modelSelectionComp, SWT.SINGLE|SWT.BORDER);
        selectedModelText.setLayoutData(new GridData(450, SWT.DEFAULT));
        
        Button clearAllBtn = new Button( modelSelectionComp, SWT.NONE);
        clearAllBtn.setText("Clear");
//        clearAllBtn.setLayoutData(gd);
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	clearAll();
            }
        });

    }

   
    private void createModelListControls() {
    	
    	if ( scrolledComposite != null ) {
    		scrolledComposite.dispose();
    	}
    	scrolledComposite = new ScrolledComposite( this, SWT.V_SCROLL|SWT.BORDER);
    	scrolledComposite.setLayout(new GridLayout(1, true));
    	GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 8 * HEIGHT_PER_LINE;
       // gd.widthHint = WINDOW_WIDTH;
        scrolledComposite.setLayoutData(gd);
        Composite modelListComp = new Composite(scrolledComposite, SWT.NONE);
        
        GridLayout gl = new GridLayout(1, false);
        modelListComp.setLayout(gl);
        
        for (int i = 0; i < ensCompGuiList.size(); i++) {
        	Composite comp = new Composite(modelListComp, SWT.NONE);
        	comp.setLayout(new GridLayout(15, false));
        	comp.setLayoutData(new GridData());//WINDOW_WIDTH, SWT.DEFAULT));

//        	ensCompGuiList.get(i).composite = new Composite(modelListComp, SWT.NONE);
//        	ensCompGuiList.get(i).composite.setLayout(new GridLayout(15, false));
//        	ensCompGuiList.get(i).composite.setLayoutData(new GridData(WINDOW_WIDTH, SWT.DEFAULT));
        	
        	createWidget(i, comp, false, -1);
        	modelListComp.pack();

        	
        	if (ensCompGuiList.get(i).hasMembers) {
        		ensCompGuiList.get(i).memberComp = new Composite(modelListComp, SWT.NONE);
        		ensCompGuiList.get(i).memberComp.setLayout(new GridLayout(15, false));
        		GridData griddata = new GridData(WINDOW_WIDTH, ensCompGuiList.get(i).memberCount * HEIGHT_PER_LINE);
        		griddata.verticalIndent = 0;
        		griddata.grabExcessVerticalSpace = true;
        		griddata.verticalAlignment = SWT.TOP;
            	ensCompGuiList.get(i).memberComp.setLayoutData(griddata);
        		for(int kk = 0; kk < ensCompGuiList.get(i).memberCount; kk++) {
        			createWidget(i, ensCompGuiList.get(i).memberComp, true, kk);
     				modelListComp.pack();
        		}
        		
        		((GridData)ensCompGuiList.get(i).memberComp.getLayoutData()).exclude = false;
    			ensCompGuiList.get(i).memberComp.setVisible(true);
        	}
        	
        }
        
        modelListComp.setSize(WINDOW_WIDTH, getTotalModelNumber()*HEIGHT_PER_LINE);
        scrolledComposite.setContent(modelListComp);
        
    }
   
     private void createWidget(final int index, Composite composite, boolean isMember, final int memberIndex) {
    	 int naCycleCnts;
    	 boolean isChecked;
    	 //Arrow Button to turn on/off ensemble members
    	boolean hasMembers = ensCompGuiList.get(index).hasMembers;
    	if (isMember) 
    		hasMembers = false;
    	
     	if (hasMembers) {
     		boolean isExpanded = false;
     		
     		if ( ensCompGuiList.get(index).isExpanded ) {
     			isExpanded = true;
     		}
     		if ( ensCompGuiList.get(index).expandMembersButton != null ) {
     			ensCompGuiList.get(index).expandMembersButton.dispose();
     		}
     		if ( isExpanded ) {
     			ensCompGuiList.get(index).expandMembersButton = new Button(composite, SWT.ARROW|SWT.DOWN);
     		}
     		else {
     			ensCompGuiList.get(index).expandMembersButton = new Button(composite, SWT.ARROW|SWT.RIGHT);
     		}
     		ensCompGuiList.get(index).expandMembersButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensCompGuiList.get(index).expandMembersButton.getAlignment() == SWT.DOWN) {
     					ensCompGuiList.get(index).expandMembersButton.setAlignment(SWT.RIGHT);
     					ensCompGuiList.get(index).isExpanded = false;
     					((GridData)ensCompGuiList.get(index).memberComp.getLayoutData()).exclude = true;
//     	    			ensCompGuiList.get(index).memberComp.setVisible(false);
     					updateModelListControls ();
     				} else {
     					ensCompGuiList.get(index).expandMembersButton.setAlignment(SWT.DOWN);
     					ensCompGuiList.get(index).isExpanded = true;
     					((GridData)ensCompGuiList.get(index).memberComp.getLayoutData()).exclude = false;
//     	    			ensCompGuiList.get(index).memberComp.setVisible(true);
     					updateModelListControls ();
     				}	     			
     			}
     		});
     	} 
     	else {
     		Label label = new Label(composite, SWT.NONE);
     		label.setText("       ");
     	}
     	
     	
     	/*
     	 * For ensemble member
     	 */
     	if (isMember) {
     		// Model Name
     		isChecked = false;
     		if ( ensCompGuiList.get(index).members[memberIndex].modelNameButton != null ) {
     			isChecked = ensCompGuiList.get(index).members[memberIndex].modelNameButton.getSelection();
     			ensCompGuiList.get(index).members[memberIndex].modelNameButton.dispose();
     		}
     		ensCompGuiList.get(index).members[memberIndex].modelNameButton = new Button(composite, SWT.CHECK);
     		ensCompGuiList.get(index).members[memberIndex].modelNameButton.setText("   :"+ensCompGuiList.get(index).members[memberIndex].memberName);
     		ensCompGuiList.get(index).members[memberIndex].modelNameButton.setLayoutData(new GridData(100, SWT.DEFAULT));
     		ensCompGuiList.get(index).members[memberIndex].modelNameButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				updateSelectedModels();
     			}
     		});
     		ensCompGuiList.get(index).members[memberIndex].modelNameButton.setSelection(isChecked);
     		
     		// isFirst button
     		isChecked = false;
     		if ( ensCompGuiList.get(index).members[memberIndex].isPrimaryButton != null) {
     			isChecked = ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.getSelection();
     			ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.dispose();
     		}
     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton = new Button(composite, SWT.CHECK);
     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.setText("      ");
     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.getSelection()) {
     					processFirstButton(index, memberIndex);
     					updateSelectedModels();
     				}	
     			}
     		});
     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.setSelection(isChecked);

     		// TODO : Remove this if we are going to keep the primaryModel parameter in the Resource Definition.
     		// for now this is just turned off.
//     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.setEnabled( false );
//     		ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.setVisible( false );

     		
     		naCycleCnts = 0;
     		// weight at each cycle
     		for (int jj = 0; jj < MaxNumOfEnsembleCycles; jj++) {
     			String text ="";
     			if ( ensCompGuiList.get(index).members[memberIndex].weightText[jj] != null ) {
     				text = ensCompGuiList.get(index).members[memberIndex].weightText[jj].getText().trim();
     				ensCompGuiList.get(index).members[memberIndex].weightText[jj].dispose();
     			}
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj] = new Text(composite, SWT.SINGLE|SWT.BORDER|SWT.RIGHT);
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj].setTextLimit(2);
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj].setLayoutData(new GridData(18, SWT.DEFAULT));
     			Label percentLabel1 = new Label(composite, SWT.LEFT);
     			percentLabel1.setText("%");
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj].setText(text);
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj].addModifyListener( new ModifyListener() {
     				@Override
     				public void modifyText(ModifyEvent e) {
     					updateSelectedModels();
     				}         			
     			});
     			ensCompGuiList.get(index).members[memberIndex].weightText[jj].addVerifyListener( new VerifyListener() {
     				@Override
     				public void verifyText(VerifyEvent e) {
//     		    		try {
//     		    			if( Integer.parseInt(e.text.toString()) <= 0 ) {
//     		    				e.doit = false;
//     		    			}
//     		    		}
//     		    		catch ( NumberFormatException nfe ) {
//     		    			e.doit = false;
//     		    		}
//
//     					if ( !e.doit ) 
//     						Display.getCurrent().beep();
     				}
     			});
     			

     			isChecked = false;
     			if ( ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj] != null ) {
     				isChecked = ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].getSelection();
     				ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].dispose();
     			}
     			ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj] = new Button(composite, SWT.CHECK);
     			ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].setText( ensCompGuiList.get(index).cyclesLblStr[jj]  );
     			ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].addSelectionListener(new SelectionAdapter() {
     				public void widgetSelected(SelectionEvent event) {
     					updateSelectedModels();
     				}
     			});	
     			ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].setSelection(isChecked);
     			
     			if (ensCompGuiList.get(index).cyclesLblStr[jj].equals(NA_CYCLE)) {
     				naCycleCnts ++;
     				ensCompGuiList.get(index).members[memberIndex].cycleButtons[jj].setEnabled(false);
     				ensCompGuiList.get(index).members[memberIndex].weightText[jj].setEnabled(false);
     			}
         	}
     		
     		if ( naCycleCnts == MaxNumOfEnsembleCycles ) {
     			ensCompGuiList.get(index).members[memberIndex].modelNameButton.setSelection(false);
     			ensCompGuiList.get(index).members[memberIndex].isPrimaryButton.setSelection(false);
     		}
     	}
     	
     	/*
     	 * for model
     	 */
     	else {
     		// Model Name
     		isChecked = false;
     		
     		if ( ensCompGuiList.get(index).modelNameButton != null ) {
     			isChecked = ensCompGuiList.get(index).modelNameButton.getSelection();
     			ensCompGuiList.get(index).modelNameButton.dispose();
     		}
     		ensCompGuiList.get(index).modelNameButton = new Button(composite, SWT.CHECK);
     		ensCompGuiList.get(index).modelNameButton.setText(ensCompGuiList.get(index).getEnsCompName());
     		ensCompGuiList.get(index).modelNameButton.setLayoutData(new GridData(100, SWT.DEFAULT));
     		ensCompGuiList.get(index).modelNameButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				updateSelectedModels();
     			}
     		});	
     		ensCompGuiList.get(index).modelNameButton.setSelection(isChecked);
     		// isFirst button
     		isChecked = false;
     		if ( ensCompGuiList.get(index).isPrimaryButton != null ) {
     			isChecked = ensCompGuiList.get(index).isPrimaryButton.getSelection();
     			ensCompGuiList.get(index).isPrimaryButton.dispose();
     		}
     		ensCompGuiList.get(index).isPrimaryButton = new Button(composite, SWT.CHECK);
     		ensCompGuiList.get(index).isPrimaryButton.setText("      ");
     		ensCompGuiList.get(index).isPrimaryButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensCompGuiList.get(index).isPrimaryButton.getSelection()) {
     					processFirstButton(index, -1);
     					updateSelectedModels();
     				}
     			}
     		});	
     		ensCompGuiList.get(index).isPrimaryButton.setSelection(isChecked);

     		// TODO : Remove this if we are going to keep the primaryModel parameter in the Resource Definition.
     		// for now this is just turned off.
//     		ensCompGuiList.get(index).isPrimaryButton.setEnabled( false );
//     		ensCompGuiList.get(index).isPrimaryButton.setVisible( false );

     		naCycleCnts = 0;
     		// weight at each cycle
     		for (int jj = 0; jj < MaxNumOfEnsembleCycles; jj++) {
     			String text ="";
     			if ( ensCompGuiList.get(index).weightText[jj] != null) {
     				text = ensCompGuiList.get(index).weightText[jj].getText().trim();
     				ensCompGuiList.get(index).weightText[jj].dispose();
     			}
     			ensCompGuiList.get(index).weightText[jj] = new Text(composite, SWT.SINGLE|SWT.BORDER|SWT.RIGHT);
     			ensCompGuiList.get(index).weightText[jj].setTextLimit(2);
     			ensCompGuiList.get(index).weightText[jj].setLayoutData(new GridData(18, SWT.DEFAULT));
     			Label percentLabel1 = new Label(composite, SWT.LEFT);
     			percentLabel1.setText("%");
     			ensCompGuiList.get(index).weightText[jj].addModifyListener( new ModifyListener() {
     				@Override
					public void modifyText(ModifyEvent e) {
						updateSelectedModels();
					}         			
         		});
     			ensCompGuiList.get(index).weightText[jj].addVerifyListener( new VerifyListener() {
     				@Override
     				public void verifyText(VerifyEvent e) {
//     		    		try {
//     		    			if( Integer.parseInt(e.text.toString()) <= 0 ) {
//     		    				e.doit = false;
//     		    			}
//     		    		}
//     		    		catch ( NumberFormatException nfe ) {
//     		    			e.doit = false;
//     		    		}
//
//     					if ( !e.doit ) 
//     						Display.getCurrent().beep();
     				}
         		});
     			ensCompGuiList.get(index).weightText[jj].setText(text);
     			
     			isChecked = false;
     			if ( ensCompGuiList.get(index).cycleButtons[jj] != null ) {
     				isChecked = ensCompGuiList.get(index).cycleButtons[jj].getSelection();
     				ensCompGuiList.get(index).cycleButtons[jj].dispose();
     			}
     			ensCompGuiList.get(index).cycleButtons[jj] = new Button(composite, SWT.CHECK);
     			ensCompGuiList.get(index).cycleButtons[jj].setText(ensCompGuiList.get(index).cyclesLblStr[jj] );
     			ensCompGuiList.get(index).cycleButtons[jj].addSelectionListener(new SelectionAdapter() {
     				public void widgetSelected(SelectionEvent event) {
     					updateSelectedModels();
     				}
     			});	
     			ensCompGuiList.get(index).cycleButtons[jj].setSelection(isChecked);
     			
     			if (ensCompGuiList.get(index).cyclesLblStr[jj].equals(NA_CYCLE)) {
     				naCycleCnts ++;
     				ensCompGuiList.get(index).cycleButtons[jj].setEnabled(false);
     				ensCompGuiList.get(index).weightText[jj].setEnabled(false);
     			}
     		}
     		
     		if ( naCycleCnts == MaxNumOfEnsembleCycles ) {
     			ensCompGuiList.get(index).modelNameButton.setSelection(false);
     			ensCompGuiList.get(index).isPrimaryButton.setSelection(false);
     		}
     	}
     }
    
    private int getTotalModelNumber() {
		int ret = ensCompGuiList.size();
		for (int i = 0; i < ensCompGuiList.size(); i++) {
			if (ensCompGuiList.get(i).hasMembers) ret += ensCompGuiList.get(i).memberCount;
		}
		
		return ret;
	}
  
    private void clearAll() {
    	for (int i = 0; i < ensCompGuiList.size(); i++) {
    		
    			ensCompGuiList.get(i).modelNameButton.setSelection(false);
    			ensCompGuiList.get(i).isPrimaryButton.setSelection(false);
    			ensCompGuiList.get(i).isPrimary = false;
    		
    		for (int jj = 0; jj < 4; jj++) {
    			ensCompGuiList.get(i).weightText[jj].setText("");
    			ensCompGuiList.get(i).cycleButtons[jj].setSelection(false);
    		}
    		
    		if (ensCompGuiList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensCompGuiList.get(i).memberCount; jj++) {
    				ensCompGuiList.get(i).members[jj].modelNameButton.setSelection(false);
    				ensCompGuiList.get(i).members[jj].isPrimaryButton.setSelection(false);
    				ensCompGuiList.get(i).members[jj].isPrimary = false;
    				
    				for (int kk = 0; kk < MaxNumOfEnsembleCycles ; kk++) {
    					ensCompGuiList.get(i).members[jj].weightText[kk].setText("");
    					ensCompGuiList.get(i).members[jj].cycleButtons[kk].setSelection(false);
    				}
    			}
    		}
    	}
    	
    	selectedModelText.setText("");
    }
    
    // set the GUI with data saved in the ensCompList 
    //
    private void setEnsembleSelectionGUI() {
    	if (ensCompList == null) 
    		return;
    	
    	boolean firstDone = false;
    	for(int i = 0; i < ensCompList.size(); i++) {
    		
    		EnsComp ensComp = ensCompList.get(i);
    		boolean modelFound = false;
    		
    		for(int j = 0; j < ensCompGuiList.size(); j++) {
    		
    			if( modelFound ) {
    				break;
    			}
    			EnsembleCompGuiData ensGuiData = ensCompGuiList.get(j);
    			
    			if (ensComp.getEnsCompName().equalsIgnoreCase( ensGuiData.getEnsCompName() )) {
    				
    				modelFound = true;

//    				setFirstButtons(j,-1);
    				
    				// if this cycle time 
    				if (ensComp.getCycleTime() != null) {
    					ensGuiData.modelNameButton.setSelection(true);
        				
        				if (!firstDone) {
        					ensGuiData.isPrimaryButton.setSelection(true);
        					firstDone = true;
        				}
        				
    					for (int k = 0; k < 4; k++) {
        					
    						Date kCycleTime = ensGuiData.cycleTimes[k];

        					if( kCycleTime != null &&
        						kCycleTime.getTime() == ensComp.getCycleTime().getTime() ) {
        						
//    						if (ensGuiData.cyclesLblStr[k].contains(
//    								modelInfo.getCycle())) {
    							
    							ensGuiData.cycleButtons[k].setSelection(true);
    							
    							if (ensComp.getWeight() > 0 && ensComp.getWeight() <= 100) {
    								ensGuiData.weightText[k].setText( String.valueOf(ensComp.getWeight()));
    							}
    							break;
    						}
    					}
    				}
    				else {
    					ensGuiData.modelNameButton.setSelection(false);
    					ensGuiData.isPrimaryButton.setSelection(false);
    				}
    			}
    			else if (ensGuiData.hasMembers && ensComp.getEnsCompName().contains(":")) {

    				boolean isModelEnable = false;
    				for (int mm = 0; mm < ensGuiData.memberCount; mm++) {
    					
    					if (ensComp.getEnsCompName().equalsIgnoreCase(ensGuiData.members[mm].getEnsCompName())) {
    						
    						modelFound = true;
    						
    						// setFirstButtons(j,-1);

    						if (ensComp.getCycleTime() != null) {
    							isModelEnable = true;
    							ensGuiData.members[mm].modelNameButton.setSelection(true);
        						
    							if (!firstDone) {
    								ensGuiData.members[mm].isPrimaryButton.setSelection(true);
    								firstDone = true;
    							}
    							for (int k = 0; k < 4; k++) {
            						Date kCycleTime = ensGuiData.cycleTimes[k];

    								//if (ensGuiData.members[mm].cyclesLblStr[k].contains(modelInfo.getCycle())) {
    								if( kCycleTime != null &&
    									kCycleTime.getTime() == ensComp.getCycleTime().getTime() ) {
    									
    									ensGuiData.members[mm].cycleButtons[k].setSelection(true);
    									
    									if (ensComp.getWeight() > 0 && ensComp.getWeight() <= 100) {
    										ensGuiData.members[mm].weightText[k].setText(String.valueOf(ensComp.getWeight()));
    									}
    									break;
    								}
    							}
    						}
    						else {
    							ensGuiData.members[mm].modelNameButton.setSelection(false);
    							ensGuiData.members[mm].isPrimaryButton.setSelection(false);
    						}
    					}
    				}
    				
    				if ( ! isModelEnable ) {
    					ensGuiData.modelNameButton.setSelection(false);
    					ensGuiData.isPrimaryButton.setSelection(false);
    				}
    			}
    		}
    		if( !modelFound ) {
    			System.out.println("Warning: model, "+ensComp.getEnsCompName()+" not found in list of models for this Ensemble.");
    		}
    	}
    	
    	// compute the GDFILE string  and set it here
//    	selectedModelText.setText();
    }
    
    /*
     * Only allow ONE 'isFirst' button to be selected
     */
    private void processFirstButton(int index, int memberIndex) {
    	for (int i = 0; i < ensCompGuiList.size(); i++) {
    		if (ensCompGuiList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensCompGuiList.get(i).memberCount; jj++) {
    				if ( !(i == index && jj == memberIndex))
    					ensCompGuiList.get(i).members[jj].isPrimaryButton.setSelection(false);
    			}
    		}
    		
    		if ( i != index || (i == index && memberIndex >= 0)) {
    			ensCompGuiList.get(i).isPrimaryButton.setSelection(false);
    		}	
    	}
    }
    
    // set the ensCompData with selections from the GUI.
    //
    private void updateSelectedModels() {
    	ensCompData.reset();
    	    	
    	for (int i = 0; i < ensCompGuiList.size(); i++) {
    		String ensCompName = null;
        	int wt = -1,j;
        	Date cyc = null;
        	boolean first = false;
        	
    		if (ensCompGuiList.get(i).modelNameButton.getSelection() ) {
    			if ( ! avariableCycles (i)) {
    				ensCompGuiList.get(i).modelNameButton.setSelection(false);
    				continue;
    			}
    			ensCompName = ensCompGuiList.get(i).getEnsCompName();
    			if (ensCompGuiList.get(i).isPrimaryButton.getSelection())  
    				first = true;
    			
    			boolean cycleFlag = false;
    			for (j = 0; j < MaxNumOfEnsembleCycles ; j++) {
    				wt = -1;
    				
    				if (ensCompGuiList.get(i).cycleButtons[j].getSelection()) {
    				
    					if (ensCompGuiList.get(i).weightText[j].getText() != null &&
    							ensCompGuiList.get(i).weightText[j].getText().trim().length() > 0) {
    						int w = Integer.valueOf(ensCompGuiList.get(i).weightText[j].getText());
    						if (w > 0 && w <= 100) 
    							wt = w;
    					}	
    					
    					if (ensCompGuiList.get(i).cyclesLblStr[j] != null) {
    						cyc = ensCompGuiList.get(i).cycleTimes[j]; //cyclesLblStr[j];
    					}
    					
    					cycleFlag = true;

    					ensCompData.addModel(ensCompName, wt, cyc, first);
    					
    				}
    			}
    			
    			if (!cycleFlag) {
    				ensCompData.addModel(ensCompName, wt, cyc, first);
    			}
    			
    		}
    		else {
    			if (ensCompGuiList.get(i).isPrimaryButton.getSelection()) {
    				ensCompGuiList.get(i).isPrimaryButton.setSelection(false);
    				ensCompGuiList.get(i).isPrimary = false;
    			}
    			for (j = 0; j < MaxNumOfEnsembleCycles ; j++) {
					if (ensCompGuiList.get(i).cycleButtons[j].getSelection()) {
						ensCompGuiList.get(i).cycleButtons[j].setSelection(false);
					}
					if (ensCompGuiList.get(i).weightText[j].getText() != null &&
							ensCompGuiList.get(i).weightText[j].getText().trim().length() > 0) {
						ensCompGuiList.get(i).weightText[j].setText("");
					}
				}
    		}
    		
    		if (ensCompGuiList.get(i).hasMembers) {
    			if ( ! ensCompGuiList.get(i).modelNameButton.getSelection() ) {
    				for (int jj = 0; jj < ensCompGuiList.get(i).memberCount; jj++) {
    					ensCompName = null;
    					wt = -1;
    					cyc = null;
    					first = false;
    				
    					if (ensCompGuiList.get(i).members[jj].modelNameButton.getSelection()) {
    						ensCompName = ensCompGuiList.get(i).members[jj].getEnsCompName();
    					
    						if (ensCompGuiList.get(i).members[jj].isPrimaryButton.getSelection()) {
    							first = true;
    						}
    	    			
    						boolean cycleFlag = false;
    					
    						for (j = 0; j < MaxNumOfEnsembleCycles ; j++) {
    	    				
    							if (ensCompGuiList.get(i).members[jj].cycleButtons[j].getSelection()) {
    	    					
    								String weightStr = ensCompGuiList.get(i).members[jj].weightText[j].getText();
    								wt = -1;

    								if (weightStr != null && weightStr.trim().length() > 0) {
    	    					
    									int w = Integer.valueOf(weightStr.trim());
    	    						
    									if (w > 0 && w <= 100) {
    										wt = w;
    									}
    								}	
    	    					
    								if (ensCompGuiList.get(i).cyclesLblStr[j] != null) 
    									cyc = ensCompGuiList.get(i).cycleTimes[j];//cyclesLblStr[j];
    	    					
    								cycleFlag = true;

    								ensCompData.addModel(ensCompName, wt, cyc, first);
    							}
    						}
    	    			
    						if (!cycleFlag) {
    							ensCompData.addModel(ensCompName, wt, cyc, first);    	    				
    						}
    					}
    					else {
    						clearMember (i, jj);
    					}
    				}
    			}
    			else {
    	    		for (int jj = 0; jj < ensCompGuiList.get(i).memberCount; jj++) {
    	    			clearMember (i,jj);
    	    		}
    	    	}
    
    		}
    		
    		ensCompData.setSelectedModelStrings();
    		
    		selectedModelText.setText(ensCompData.getEnsCompsStringForRefTime());
    		
    		ensembleComponentWeightsAttr.setAttrValue( ensCompData.getEnsCompsStringWithRelativeCycTimes() );
    	}    	
    }
    
    private boolean avariableCycles (int memberId) {
    	boolean hasCycles = false;
    	for ( int j = 0 ; j < MaxNumOfEnsembleCycles; j ++) {
    		if ( ! ensCompGuiList.get(memberId).cyclesLblStr[j].equals(NA_CYCLE) ) {
    			hasCycles = true;
    			break;
    		}
    	}
    	return hasCycles;
    }
    
    private void clearMember (int model, int member ) {
    	if (ensCompGuiList.get(model).members[member].modelNameButton.getSelection()) {
			ensCompGuiList.get(model).members[member].modelNameButton.setSelection(false);
		}
		if (ensCompGuiList.get(model).members[member].isPrimaryButton.getSelection()) {
			ensCompGuiList.get(model).members[member].isPrimaryButton.setSelection(false);
			ensCompGuiList.get(model).members[member].isPrimary = false;
		}
		for (int j = 0; j < MaxNumOfEnsembleCycles ; j++) {
			if (ensCompGuiList.get(model).members[member].cycleButtons[j].getSelection()) {
				ensCompGuiList.get(model).members[member].cycleButtons[j].setSelection(false);
			}
			if ( ensCompGuiList.get(model).members[member].weightText[j].getText() != null && 
				ensCompGuiList.get(model).members[member].weightText[j].getText().trim().length() > 0 ) {
				ensCompGuiList.get(model).members[member].weightText[j].setText("");
			}
		}
    }
    
    private void updateModelListControls() {
    	
    	int cnt = 0;
        Composite modelListComp = new Composite(scrolledComposite, SWT.NONE);
        
        GridLayout gl = new GridLayout(1, false);
        modelListComp.setLayout(gl);
        
        for (int i = 0; i < ensCompGuiList.size(); i++) {
        	Composite comp = new Composite(modelListComp, SWT.NONE);
        	comp.setLayout(new GridLayout(15, false));
        	comp.setLayoutData(new GridData());//WINDOW_WIDTH, SWT.DEFAULT));
        	
        	createWidget(i, comp, false, -1);
        	modelListComp.pack();

        	cnt ++;
        	if (ensCompGuiList.get(i).hasMembers &&
        			ensCompGuiList.get(i).isExpanded ) {
        		ensCompGuiList.get(i).memberComp = new Composite(modelListComp, SWT.NONE);
        		ensCompGuiList.get(i).memberComp.setLayout(new GridLayout(15, false));
        		GridData griddata = new GridData(WINDOW_WIDTH, ensCompGuiList.get(i).memberCount * HEIGHT_PER_LINE);
        		griddata.verticalIndent = 0;
        		griddata.grabExcessVerticalSpace = true;
        		griddata.verticalAlignment = SWT.TOP;
            	ensCompGuiList.get(i).memberComp.setLayoutData(griddata);
        		for(int kk = 0; kk < ensCompGuiList.get(i).memberCount; kk++) {
        			createWidget(i, ensCompGuiList.get(i).memberComp, true, kk);
     				modelListComp.pack();
     				cnt ++;
        		}
        		
        		((GridData)ensCompGuiList.get(i).memberComp.getLayoutData()).exclude = false;
    			ensCompGuiList.get(i).memberComp.setVisible(true);
        	}
        	
        }
        
        modelListComp.setSize(WINDOW_WIDTH, cnt*HEIGHT_PER_LINE);
        scrolledComposite.setContent(modelListComp);
        
    }

}
