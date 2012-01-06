package gov.noaa.nws.ncep.viz.rsc.ncgrid.rsc;

import gov.noaa.nws.ncep.viz.common.ui.ModelListInfo;
import gov.noaa.nws.ncep.viz.common.ui.ModelListInfo.ModelInfo;
import gov.noaa.nws.ncep.viz.localization.impl.LocalizationManager;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.ScrolledComposite;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 05/2010  	277        	M. Li    	Initial creation 
 * 
 * </pre>
 * 
 * @author mli
 * @version 1.0
 * 
 */

public class EnsembleSelectDialog extends Dialog {

	/**
     * Dialog shell;
     */
    private Shell shell;
    
    /**
     * SWT display component.
     */
    private Display display;

    /**
     * Label font.
     */
    private Font labelFont;
    
    private final int WINDOW_WIDTH = 860;
    private final int HEIGHT_PER_LINE = 41;
    private final String NA_CYCLE = " N/A ";
    
    private Text selectedModelText;
    
    private Composite modelListComp;
    
    private String modelListString;
    
    private String modelListString0;
    
    
    private List<ModelInfo> modelInfoList;
    
    private ModelListInfo modelListInfo;
    
	public EnsembleSelectDialog(Shell parentShell, String model_group) {
		super(parentShell);
//		if (model_group.contains("GEFS_TEST"))
//			modelListString = "{50%gefs|00,gfs|06,gfs|12,gfs|18}";
//		else 
//			modelListString = "{nam}";
		
		this.modelListString = model_group;
		this.modelListString0 = model_group;
		ModelListInfo modelListInfo = new ModelListInfo(modelListString);
		modelInfoList = modelListInfo.getModelList();
	}


	private List<EnsembleInfo> ensInfoList;
	
	private class EnsembleInfo {
		private int cycleNumber = 4;
		private String name;
		private boolean isFirst;
		private boolean hasMembers;
		private boolean isExpanded = true;
		private int memberNumber;
		EnsembleInfo[] members;
		private String[] cycles = new String[cycleNumber];
		
		Composite composite;
		Composite memberComp;
		Button expandMembersButton;
		Button modelNameButton;
		Button isFirstButton;
		
		Text[] weightText = new Text[cycleNumber];
		Button[] cycleButtons = new Button[cycleNumber];
		
	}
	
    /**
     * Open method used to display the locator edit dialog. *
     * 
     * @return Return object (can be null).
     */
    public Object open() {
        Shell parent = getParent();
        display = parent.getDisplay();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.APPLICATION_MODAL);
        shell.setText("Ensemble Selection Window");

        // Create the main layout for the shell.
        GridLayout mainLayout = new GridLayout(1, false);
        mainLayout.marginHeight = 3;
        mainLayout.marginWidth = 3;
        shell.setLayout(mainLayout);
        Rectangle rect = parent.getBounds();
//        Point pt = new Point(0, rect.y + rect.height - 100);
        // align the bottom left of the dialog with the bottom left of Cave.
        shell.setLocation( rect.x+20, rect.y + rect.height - shell.getSize().y-40 );

        labelFont = new Font(shell.getDisplay(), "Arial", 10, SWT.BOLD);

        // Initialize all of the menus, controls, and layouts
        initializeComponents(true);
        
        
        // Pack the controls and open the display.
        shell.pack();
        shell.open();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

        labelFont.dispose();

        return null;
    }

    /**
     * Initialize the dialog components.
     */
    private void initializeComponents(boolean firstTime) {
    	if (firstTime)readEnsemblTable();

    	createHeader();
//    	addSeparator();
    	createModelListControls();
    	createModelSelectionControls();
    	addSeparator();
    	
    	createCloseButton();
    	
    	setEnsembleSelectionGUI();
    }
    
    
    private void readEnsemblTable() {
    	
    	String table_name = LocalizationManager.getInstance().getFilename("ensembleTable");
    	try {
			BufferedReader input = new BufferedReader(new FileReader(new File(table_name)));
			String lineStr = null;
			ensInfoList = new ArrayList<EnsembleInfo>();
			
			try {
				while( (lineStr=input.readLine()) != null ) {
					if( lineStr.startsWith("!") ) {
						continue;
					}
					
					String[] lv = lineStr.trim().split("\\s+");
					// Has members
					if (lv.length == 2) {
						String modelname = lv[0].trim();
		        		String memberStr = lv[1].trim();
		        		String[] members = memberStr.trim().split(";");
		        		
		        		EnsembleInfo ensInfo1 = new EnsembleInfo();
		            	ensInfo1.name = modelname;
		            	ensInfo1.hasMembers = true;
		            	ensInfo1.isFirst = false;
		            	ensInfo1.memberNumber = members.length;
		            	
		            	ArrayList<String> cycles = ModelListInfo.getCycles(modelname);
		            	ensInfo1.cycles = new String[ensInfo1.cycleNumber];
		            	for (int i = 0; i < ensInfo1.cycleNumber; i++ ) {
		            		if (i < cycles.size()) {
		            			ensInfo1.cycles[i] = cycles.get(i);
		            		}
		            		else {
		            			ensInfo1.cycles[i] = NA_CYCLE;
		            		}
		            	}
		            	
		            	
		            	ensInfo1.members = new EnsembleInfo[ensInfo1.memberNumber];
		            	for (int i = 0; i < ensInfo1.memberNumber; i++) {
		            		ensInfo1.members[i] = new EnsembleInfo();
		                	ensInfo1.members[i].name = members[i].trim();
		                	ensInfo1.members[i].hasMembers = false;
		                	ensInfo1.members[i].isFirst = false;
		            	}
		            	ensInfoList.add(ensInfo1);
					}
					else if (lv.length == 1) {
						EnsembleInfo ensInfo2 = new EnsembleInfo();
				    	ensInfo2.name = lv[0].trim();
				    	ensInfo2.hasMembers = false;
				    	ensInfo2.isFirst = false;
				    	
				    	ArrayList<String> cycles = ModelListInfo.getCycles(ensInfo2.name);
				    	ensInfo2.cycles = new String[ensInfo2.cycleNumber];
		            	for (int i = 0; i < ensInfo2.cycleNumber; i++ ) {
		            		if (i < cycles.size()) {
		            			ensInfo2.cycles[i] = cycles.get(i);
		            		}
		            		else {
		            			ensInfo2.cycles[i] = NA_CYCLE;
		            		}
		            	}
				    	
				    	ensInfoList.add(ensInfo2);
				    	
				    	
						
					}
				}
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}	
			
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
    	
    }
    
    private void createHeader() {
    	Composite comp = new Composite(shell, SWT.NONE);
    	GridLayout gl = new GridLayout(1, false);
    	comp.setLayout(gl);
    	
    	GridData gd = new GridData(WINDOW_WIDTH, SWT.DEFAULT);
    	Label label = new Label(comp, SWT.NONE);
    	label.setLayoutData(gd);
    	label.setText("       Models                                First                    Cycle1                           Cycle2                          Cycle3                           Cycle4");
    }
    /**
     * Create the locatorSelection controls.
     */
    private void createModelSelectionControls() {
        Composite modelSelectionComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(2, false);
        modelSelectionComp.setLayout(gl);

        GridData gd = new GridData(120, SWT.DEFAULT);
        Label label = new Label(modelSelectionComp, SWT.NONE);
        label.setText("Selected Models:");
        label.setLayoutData(gd);

        
        selectedModelText = new Text(modelSelectionComp, SWT.SINGLE|SWT.BORDER);
        selectedModelText.setLayoutData(new GridData(700, SWT.DEFAULT));
        
        
       
    }

   
    private void createModelListControls() {
    	
    	ScrolledComposite scrolledComposite = new ScrolledComposite(shell, SWT.V_SCROLL|SWT.BORDER);
    	scrolledComposite.setLayout(new GridLayout(1, true));
    	GridData gd = new GridData(SWT.FILL, SWT.FILL, true, true);
        gd.heightHint = 8 * HEIGHT_PER_LINE;
        gd.widthHint = WINDOW_WIDTH;
        scrolledComposite.setLayoutData(gd);
    	
        modelListComp = new Composite(scrolledComposite, SWT.NONE);
        
        GridLayout gl = new GridLayout(1, false);
        modelListComp.setLayout(gl);
        
        for (int i = 0; i < ensInfoList.size(); i++) {
        	ensInfoList.get(i).composite = new Composite(modelListComp, SWT.NONE);
        	ensInfoList.get(i).composite.setLayout(new GridLayout(15, false));
        	ensInfoList.get(i).composite.setLayoutData(new GridData(WINDOW_WIDTH, SWT.DEFAULT));
        	
        	createWidget(i, ensInfoList.get(i).composite, false, -1);
        	
        	if (ensInfoList.get(i).hasMembers) {
        		ensInfoList.get(i).memberComp = new Composite(modelListComp, SWT.NONE);
        		ensInfoList.get(i).memberComp.setLayout(new GridLayout(15, false));
        		GridData griddata = new GridData(WINDOW_WIDTH, ensInfoList.get(i).memberNumber * HEIGHT_PER_LINE);
        		griddata.verticalIndent = 0;
        		griddata.grabExcessVerticalSpace = true;
        		griddata.verticalAlignment = SWT.TOP;
            	ensInfoList.get(i).memberComp.setLayoutData(griddata);
        		for(int kk = 0; kk < ensInfoList.get(i).memberNumber; kk++) {
        			createWidget(i, ensInfoList.get(i).memberComp, true, kk);
        		}
        		
        		((GridData)ensInfoList.get(i).memberComp.getLayoutData()).exclude = false;
    			ensInfoList.get(i).memberComp.setVisible(true);
        	}
        	
        }
        
        modelListComp.setSize(WINDOW_WIDTH, getTotalModelNumber()*HEIGHT_PER_LINE);
        scrolledComposite.setContent(modelListComp);
    }
   
     private void createWidget(final int index, Composite composite, boolean isMember, final int memberIndex) {
    	
    	 //Arrow Button to turn on/off ensemble members
    	boolean hasMembers = ensInfoList.get(index).hasMembers;
    	if (isMember) hasMembers = false;
     	if (hasMembers) {
     		ensInfoList.get(index).expandMembersButton = new Button(composite, SWT.ARROW|SWT.DOWN);
     		ensInfoList.get(index).expandMembersButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensInfoList.get(index).expandMembersButton.getAlignment() == SWT.DOWN) {
     					ensInfoList.get(index).expandMembersButton.setAlignment(SWT.RIGHT);
     					ensInfoList.get(index).isExpanded = false;
     					((GridData)ensInfoList.get(index).memberComp.getLayoutData()).exclude = true;
     	    			ensInfoList.get(index).memberComp.setVisible(false);
     				} else {
     					ensInfoList.get(index).expandMembersButton.setAlignment(SWT.DOWN);
     					ensInfoList.get(index).isExpanded = true;
     					((GridData)ensInfoList.get(index).memberComp.getLayoutData()).exclude = false;
     	    			ensInfoList.get(index).memberComp.setVisible(true);
     				}	
     			
     				modelListComp.pack();
     			}
     		});
     	} else {
     		Label label = new Label(composite, SWT.NONE);
     		label.setText("       ");
     	}
     	
     	
     	/*
     	 * For ensemble member
     	 */
     	if (isMember) {
     		// Model Name 
     		ensInfoList.get(index).members[memberIndex].modelNameButton = new Button(composite, SWT.CHECK);
     		ensInfoList.get(index).members[memberIndex].modelNameButton.setText(ensInfoList.get(index).members[memberIndex].name);
     		ensInfoList.get(index).members[memberIndex].modelNameButton.setLayoutData(new GridData(160, SWT.DEFAULT));
     		ensInfoList.get(index).members[memberIndex].modelNameButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				updateSelectedModels();
     			}
     		});	
     		
     		// isFirst button
     		ensInfoList.get(index).members[memberIndex].isFirstButton = new Button(composite, SWT.CHECK);
     		ensInfoList.get(index).members[memberIndex].isFirstButton.setText("      ");
     		ensInfoList.get(index).members[memberIndex].isFirstButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensInfoList.get(index).members[memberIndex].isFirstButton.getSelection()) {
     					processFirstButton(index, memberIndex);
     					updateSelectedModels();
     				}	
     			}
     		});	
     		
     		// weight at each cycle
     		for (int jj = 0; jj < ensInfoList.get(index).members[memberIndex].cycleNumber; jj++) {
         		ensInfoList.get(index).members[memberIndex].weightText[jj] = new Text(composite, SWT.SINGLE|SWT.BORDER|SWT.RIGHT);
         		ensInfoList.get(index).members[memberIndex].weightText[jj].setTextLimit(3);
         		ensInfoList.get(index).members[memberIndex].weightText[jj].setLayoutData(new GridData(30, SWT.DEFAULT));
         		Label percentLabel1 = new Label(composite, SWT.LEFT);
         		percentLabel1.setText("%");
         		ensInfoList.get(index).members[memberIndex].weightText[jj].addModifyListener( new ModifyListener() {

					@Override
					public void modifyText(ModifyEvent e) {
						updateSelectedModels();
					}
         			
         		});

         		ensInfoList.get(index).members[memberIndex].cycleButtons[jj] = new Button(composite, SWT.CHECK);
         		ensInfoList.get(index).members[memberIndex].cycleButtons[jj].setText(ensInfoList.get(index).cycles[jj]+ "      ");
         		ensInfoList.get(index).members[memberIndex].cycleButtons[jj].addSelectionListener(new SelectionAdapter() {
         			public void widgetSelected(SelectionEvent event) {
         				updateSelectedModels();
         			}
         		});	
         		
         		if (ensInfoList.get(index).cycles[jj].equals(NA_CYCLE)) {
         			ensInfoList.get(index).members[memberIndex].cycleButtons[jj].setEnabled(false);
         			ensInfoList.get(index).members[memberIndex].weightText[jj].setEnabled(false);
         		}
         	}
     	}
     	
     	/*
     	 * for model
     	 */
     	else {
     		// Model Name 
     		ensInfoList.get(index).modelNameButton = new Button(composite, SWT.CHECK);
     		ensInfoList.get(index).modelNameButton.setText(ensInfoList.get(index).name);
     		ensInfoList.get(index).modelNameButton.setLayoutData(new GridData(160, SWT.DEFAULT));
     		ensInfoList.get(index).modelNameButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				updateSelectedModels();
     			}
     		});	

     		// isFirst button
     		ensInfoList.get(index).isFirstButton = new Button(composite, SWT.CHECK);
     		ensInfoList.get(index).isFirstButton.setText("      ");
     		ensInfoList.get(index).isFirstButton.addSelectionListener(new SelectionAdapter() {
     			public void widgetSelected(SelectionEvent event) {
     				if (ensInfoList.get(index).isFirstButton.getSelection()) {
     					processFirstButton(index, -1);
     					updateSelectedModels();
     				}
     			}
     		});	


     		// weight at each cycle
     		for (int jj = 0; jj < ensInfoList.get(index).cycleNumber; jj++) {
     			ensInfoList.get(index).weightText[jj] = new Text(composite, SWT.SINGLE|SWT.BORDER|SWT.RIGHT);
     			ensInfoList.get(index).weightText[jj].setTextLimit(3);
     			ensInfoList.get(index).weightText[jj].setLayoutData(new GridData(30, SWT.DEFAULT));
     			Label percentLabel1 = new Label(composite, SWT.LEFT);
     			percentLabel1.setText("%");
     			ensInfoList.get(index).weightText[jj].addModifyListener( new ModifyListener() {

					@Override
					public void modifyText(ModifyEvent e) {
						updateSelectedModels();
					}
         			
         		});


     			ensInfoList.get(index).cycleButtons[jj] = new Button(composite, SWT.CHECK);
     			ensInfoList.get(index).cycleButtons[jj].setText(ensInfoList.get(index).cycles[jj]+ "      ");
     			ensInfoList.get(index).cycleButtons[jj].addSelectionListener(new SelectionAdapter() {
         			public void widgetSelected(SelectionEvent event) {
         				updateSelectedModels();
         			}
         		});	
     			
     			if (ensInfoList.get(index).cycles[jj].equals(NA_CYCLE)) {
     				ensInfoList.get(index).cycleButtons[jj].setEnabled(false);
     				ensInfoList.get(index).weightText[jj].setEnabled(false);
     			}
     		}
     	}
     }
    
    /**
     * Add a horizontal separator to the display.
     */
    private void addSeparator() {
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        Label sepLbl = new Label(shell, SWT.SEPARATOR | SWT.HORIZONTAL);
        sepLbl.setLayoutData(gd);
    }
    
    /**
     * Create control buttons.
     */
    private void createCloseButton() {
        Composite centeredComp = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        centeredComp.setLayout(gl);
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
//        gd.horizontalSpan = 2;
        centeredComp.setLayoutData(gd);

        gd = new GridData(200, SWT.DEFAULT);
        
        Button defaultBtn = new Button(centeredComp, SWT.NONE);
        defaultBtn.setText("Accept");
        defaultBtn.setLayoutData(gd);
        defaultBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
              
            	modelListString = selectedModelText.getText().trim();
            	
            	shell.dispose();
            }
        });
        
        Button clearAllBtn = new Button(centeredComp, SWT.NONE);
        clearAllBtn.setText("Clear All");
        clearAllBtn.setLayoutData(gd);
        clearAllBtn.addSelectionListener(new SelectionAdapter() {
            public void widgetSelected(SelectionEvent event) {
            	clearAll();
            }
        });
        
        Button closeBtn = new Button(centeredComp, SWT.NONE);
        closeBtn.setText("Cancel");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		modelListString = modelListString0;
        		shell.dispose();
            }
        	
        });
    }
    
    private int getTotalModelNumber() {
		int ret = ensInfoList.size();
		for (int i = 0; i < ensInfoList.size(); i++) {
			if (ensInfoList.get(i).hasMembers) ret += ensInfoList.get(i).memberNumber;
		}
		
		return ret;
	}
  
    public final boolean isOpen() {
        return shell != null && !shell.isDisposed();
    }
    
    private void clearAll() {
    	for (int i = 0; i < ensInfoList.size(); i++) {
    		
    			ensInfoList.get(i).modelNameButton.setSelection(false);
    			ensInfoList.get(i).isFirstButton.setSelection(false);
    			ensInfoList.get(i).isFirst = false;
    		
    		for (int jj = 0; jj < 4; jj++) {
    			ensInfoList.get(i).weightText[jj].setText("");
    			ensInfoList.get(i).cycleButtons[jj].setSelection(false);
    		}
    		
    		if (ensInfoList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensInfoList.get(i).memberNumber; jj++) {
    				ensInfoList.get(i).members[jj].modelNameButton.setSelection(false);
    				ensInfoList.get(i).members[jj].isFirstButton.setSelection(false);
    				ensInfoList.get(i).members[jj].isFirst = false;
    				
    				for (int kk = 0; kk < ensInfoList.get(i).members[jj].cycleNumber; kk++) {
    					ensInfoList.get(i).members[jj].weightText[kk].setText("");
    					ensInfoList.get(i).members[jj].cycleButtons[kk].setSelection(false);
    				}
    			}
    		}
    	}
    	
    	selectedModelText.setText("");
    }
    
    private void setEnsembleSelectionGUI() {
    	if (modelInfoList == null) return;
    	
    	boolean firstDone = false;
    	for(int i = 0; i < modelInfoList.size(); i++) {
    		for(int j = 0; j < ensInfoList.size(); j++) {
    			if (modelInfoList.get(i).getModelName().equalsIgnoreCase(ensInfoList.get(j).name)) {
    				ensInfoList.get(j).modelNameButton.setSelection(true);
    				if (!firstDone) {
    					ensInfoList.get(j).isFirstButton.setSelection(true);
    					firstDone = true;
    				}
//    				setFirstButtons(j,-1);
    				
    				if (modelInfoList.get(i).getCycle() != null) {
    					for (int k = 0; k < 4; k++) {
    						if (ensInfoList.get(j).cycles[k].contains(modelInfoList.get(i).getCycle())) {
    							ensInfoList.get(j).cycleButtons[k].setSelection(true);
    							if (modelInfoList.get(i).getWeight() > 0 && modelInfoList.get(i).getWeight() <= 100) {
    								ensInfoList.get(j).weightText[k].setText(String.valueOf(modelInfoList.get(i).getWeight()));
    							}
    							break;
    						}
    					}
    				}
    				
    				break;
    			}
    			
    			if (ensInfoList.get(j).hasMembers) {
    				for (int mm = 0; mm < ensInfoList.get(j).memberNumber; mm++) {
    					if (modelInfoList.get(i).getModelName().equalsIgnoreCase(ensInfoList.get(j).members[mm].name)) {
    						ensInfoList.get(j).members[mm].modelNameButton.setSelection(true);
    						if (!firstDone) {
    							ensInfoList.get(j).members[mm].isFirstButton.setSelection(true);
    							firstDone = true;
    						}
    						//    				setFirstButtons(j,-1);

    						if (modelInfoList.get(i).getCycle() != null) {
    							for (int k = 0; k < 4; k++) {
    								if (ensInfoList.get(j).members[mm].cycles[k].contains(modelInfoList.get(i).getCycle())) {
    									ensInfoList.get(j).members[mm].cycleButtons[k].setSelection(true);
    									if (modelInfoList.get(i).getWeight() > 0 && modelInfoList.get(i).getWeight() <= 100) {
    										ensInfoList.get(j).members[mm].weightText[k].setText(String.valueOf(modelInfoList.get(i).getWeight()));
    									}
    									break;
    								}
    							}
    						}

    						break;
    					}
    				}
    			}
    		}
    	}
    	
    	selectedModelText.setText(modelListString);
    }
    
    /*
     * Only allow ONE 'isFirst' button to be selected
     */
    private void processFirstButton(int index, int memberIndex) {
    	for (int i = 0; i < ensInfoList.size(); i++) {
    		if (ensInfoList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensInfoList.get(i).memberNumber; jj++) {
    				if ( !(i == index && jj == memberIndex))
    					ensInfoList.get(i).members[jj].isFirstButton.setSelection(false);
    			}
    		}
    		
    		if ( i != index || (i == index && memberIndex >= 0)) {
    			ensInfoList.get(i).isFirstButton.setSelection(false);
    		}	
    	}
    }
    
    private void updateSelectedModels() {
    	modelListInfo = new ModelListInfo(true);
    	
    	/*
    	 * Get First model
    	 */
    	/*
    	String firstModel = null;
    	String firstCycle = null;
    	int weight = -1;
    	for (int i = 0; i < ensInfoList.size(); i++) {
    		if (ensInfoList.get(i).modelNameButton.getSelection() && 
    			ensInfoList.get(i).isFirstButton.getSelection()) {
    			firstModel = modelInfoList.get(i).getModelName();
    			
    			for (int j = 0; j < ensInfoList.get(i).cycleNumber; j++) {
    				if (ensInfoList.get(i).cycleButtons[j].getSelection()) {
    					if (ensInfoList.get(i).weightText[j].getText() != null && 
    							ensInfoList.get(i).weightText[j].getText().trim().length() > 0) {
    						int w = Integer.valueOf(ensInfoList.get(i).weightText[j].getText());
    						if (w > 0 && w <= 100) weight = w;
    					}
    					if (ensInfoList.get(i).cycles[j] != null) firstCycle = ensInfoList.get(i).cycles[j];
						break;
    				}
    			}
    			
    			
    		}
    		
    		if (ensInfoList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensInfoList.get(i).memberNumber; jj++) {
    				if (!ensInfoList.get(i).modelNameButton.getSelection() &&
    					ensInfoList.get(i).members[jj].isFirstButton.getSelection()) {
    					firstModel = ensInfoList.get(i).members[jj].name;
    	    			
    	    			for (int j = 0; j < ensInfoList.get(i).members[jj].cycleNumber; j++) {
    	    				if (ensInfoList.get(i).members[jj].cycleButtons[j].getSelection()) {
    	    					if (ensInfoList.get(i).members[jj].weightText[j].getText() != null &&
    	    							ensInfoList.get(i).members[jj].weightText[j].getText().trim().length() > 0) {
    	    						int w = Integer.valueOf(ensInfoList.get(i).members[jj].weightText[j].getText());
    	    						if (w > 0 && w <= 100) weight = w;
    	    					}
    	    					if (ensInfoList.get(i).cycles[j] != null) firstCycle = ensInfoList.get(i).cycles[j];
	    						break;
    	    				}
    	    			}
    	    			
    	    			break;
    				}
    			}
    		}
    	}
    	if (firstModel != null) {
    		modelListInfo.addModel(firstModel, weight, firstCycle, true);
    	}	
    	*/
    	/*
    	 * Other models
    	 */
    	
    	for (int i = 0; i < ensInfoList.size(); i++) {
    		String name = null;
        	int wt = -1;
        	String cyc = null;
        	boolean first = false;
        	
    		if (ensInfoList.get(i).modelNameButton.getSelection() ) {
    			name = ensInfoList.get(i).name;
    			if (ensInfoList.get(i).isFirstButton.getSelection()) first = true;
    			
    			boolean cycleFlag = false;
    			for (int j = 0; j < ensInfoList.get(i).cycleNumber; j++) {
    				if (ensInfoList.get(i).cycleButtons[j].getSelection()) {
    					if (ensInfoList.get(i).weightText[j].getText() != null &&
    							ensInfoList.get(i).weightText[j].getText().trim().length() > 0) {
    						int w = Integer.valueOf(ensInfoList.get(i).weightText[j].getText());
    						if (w > 0 && w <= 100) wt = w;
    					}	
    					
    					if (ensInfoList.get(i).cycles[j] != null) cyc = ensInfoList.get(i).cycles[j];
    					cycleFlag = true;

    					modelListInfo.addModel(name, wt, cyc, first);
    					
    				}
    			}
    			
    			if (!cycleFlag) modelListInfo.addModel(name, wt, cyc, first);
    			
    		}
    		
    		if (ensInfoList.get(i).hasMembers) {
    			for (int jj = 0; jj < ensInfoList.get(i).memberNumber; jj++) {
    				name = null;
    	        	wt = -1;
    	        	cyc = null;
    	        	first = false;
    				
    	        	if (ensInfoList.get(i).members[jj].modelNameButton.getSelection()) {
    					name = ensInfoList.get(i).members[jj].name;
    					if (ensInfoList.get(i).members[jj].isFirstButton.getSelection()) first = true;
    	    			
    					boolean cycleFlag = false;
    	    			for (int j = 0; j < ensInfoList.get(i).members[jj].cycleNumber; j++) {
    	    				if (ensInfoList.get(i).members[jj].cycleButtons[j].getSelection()) {
    	    					if (ensInfoList.get(i).members[jj].weightText[j].getText() != null &&
    	    							ensInfoList.get(i).members[jj].weightText[j].getText().trim().length() > 0) {
    	    						int w = Integer.valueOf(ensInfoList.get(i).members[jj].weightText[j].getText());
    	    						if (w > 0 && w <= 100) wt = w;
    	    					}	
    	    					if (ensInfoList.get(i).cycles[j] != null) cyc = ensInfoList.get(i).cycles[j];
    	    					cycleFlag = true;

    	    					modelListInfo.addModel(name, wt, cyc, first);
    	    				}
    	    			}
    	    			
    	    			if (!cycleFlag) modelListInfo.addModel(name, wt, cyc, first);
    				}
    			}
    		}
    		
    		modelListInfo.setSelectedModelString();
    		selectedModelText.setText(modelListInfo.getSelectedModelString());
    	}
    	
    }
    
    public String getSelectedModels() {
    	return modelListString;
    }

}
