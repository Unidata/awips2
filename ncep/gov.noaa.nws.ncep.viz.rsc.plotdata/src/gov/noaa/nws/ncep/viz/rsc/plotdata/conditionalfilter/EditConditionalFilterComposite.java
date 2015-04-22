package gov.noaa.nws.ncep.viz.rsc.plotdata.conditionalfilter;

import gov.noaa.nws.ncep.viz.localization.NcPathManager;
import gov.noaa.nws.ncep.viz.localization.NcPathManager.NcPathConstants;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefns;
import gov.noaa.nws.ncep.viz.rsc.plotdata.parameters.PlotParameterDefnsMngr;

import java.awt.Color;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.common.dataquery.requests.RequestConstraint;
import com.raytheon.uf.common.dataquery.requests.RequestConstraint.ConstraintType;

/**
 *  UI for editing Conditional Filter and Conditional Filter Elements. 
 *   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 04/2012      #615       S. Gurung   Initial Creation.
 *  
 * </pre>
 * 
 * @author sgurung
 * @version 1.0
 */
public class EditConditionalFilterComposite extends Composite {	
	
	private ConditionalFilter editedConditionalFilter = null;
	
	private ArrayList<Button> delBtnList = null;
	private ArrayList<Combo> paramNameComboList = null;
	private ArrayList<Combo> consTypeComboList = null;
	private ArrayList<Text> valueTFList = null;
		
	private Text descTF = null;
	
	private PlotParameterDefns plotParamDefns = null;	

	private Composite topComposite = null;
	
	private Group cfConditionsGrp = null;

	private GridData gd = null;	
	
	private String[] paramNameArray = null;
	
	private String[] consTypeArray = {ConstraintType.EQUALS.getOperand(), ConstraintType.NOT_EQUALS.getOperand(), 
			ConstraintType.LESS_THAN.getOperand(), ConstraintType.LESS_THAN_EQUALS.getOperand(), ConstraintType.GREATER_THAN.getOperand(),
			ConstraintType.GREATER_THAN_EQUALS.getOperand(), ConstraintType.IN.getOperand(), ConstraintType.BETWEEN.getOperand()};
	
	private File minusImageFile = NcPathManager.getInstance().getStaticFile(NcPathConstants.CONDITIONAL_FILTER_MINUS_IMG ); ;
	
	private File plusImageFile = NcPathManager.getInstance().getStaticFile(NcPathConstants.CONDITIONAL_FILTER_PLUS_IMG ); ;
	
	public EditConditionalFilterComposite( Composite parent, int style, ConditionalFilter cf) {
		super(parent, style);
		editedConditionalFilter = cf;
	
		topComposite = this;
		
		GridLayout mainLayout = new GridLayout(1, true);
		mainLayout.marginHeight = 1;
		mainLayout.marginWidth = 1;
		mainLayout.verticalSpacing = 5;
		topComposite.setLayout(mainLayout);

		plotParamDefns = PlotParameterDefnsMngr.getInstance().getPlotParamDefns( 
								editedConditionalFilter.getPlugin() );		
		
		createControls();
		
		initWidgets();
	}
	
	/*
	 * Create conditional filter elements with attributes: plotParamName, constraintType and value
	 */
	private void createControls() {
		if( editedConditionalFilter == null ) {
			System.out.println("Condtional Filter to Edit is not set???");
			return;
		}
				
		Group cfAttrGrp = new Group ( topComposite, SWT.SHADOW_NONE );
		
		GridLayout gl = new GridLayout(1, false);
		gl.marginTop = 7;
		gl.marginBottom = 10;
		gl.marginRight = 8;
		gl.marginLeft = 7;
		cfAttrGrp.setLayout(gl);
		cfAttrGrp.setText("Conditional Filter");
		
        gd = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
        Label messageLbl = new Label(cfAttrGrp, SWT.NONE);
        messageLbl.setLayoutData(gd);
        messageLbl.setText("Description");
   
        gd = new GridData(410, 50);
        gd.grabExcessVerticalSpace = true;
        descTF = new Text( cfAttrGrp, SWT.MULTI | SWT.BORDER | SWT.V_SCROLL | SWT.H_SCROLL );
        descTF.setLayoutData(gd);

        descTF.addModifyListener(new ModifyListener() {

			@Override
			public void modifyText(ModifyEvent e) {					
				editedConditionalFilter.setDescription( descTF.getText().trim() );
			}
        });
        
    	//new Label(cfAttrGrp, SWT.NONE ).setText("     ");
    	
    	cfConditionsGrp = new Group ( cfAttrGrp, SWT.SHADOW_NONE );        
		GridLayout gl2 = new GridLayout(4, false);
		gl2.marginTop = 7;
		gl2.marginBottom = 8;
		gl2.marginRight = 8;
		gl2.horizontalSpacing = 30;
		cfConditionsGrp.setLayout( gl2 );
		cfConditionsGrp.setText("Conditions");
			
		gd = new GridData();	
		gd.heightHint = 25;
		gd.widthHint = 25;
		Button addBtn = new Button(cfConditionsGrp, SWT.TOGGLE);
		addBtn.setToolTipText("Add New Condition");		
		addBtn.setLayoutData(gd);
		
		if(plusImageFile != null && plusImageFile.exists()) {
			addBtn.setImage(new Image(Display.getCurrent(), plusImageFile.getAbsolutePath()));
		} else {
			addBtn.setText("+");
		}
		
		addBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				addCfe();
			}
		}); 
							
		//new Label(cfConditionsGrp, SWT.NONE ).setText("");
		new Label(cfConditionsGrp, SWT.NONE).setText("Parameter Name");
		new Label(cfConditionsGrp, SWT.NONE).setText("Constraint Type");
		new Label(cfConditionsGrp, SWT.NONE).setText("Value");
		
		delBtnList = new ArrayList<Button>();
		paramNameComboList = new ArrayList<Combo>();
		consTypeComboList = new ArrayList<Combo>();
		valueTFList = new ArrayList<Text>();
		
		Button delBtn = null;
		Combo paramNameCombo = null;
		Combo consTypeCombo = null;
		Text valueTF = null;		
		
		for (int i=0; i < editedConditionalFilter.getSize(); i++) {
			
			// delete 
			delBtn = createDeleteBtn();
			delBtnList.add(delBtn);
			
			// plotParamName 
			paramNameCombo = new Combo(cfConditionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
			paramNameComboList.add(paramNameCombo);			

			// constraintType 
			consTypeCombo = new Combo(cfConditionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
			consTypeComboList.add(consTypeCombo);
					
			// value 
			gd = new GridData(80, SWT.DEFAULT);			
			valueTF = new Text(cfConditionsGrp,  SWT.BORDER);
			valueTF.setLayoutData(gd);
			valueTFList.add(valueTF);			
		}	
		
	}

	public void initWidgets() {				
		
		descTF.setText(editedConditionalFilter.getDescription());
		
		paramNameArray = plotParamDefns.getAllParameterNames( false, false );
		Arrays.sort(paramNameArray);
			
		for (int i=0; i < editedConditionalFilter.getSize(); i++) {		
			paramNameComboList.get(i).setItems(paramNameArray);
			
			consTypeComboList.get(i).setItems(consTypeArray);
			
			if (editedConditionalFilter.getConditionalFilterElement(i) != null) {
				
				for (int j=0; j<paramNameArray.length; j++) {
					if (editedConditionalFilter.getConditionalFilterElement(i).getParamName().equals(paramNameArray[j])) {
						paramNameComboList.get(i).select(j); 
						break;
					}
				}
				
				for (int j=0; j<consTypeArray.length; j++) {
					String ct = editedConditionalFilter.getConditionalFilterElement(i).getConstraintType();
					if (!"".equals(ct) && consTypeArray[j].equals((ConstraintType.valueOf(ct)).getOperand())) {
						consTypeComboList.get(i).select(j); 
						break;
					}
				}
				
				valueTFList.get(i).setText(editedConditionalFilter.getConditionalFilterElement(i).getValue()); 
			}
		}		
		
		for (int i=0; i < delBtnList.size(); i++) {	
			
			delBtnList.get(i).addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					deleteCfe();
				}
			});        
		}	
		
		for (int i=0; i < paramNameComboList.size(); i++) {	
			
			paramNameComboList.get(i).addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					updateCfeParamName();
				}
			});
		}
		
		for (int i=0; i < consTypeComboList.size(); i++) {	
			
			consTypeComboList.get(i).addSelectionListener(new SelectionAdapter() {
				public void widgetSelected(SelectionEvent event) {
					updateCfeConstraintType();
				}
			});
		}
		
		for (int i=0; i < valueTFList.size(); i++) {	
			
			valueTFList.get(i).addModifyListener(new ModifyListener() {

				@Override
				public void modifyText(ModifyEvent e) {					
					updateCfeValue();
				}
	        });
		}		
		
	}
	
	private Button createDeleteBtn() {
		gd = new GridData();	
		gd.heightHint = 25;
		gd.widthHint = 25;
		Button delBtn = new Button(cfConditionsGrp, SWT.TOGGLE);
		
		if( minusImageFile != null && minusImageFile.exists()) {
			delBtn.setImage(new Image(Display.getCurrent(), minusImageFile.getAbsolutePath()));
		} else {
			delBtn.setText("-");
		}
		
		delBtn.setToolTipText("Delete Condition");
		delBtn.setLayoutData(gd);
		
		return delBtn;
	}
	
	public void updateCfeParamName() {

		for (int i=0; i < editedConditionalFilter.getSize(); i++) {	
			editedConditionalFilter.getConditionalFilterElement(i).setParamName( paramNameComboList.get(i).getText() );
		}
	}
	
	public void updateCfeConstraintType() {

		for (int i=0; i < editedConditionalFilter.getSize(); i++) {	
			editedConditionalFilter.getConditionalFilterElement(i).setConstraintType(convertConstraintType(consTypeComboList.get(i).getText()));
		}
	}
	
	public void updateCfeValue() {
		for (int i=0; i < editedConditionalFilter.getSize(); i++) {	
			editedConditionalFilter.getConditionalFilterElement(i).setValue( valueTFList.get(i).getText() );
		}
	}
	
	public void deleteCfe() {
		for (int i=0; i < editedConditionalFilter.getSize(); i++) {	
			
			if (delBtnList.get(i).getSelection()) {
				editedConditionalFilter.getConditionalFilterElements().remove(editedConditionalFilter.getConditionalFilterElement(i));
				
				delBtnList.get(i).dispose();
				paramNameComboList.get(i).dispose();
				consTypeComboList.get(i).dispose();
				valueTFList.get(i).dispose();
				
				delBtnList.remove(i);
				paramNameComboList.remove(i);
				consTypeComboList.remove(i);
				valueTFList.remove(i);
			}
		}
		
		this.getShell().pack();
	}
	
	public void addCfe() {
		
		Button delBtn = createDeleteBtn();
		delBtnList.add(delBtn);
		delBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				deleteCfe();
			}
		}); 
		
		Combo paramNameCombo = new Combo(cfConditionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		paramNameCombo.setItems(paramNameArray);
		paramNameComboList.add(paramNameCombo);
		paramNameCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				updateCfeParamName();
			}
		});
		
		Combo consTypeCombo = new Combo(cfConditionsGrp, SWT.DROP_DOWN | SWT.READ_ONLY);
		consTypeCombo.setItems(consTypeArray);
		consTypeComboList.add(consTypeCombo);
		consTypeCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent event) {
				updateCfeConstraintType();
			}
		});
				
		gd = new GridData(80, SWT.DEFAULT);			
		Text valueTF = new Text(cfConditionsGrp,  SWT.BORDER);
		valueTF.setText("");	
		valueTF.setLayoutData(gd);
		valueTFList.add(valueTF);		
		valueTF.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {					
				updateCfeValue();
			}
        });
		
		editedConditionalFilter.getConditionalFilterElements().add(new ConditionalFilterElement());
		
		this.getShell().pack();
	}	
	
	public String convertConstraintType(String ct) {
		
		if (ConstraintType.EQUALS.getOperand().equals(ct)) {
			return ConstraintType.EQUALS.toString();
		} 
		else if (ConstraintType.NOT_EQUALS.getOperand().equals(ct)) {
			return ConstraintType.NOT_EQUALS.toString();
		} 
		else if (ConstraintType.GREATER_THAN.getOperand().equals(ct)) {
			return ConstraintType.GREATER_THAN.toString();
		} 
		else if (ConstraintType.GREATER_THAN_EQUALS.getOperand().equals(ct)) {
			return ConstraintType.GREATER_THAN_EQUALS.toString();
		} 
		else if (ConstraintType.LESS_THAN.getOperand().equals(ct)) {
			return ConstraintType.LESS_THAN.toString();
		} 
		else if (ConstraintType.LESS_THAN_EQUALS.getOperand().equals(ct)) {
			return ConstraintType.LESS_THAN_EQUALS.toString();
		} 
		else if (ConstraintType.IN.getOperand().equals(ct)) {
			return ConstraintType.IN.toString();
		} 
		else if (ConstraintType.BETWEEN.getOperand().equals(ct)) {
			return ConstraintType.BETWEEN.toString();
		} 	
		
		return "";
	}
	 
	ConditionalFilter getEditedConditionalFilter() {
		return editedConditionalFilter;
	}	   
	
}

