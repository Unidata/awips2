/*
 * gov.noaa.nws.ncep.ui.pgen.attrDialog.GfaAttrDlg
 * 
 * 22 February 2010
 *
 * This code has been developed by the NCEP/SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.ui.pgen.attrdialog;

import static gov.noaa.nws.ncep.ui.pgen.gfa.GfaInfo.*;
import gov.noaa.nws.ncep.ui.pgen.PgenSession;
import gov.noaa.nws.ncep.ui.pgen.PgenStaticDataProvider;
import gov.noaa.nws.ncep.ui.pgen.PgenUtil;
import gov.noaa.nws.ncep.ui.pgen.elements.AbstractDrawableComponent;
import gov.noaa.nws.ncep.ui.pgen.elements.DrawableElement;
import gov.noaa.nws.ncep.ui.pgen.display.IAttribute;
import gov.noaa.nws.ncep.ui.pgen.elements.Layer;
import gov.noaa.nws.ncep.ui.pgen.gfa.Gfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.GfaInfo;
import gov.noaa.nws.ncep.ui.pgen.gfa.IGfa;
import gov.noaa.nws.ncep.ui.pgen.gfa.PreloadGfaDataThread;
import gov.noaa.nws.ncep.ui.pgen.tools.PgenCycleTool;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;

import java.awt.Color;
import java.util.*;
import java.util.List;

//import org.apache.log4j.Logger;
import org.dom4j.Node;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.*;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Singleton attribute dialog for GFA.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 03/2010		#223		M.Laryukhin	Initial Creation.
 * 04/11		#?			B. Yin		Re-factor IAttribute
 * 05/11		#?			J. Wu		Retrieve cycle hour from PgenCycleTool
 * 07/11        #450        G. Hull     NcPathManager
 * 11/11        #?          J. Wu       Add linkage between GFA hazard type 
 *                                      and GAIRMET layer names.
 * 12/11		#?			B. Yin		Set voxText
 * 02/12		#662		J. Wu		update verification of "other" text.
 * 02/12        #597        S. Gurung   Moved snap functionalities to SnapUtil from SigmetInfo.
 * 05/12        #637        Archana.S   Updated the code to update the filter hr in PgenFilterDlg
 *                                      based on the current Fcst hr selected.
 * 05/12		#808		J. Wu		update vor text.
 * 07/12        #663        Q. Zhou     Add selected Gfa in movetext listener. Added get/set for MoveTextBtn
 * 12/12		#908		B. Yin		Added empty items in drop-down menus for multi-selecting
 * 12/12  		#937        J. Wu    	Update G_Airmet layers/hazard - "C&V"
 * </pre>
 * 
 * @author mlaryukhin
 */

public class GfaAttrDlg extends LineAttrDlg implements IGfa {

	/** Singleton instance. */
	private static GfaAttrDlg instance = null;


	/** Logger */
//	private final static Logger log = Logger.getLogger(GfaAttrDlg.class);
    public static final String PGEN_RED_CROSS_IMG   = "red_cross.png";

	private final String[] LABELS = { "Hazard:", "Fcst Hr:", "Tag:", "Desk:", "Issue Type:", "", "" };

	private Combo hazardCbo;
	protected Combo fcstHrCbo;
	private Combo tagCbo;
	private Combo deskCbo;
	private Combo issueTypeCbo;
	private Button moveTextBtn;
	private Label emptyLabel;
	private Text otherText;
	private Text textVOR;
	private Text type;
	private Text areaText;
	private Text beginningText;
	private Text endingText;
	private Button statesBtn;
	private static boolean statesBtnEnabled;
	private static org.eclipse.swt.graphics.Color statesBtnBackground;
	private Text statesText; 
	
	private static int hazardIndexLastUsed;
	private static int fcstHrIndexLastUsed;
	private static int deskIndexLastUsed;
	private static int issueTypeIndexLastUsed;
	private static String tagLastUsed;
	private static String typeLastUsed;

	private static RGB rgbLastUsed;
	private Gfa lastUsedGfa;
	private String otherTextLastUsed;
	
	private Group panelComboGroup;
	private Group hazardSpecificGroup;
	private Group bottomGroup;

	private LinkedHashMap<String, Boolean> typeCheckboxes = new LinkedHashMap<String, Boolean>();
	private String addRemoveBtnLabel = "";
	private LinkedHashMap<String, Boolean> addRemoveDlgCheckboxes = new LinkedHashMap<String, Boolean>();
	// everything else is stored in this map 
	// it holds all the entered values from the form
	private HashMap<String, String> values = new HashMap<String, String>();
	private HashMap<String, Widget> widgets = new HashMap<String, Widget>();
	
	private final int TEXT_WIDTH = 450;
	private final int TEXT_HEIGHT = 25;

	private AddRemoveTypeDlg addRemoveTypeDlg;
	private ArrayList<String> popupCheckboxes = new ArrayList<String>();
	private HashMap<String, Boolean> requiredFields = new HashMap<String, Boolean>();
	private HashMap<String, Label> requiredCrosses = new HashMap<String, Label>();
	private Label warning;

	private final String RED_CROSS_PATH = 
		PgenStaticDataProvider.getProvider().getFileAbsolutePath(
				   PgenStaticDataProvider.getProvider().getPgenLocalizationRoot() + PGEN_RED_CROSS_IMG );
	
	/** Digits, semicolon*/
	private final String TIME = "[01234569]?|12|[012345]?:|[012345]?:(0|1|3|4|00|15|30|45)";


	/**
	 * Private constructor.
	 * 
	 * @param parShell
	 * @throws VizException
	 */
	private GfaAttrDlg(Shell parShell) throws VizException {
		
		super(parShell);

	}

	/**
	 * Creates a G-AIRMET attribute dialog if the dialog does not exist and
	 * returns the instance. If the dialog exists, return its instance.
	 * 
	 * @param parShell
	 * @return
	 */
	public static GfaAttrDlg getInstance(Shell parShell) {
		if (instance == null) {
			try {
				instance = new GfaAttrDlg(parShell);
			} catch (VizException e) {
//				log.error(e.getStackTrace());
				e.printStackTrace();
			}
		}
		return instance;
	}

	@Override
	/*
	 * Creates buttons, menus, and other controls in the dialog area
	 */
	protected void initializeComponents() {

		this.getShell().setText("GFA Attributes");

		if(!PreloadGfaDataThread.loaded) {
			// preload the classes to reduce the first GFA format time 
			new PreloadGfaDataThread().start();
		}

		// set main layout
		GridLayout mainLayout = new GridLayout(2, false);
		mainLayout.marginHeight = 3;
		mainLayout.marginWidth = 3;
		top.setLayout(mainLayout);
		
		Group comboGroup = createPanelCombo();
		comboGroup.setLayoutData(new GridData(SWT.LEFT, SWT.BEGINNING, false, true));

		hazardSpecificGroup = new Group(top, SWT.BORDER);
		hazardSpecificGroup.setLayoutData(new GridData(SWT.LEFT, SWT.BEGINNING, false, true));
		createHazardSpecificPanel();

		createBottomPanel();
		
		addSelectionListeners();
		
		populateTagCbo();
	}

	/**
	 * Creates label and dropdown combos panel.
	 */
	private Group createPanelCombo() {
		panelComboGroup = new Group(top, SWT.BORDER);
		GridLayout layout = new GridLayout(7, false);
		layout.marginHeight = 3;
		layout.marginWidth = 3;
		panelComboGroup.setLayout(layout);

		Label lbl = null;
		for (String s : LABELS) {
			lbl = new Label(panelComboGroup, SWT.LEFT);
			lbl.setText(s);
		}

		hazardCbo = createCombo(panelComboGroup, HAZARD_XPATH, hazardIndexLastUsed);
		fcstHrCbo = createFcstHrCombo(panelComboGroup, FCSTHR_XPATH, fcstHrIndexLastUsed);

		tagCbo = new Combo(panelComboGroup, SWT.DROP_DOWN | SWT.READ_ONLY);
		deskCbo = createCombo(panelComboGroup, DESK_XPATH, deskIndexLastUsed);
		issueTypeCbo = createCombo(panelComboGroup, ISSUE_TYPE_XPATH, issueTypeIndexLastUsed);

		moveTextBtn = new Button(panelComboGroup, SWT.LEFT);
		moveTextBtn.setText(" Move Text ");
		moveTextBtn.setEnabled(false);
		
		GridData gridData = new GridData(SWT.NONE);
	    gridData.horizontalSpan = 2;
	    gridData.horizontalAlignment = GridData.FILL;
	    moveTextBtn.setLayoutData(gridData);
		
		moveTextBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				
				PgenUtil.setDrawingGfaTextMode((Gfa) de);
				moveTextBtn.setEnabled(true);

			}
		});

		textVOR = createTextAttr(panelComboGroup, TEXT_WIDTH, TEXT_HEIGHT, 6, true, false);
		
		createColorButtonSelector();
		
		createOtherText();

		return panelComboGroup;
	}

	private void createColorButtonSelector() {
		cs = new ColorButtonSelector(panelComboGroup);
		if (rgbLastUsed == null) {
			cs.setColorValue(getRGB(hazardCbo.getItem(hazardIndexLastUsed), fcstHrCbo
					.getSelectionIndex()));
		} else {
			cs.setColorValue(rgbLastUsed);
		}
		cs.addListener(new IPropertyChangeListener(){
			@Override
			public void propertyChange(PropertyChangeEvent event) {
				rgbLastUsed = cs.getColorValue();
			}
		});
	}

	/**
	 * Reads the configuration gfa.xml, creates hazard specific panel.
	 */
	private void createHazardSpecificPanel() {

		type = null;
		
		GridLayout layout1 = new GridLayout(2, false);
		layout1.marginHeight = 3;
		layout1.marginWidth = 3;
		hazardSpecificGroup.setLayout(layout1);
		
		requiredFields.clear();
		
		String selectedHazard = hazardCbo.getText();
		// select the children of "root/hazard" element with 
		// name equals to selectedHazard
		String xPath = HAZARD_XPATH + "[@name='" + selectedHazard + "']/*";
		List<Node> nodes = selectNodes(xPath);
		
		for (Node node : nodes) {
			// these are the gui elements to add
			if ("checkbox".equalsIgnoreCase(node.getName())) {
				processCheckboxNode(node);
			} else if ("popup".equalsIgnoreCase(node.getName())) {
				processPopupNode(hazardSpecificGroup, node);
			} else if ("text".equalsIgnoreCase(node.getName())) {
				Text txt = processTextNode(hazardSpecificGroup, node);
				boolean required = "true".equals(node.valueOf("@required"));
				if ("type".equalsIgnoreCase(node.valueOf("@type"))) {
					type = txt;
					requiredFields.put("type", required);
					if(typeLastUsed != null) type.setText(typeLastUsed);
				} else {
					// holder for this value
					String lbl = node.valueOf("@label");
					requiredFields.put(lbl, required);
					values.put(lbl, txt.getText());
					widgets.put(lbl, txt);
				}
			} else if("dropdown".equalsIgnoreCase(node.getName())){
				processDropdownNode(hazardSpecificGroup, node);
			} else if("fzlText".equalsIgnoreCase(node.getName())){
				processFzlNode(hazardSpecificGroup, node);
			}
		}
		if(type != null) updateType();
		hazardSpecificGroup.setVisible(nodes.size() > 0);
	}
	
	/**
	 * Creates elements of the panel at the bottom of the page.
	 */
	private void createBottomPanel() {
		
		if(fcstHrCbo.getText().indexOf('Z') > -1 || 
				("Other".equalsIgnoreCase(fcstHrCbo.getText()) && !otherText.getText().contains("-")) ) {
			if ( bottomGroup != null && !bottomGroup.isDisposed() ) {
				bottomGroup.dispose();
			}
			return;
		} 
		
		bottomGroup = new Group(top, SWT.BORDER); 
		GridData gridData = new GridData(GridData.BEGINNING, SWT.NONE, true, false, 2, 1);
	    bottomGroup.setLayoutData(gridData);

		bottomGroup.setVisible(true);

		GridLayout layout = new GridLayout(6, false);
		layout.marginHeight = 3;
		layout.marginWidth = 3;
		bottomGroup.setLayout(layout);

		Label lbl = new Label(bottomGroup, SWT.RIGHT);
		lbl.setText("Area:");
		lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.BEGINNING, true, true));
		areaText = createTextAttr(bottomGroup, 100, TEXT_HEIGHT, 1, false, true);
		
		lbl = new Label(bottomGroup, SWT.RIGHT);
		lbl.setText("  Beginning:");
		lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.BEGINNING, true, true));
		beginningText = createTextAttr(bottomGroup, 200, TEXT_HEIGHT, 1, true, true);
		
		lbl = new Label(bottomGroup, SWT.RIGHT);
		lbl.setText("  Ending:");
		lbl.setLayoutData(new GridData(SWT.RIGHT, SWT.BEGINNING, true, true));
		endingText = createTextAttr(bottomGroup, 200, TEXT_HEIGHT, 1, true, true);

		statesBtn = new Button(bottomGroup, SWT.RIGHT);
		statesBtn.setText("States:");
		statesBtnBackground = statesBtn.getBackground();
		statesBtn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setEnableStatesButton(false);
			}
		});
		setEnableStatesButton(statesBtnEnabled);
		
		statesText = createTextAttr(bottomGroup, 300, TEXT_HEIGHT, 5, true, true);
	}


	/**
	 * Adds a checkbox to hazardSpecificGroup with the attributes specified in gfa.xml
	 * 
	 * @param node
	 */
	private void processCheckboxNode(Node node) {
		Label lbl = new Label(hazardSpecificGroup, SWT.LEFT);
		final String str = node.valueOf("@label"); 
		lbl.setText(str + ":");

		final Button chkBox = new Button(hazardSpecificGroup, SWT.CHECK);
		if("type".equalsIgnoreCase(node.valueOf("@type"))){
			if(typeCheckboxes.get(str) == null) {
				typeCheckboxes.put(str, false);
			} else {
				chkBox.setSelection(typeCheckboxes.get(str));
			}
			chkBox.addSelectionListener(new SelectionAdapter(){
				@Override
				public void widgetSelected(SelectionEvent e) {
					// typeCheckboxes holds the label vs selection values. 
					typeCheckboxes.put(str, chkBox.getSelection());
					updateType();
				}
			});
		} else {
			if(values.get(str) != null){
				chkBox.setSelection( new Boolean(values.get(str)));
			}
			values.put(node.valueOf("@label"), "" + chkBox.getSelection());
			widgets.put(node.valueOf("@label"), chkBox);
		}
		
		chkBox.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				updateValues();
			}
		});
	}

	/**
	 * Creates "Add/Remove Type" popup with the checkboxes specified in gfa.xml
	 *  
	 * @param group
	 * @param node
	 */
	@SuppressWarnings("unchecked")
	private void processPopupNode(final Group group, Node node) {
		Label lbl = new Label(group, SWT.LEFT);
		addRemoveBtnLabel = node.valueOf("@label"); 
		lbl.setText( addRemoveBtnLabel + ":");

		List<Node> list = node.selectNodes("checkbox");
		popupCheckboxes.clear();
		for (Node n : list) {
			popupCheckboxes.add(n.valueOf("@label"));
		}

		Button btn = new Button(group, SWT.LEFT);
		btn.setText("Add/Remove Types");
		btn.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				try {
					if (addRemoveTypeDlg == null || addRemoveTypeDlg.getShell() == null
							|| addRemoveTypeDlg.getShell().isDisposed()
							|| addRemoveTypeDlg.getShell().getParent() == null) {
						addRemoveTypeDlg = new AddRemoveTypeDlg(GfaAttrDlg.this.getShell());
						addRemoveTypeDlg.parentLabel = addRemoveBtnLabel;
					}
					openAttrDlg(addRemoveTypeDlg);
					Point p = addRemoveTypeDlg.getShell().getLocation();
					addRemoveTypeDlg.getShell().setLocation(p.x + TEXT_WIDTH + 160,
							p.y + 130);
					addRemoveTypeDlg.getShell().setText("Add/Remove Types");

				} catch (VizException e1) {
					e1.printStackTrace();
				}
			}
		});
	}
	
	/**
	 * Creates a label and a text acknowledging all the attributes. 
	 * 
	 * @param group
	 * @param node
	 */
	private Text processTextNode(final Group group, Node node) {
		String lblStr = node.valueOf("@label");
		boolean scrollable = "true".equalsIgnoreCase(node.valueOf("@scrollable"));
		boolean editable = !"false".equalsIgnoreCase(node.valueOf("@editable"));// true by default
		int horizontalSpan = 2;
		if(!lblStr.isEmpty()) {
			Label lbl = new Label(group, SWT.LEFT);
			lbl.setText(lblStr + ":");
			horizontalSpan = 1;
			if (!editable) {
				lbl.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_GRAY));
			}
		}
		
		int w = Integer.parseInt(node.valueOf("@width"));
		int h = Integer.parseInt(node.valueOf("@height"));
		final int CHAR_LIMIT;
		String limit;
		if((limit = node.valueOf("@characterLimit")).isEmpty()){
			CHAR_LIMIT = Integer.MAX_VALUE;
		} else {
			CHAR_LIMIT = Integer.parseInt(limit);
		}
		Group g = hazardSpecificGroup;
		Text txt;
		if("true".equalsIgnoreCase(node.valueOf("@required"))) {
			GridData gridData = new GridData(GridData.BEGINNING);
			if(lblStr.isEmpty()) gridData.horizontalSpan = 2;
		    gridData.horizontalAlignment = GridData.FILL;
			g = new Group(hazardSpecificGroup, SWT.NONE);
			g.setLayoutData(gridData);
			GridLayout layout = new GridLayout(2, false);
			layout.marginHeight = 1;
			layout.marginWidth = 1;
			g.setLayout(layout);

			txt = createTextAttr(g, w, h, 1, scrollable, editable);
			
			Label l = new Label(g, SWT.LEFT | SWT.BEGINNING); 
			Image image = new Image(getShell().getDisplay(), RED_CROSS_PATH);
	        l.setImage(image);
	        l.setBounds(image.getBounds());
			l.setToolTipText("Required field");
			l.setVisible(false);
			if(lblStr.isEmpty()){
				requiredCrosses.put("type", l);
			} else {
				requiredCrosses.put(lblStr, l);
			}
		} else {
			txt = createTextAttr(g, w, h, horizontalSpan, scrollable, editable);
		}

		boolean digitsOnly = "digitsOnly".equalsIgnoreCase(node.valueOf("@characterType"));
		if (digitsOnly || "digitsAndSlash".equalsIgnoreCase(node.valueOf("@characterType"))) {

			final boolean slashAllowed = "digitsAndSlash".equalsIgnoreCase(node.valueOf("@characterType"));
			final String padWithZeros = node.valueOf("@padWithZeros");
			
			txt.addVerifyListener(new VerifyListenerDigitsOnly(slashAllowed, CHAR_LIMIT));
			
			FocusListenerPadding fl = new FocusListenerPadding(padWithZeros, digitsOnly);
			if("Bottom".equalsIgnoreCase(lblStr)) fl.maxFirstToken = 999; // otherwise 450 for "Top"
			txt.addFocusListener(fl);
		}
		
		if (values.get(lblStr) != null) {
			txt.setText(values.get(lblStr));
		}
		
		return txt;
	}
	
	/**
	 * Creates a dropdown and populates it using the value children nodes.
	 * @param group
	 * @param node
	 */
	@SuppressWarnings("unchecked")
	private void processDropdownNode(final Group group, Node node) {
		String lblStr = node.valueOf("@label");
		new Label(group, SWT.LEFT).setText(lblStr + ":");
		Combo cbo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
		List<Node> list = node.selectNodes("value");
		for(Node n: list){
			cbo.add(n.getText());
		}
		String v = values.get(lblStr);
		int index = -1;
		if(v != null) {
			index = cbo.indexOf(v);
		}
		index = (index == -1) ? 0: index;
		cbo.select(index);
		values.put(lblStr, cbo.getText());
		widgets.put(lblStr, cbo);
		cbo.addSelectionListener(new SelectionAdapter(){
			@Override
			public void widgetSelected(SelectionEvent e) {
				updateValues();
			}
		});
	}

	/**
	 * Creates two FZL textboxes tied together. 
	 * 
	 * @param group
	 * @param node
	 */
	private void processFzlNode(final Group group, Node node) {
		boolean scrollable = "true".equalsIgnoreCase(node.valueOf("@scrollable"));
		int w = Integer.parseInt(node.valueOf("@width"));
		int h = Integer.parseInt(node.valueOf("@height"));
		final int CHAR_LIMIT = 3;
		
		Label lbl1 = new Label(group, SWT.LEFT);
		lbl1.setText(Gfa.TOP_BOTTOM + ":");
		
		Group g = new Group(hazardSpecificGroup, SWT.NONE);
		GridData gridData = new GridData(GridData.BEGINNING);
	    gridData.horizontalAlignment = GridData.FILL;
		g.setLayoutData(gridData);
		GridLayout layout = new GridLayout(2, false);
		layout.marginHeight = 1;
		layout.marginWidth = 1;
		g.setLayout(layout);

		Text txt1 = createTextAttr(g, w, h, 1, scrollable, true);
		
		Label lblCross1 = new Label(g, SWT.LEFT | SWT.BEGINNING); 
		Image image = new Image(getShell().getDisplay(), RED_CROSS_PATH);
		lblCross1.setImage(image);
		lblCross1.setBounds(image.getBounds());
		lblCross1.setToolTipText("Required field");
		lblCross1.setVisible(false);
		requiredCrosses.put(Gfa.TOP_BOTTOM, lblCross1);
		requiredFields.put(Gfa.TOP_BOTTOM, true);
		
		final Label lbl2 = new Label(group, SWT.LEFT);
		lbl2.setText(Gfa.FZL_TOP_BOTTOM + ":");
		lbl2.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_GRAY));
		
		g = new Group(hazardSpecificGroup, SWT.NONE);
		gridData = new GridData(GridData.BEGINNING);
	    gridData.horizontalAlignment = GridData.FILL;
		g.setLayoutData(gridData);
		layout = new GridLayout(2, false);
		layout.marginHeight = 1;
		layout.marginWidth = 1;
		g.setLayout(layout);

		final Text txt2 = createTextAttr(g, w, h, 1, scrollable, true);

		final Label lblCross2 = new Label(g, SWT.LEFT | SWT.BEGINNING); 
		lblCross2.setImage(image);
		lblCross2.setBounds(image.getBounds());
		lblCross2.setToolTipText("Required field");
		lblCross2.setVisible(false);
		
		txt1.addVerifyListener(new VerifyListenerDigitsOnly(true, CHAR_LIMIT));
		FocusListenerPadding fl = new FocusListenerPadding("3", false);
		fl.defaultBottom = "FZL";
		txt1.addFocusListener(fl);
		txt2.addVerifyListener(new VerifyListenerDigitsOnly(true, CHAR_LIMIT));
		fl = new FocusListenerPadding("3", false);
		txt2.addFocusListener(fl);
		txt2.setEditable(false);
		txt1.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				boolean hasFZL = ((Text) e.widget).getText().contains("FZL");
				if(hasFZL) {
					values.put(Gfa.FZL_TOP_BOTTOM, txt2.getText());
					requiredFields.put(Gfa.FZL_TOP_BOTTOM, true);
					requiredCrosses.put(Gfa.FZL_TOP_BOTTOM, lblCross2);
				} else {
					txt2.setText("");
					values.put(Gfa.FZL_TOP_BOTTOM, "");
					requiredFields.remove(Gfa.FZL_TOP_BOTTOM);
					lblCross2.setVisible(false);
					requiredCrosses.remove(Gfa.FZL_TOP_BOTTOM);
				}
				txt2.setEditable(hasFZL);
				int colorInt = hasFZL ? SWT.COLOR_BLACK : SWT.COLOR_GRAY;
				lbl2.setForeground(Display.getDefault().getSystemColor(colorInt));
			}
		});
		
		// update txt2, then txt1, otherwise txt2 will be blanked
		if (values.get(Gfa.TOP_BOTTOM) != null) txt2.setText(nvl(values.get(Gfa.FZL_TOP_BOTTOM)));
		if (values.get(Gfa.TOP_BOTTOM) != null) txt1.setText(nvl(values.get(Gfa.TOP_BOTTOM)));
		
		values.put(Gfa.TOP_BOTTOM, txt1.getText());
		widgets.put(Gfa.TOP_BOTTOM, txt1);
		values.put(Gfa.FZL_TOP_BOTTOM, txt2.getText());
		widgets.put(Gfa.FZL_TOP_BOTTOM, txt2);
	}

	/**
	 * Creates a combo box
	 * 
	 * @param panel
	 * @param cboValues
	 * @param indexToSelect
	 * @return
	 */
	private Combo createCombo(Group group, String xPath, int indexToSelect) {
		Combo cbo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);
		List<Node> nodes = selectNodes(xPath);
		for (Node node : nodes) {
			cbo.add(node.valueOf("@name"));
		}
		cbo.select(indexToSelect);
		return cbo;
	}

	/**
	 * Populates the tag combo box. 
	 */
	public void populateTagCbo() {

		tagCbo.removeAll();
		TreeSet<String> tags = new TreeSet<String>(new Comparator<String> (){
			public int compare(String s1, String s2) {
				if(s1.equals(s2)) return 0;
				else if("New".equals(s1) ) return -1;
				else if ("New".equals(s2) ) return 1;
				
				int i1 = Integer.parseInt(s1.replace("*", ""));
				int i2 = Integer.parseInt(s2.replace("*", ""));
				if (i1 < i2) return -1;
				else if (i1 == i2) return 0;
				return 1;
			}
		});
		int index = -1;
		if(drawingLayer != null) {
			Layer layer = drawingLayer.getActiveLayer();
			List<AbstractDrawableComponent> all = layer.getDrawables();
			List<AbstractDrawableComponent> selected = drawingLayer.getAllSelected();
			for(AbstractDrawableComponent adc: all){
				if(!(adc instanceof Gfa) || !((Gfa)adc).isSnapshot() 
						|| !((Gfa)adc).getGfaHazard().equals(this.getGfaHazard())
						|| !((Gfa)adc).getGfaDesk().equals(this.getGfaDesk())) continue;
				Gfa gfa = (Gfa)adc;
				tags.add(gfa.getGfaTag());
			}
			ArrayList<String> reinsertWithStar = new ArrayList<String>();
			for(String tag: tags){
				if(needStar(tag)) reinsertWithStar.add(tag);
			}
			for(String s: reinsertWithStar) {
				tags.remove(s);
				tags.add(s + "*");
			}
			
			Gfa selectedGfa = null;
			for(AbstractDrawableComponent adc: selected){
				if(adc instanceof Gfa) selectedGfa = (Gfa) adc;
			}
			if(selectedGfa!= null){
				index = tagCbo.indexOf(selectedGfa.getGfaTag());
			}
		}
		tags.add("New");
		for(String s: tags) tagCbo.add(s);
		
		if(index == -1 && tagLastUsed!=null) {
			index = tagCbo.indexOf(tagLastUsed);
			if(index == -1) index = tagCbo.indexOf(tagLastUsed+"*");
		}
		if (index == -1) index = 0;

		tagCbo.select(index);
	}
	
	private boolean needStar(String tag) {
		if(drawingLayer != null) {
			Layer layer = drawingLayer.getActiveLayer();
			List<AbstractDrawableComponent> all = layer.getDrawables();
			
			for(AbstractDrawableComponent adc: all) {
				if(adc instanceof Gfa 
						&& ((Gfa)adc).isSnapshot() 
						&& ((Gfa)adc).getGfaHazard().equals(this.getGfaHazard())
						&& ((Gfa)adc).getGfaFcstHr().equals(this.getGfaFcstHr())
						&& ((Gfa)adc).getGfaDesk().equals(this.getGfaDesk()) 
						&& ((Gfa)adc).getGfaTag().equals(tag))
					// already have such element, add * to the tag
					return true;
			}
		}
		return false;
	}

	private String nextNewTag() {
		if(drawingLayer == null) return "1";
		Layer layer = drawingLayer.getActiveLayer();
		List<AbstractDrawableComponent> all = layer.getDrawables();
		
		TreeSet<Integer> tags = new TreeSet<Integer>();
		// check if the same hazard/fsctHr/tag/desk exists
		for(AbstractDrawableComponent adc: all){
			if(adc instanceof Gfa 
					&& ((Gfa)adc).isSnapshot() 
					&& ((Gfa)adc).getGfaHazard().equals(this.getGfaHazard())
					&& ((Gfa)adc).getGfaDesk().equals(this.getGfaDesk())){
				int i = Integer.parseInt(((Gfa)adc).getGfaTag());
				tags.add(i);
			}
		}
		if(tags.isEmpty()) return "1";
		// find gaps
		int current = 1;
		for(Integer i: tags){
			if(current != i) return current + ""; // found a gap
			current ++;
		}
		// no gaps - take the next
		
		return tags.last() + 1 + "";
	}

	/**
	 * @param panel
	 * @param xPath
	 * @param indexToSelect
	 * @return
	 */
	private Combo createFcstHrCombo(Group group, String xPath, int indexToSelect) {
		
		Combo cbo = new Combo(group, SWT.DROP_DOWN | SWT.READ_ONLY);

		List<Node> nodes = selectNodes(xPath);
		int cycle = PgenCycleTool.getCycleHour();
		
		String value = null;
		for (Node node : nodes) {
			value = node.valueOf("@name");

			if (value.indexOf("Z") > -1) {
				String s1 = value.split(" ")[0].trim();
				int s2 = (cycle + Integer.parseInt(s1)) % 24;
				value = s1 + " " + (s2 < 10 ? "0" + s2 : "" + s2) + "Z";
			}

			cbo.add(value);
		}
		cbo.select(indexToSelect);
		return cbo;
	}

	/**
	 * Add all the selection listeners to repaint gui, to update last used
	 * indexes variables, and to change the colors.
	 */
	private void addSelectionListeners() {

		SelectionAdapter s1 = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
                updateHazard();
                linkHazardWithLayer( hazardCbo.getText() );
			}
		};
		SelectionAdapter s2 = new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				redrawHazardSpecificPanel();
				if ( PgenFilterDlg.isFilterDlgOpen()){
					selectFilterHourWhenGfaFcstHourIsChanged(getGfaFcstHr());
				}
				
			}
		};

		// add selection listeners after (!) we create cs
		hazardCbo.addSelectionListener(s1);
		fcstHrCbo.addSelectionListener(s2);
		
		
		SelectionAdapter saveSettings = new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				setAttrForDlg(GfaAttrDlg.this);
				if ( !PgenSession.getInstance().getPgenPalette().getCurrentAction()
						.equalsIgnoreCase("MultiSelect")) {
				populateTagCbo();
			}
			}
		};
		deskCbo.addSelectionListener(saveSettings);
		issueTypeCbo.addSelectionListener(saveSettings);
	}
	
	/**
	 * Enables the filter hour corresponding to the selected gfaFcsthr
	 * if the PgenFilter dialog is open.
	 * @param gfaFcstHr - the selected fcst hour in the GFA dialog
	 */
	private void selectFilterHourWhenGfaFcstHourIsChanged( String gfaFcstHr ){
		for ( Button button: PgenFilterDlg.hourBtns){
			if ( !button.getSelection() ){
				String buttonText = button.getText();
				String filterHr = ( buttonText.endsWith("+") ? buttonText.replace('+', ' ').trim() : buttonText);
				if ( gfaFcstHr.compareTo(filterHr) == 0 ){
					button.setSelection(true);
					mapEditor.refresh();
					break;
				}
					
			}
		}
	}

	/**
	 * Create widgets for the text attribute
	 * 
	 * @param panel
	 * @param width
	 * @param height
	 */
	private Text createTextAttr(Group group, int width, int height, int horizontalSpan, boolean scrollable, boolean editable) {

		int style = SWT.MULTI | SWT.BORDER;
		if(scrollable){
			style |= SWT.H_SCROLL;
		}
		Text text = new Text(group, style);
		GridData gd = new GridData(width, height);
		gd.verticalAlignment =  GridData.BEGINNING;
		gd.horizontalSpan = horizontalSpan;
		text.setLayoutData(gd);
		text.setEditable(editable);
		text.addTraverseListener(new TraverseListenerTab());
		return text;
	}
	
	@Override
	public void createButtonsForButtonBar(Composite parent) {
		super.createButtonsForButtonBar(parent);
		getButton(IDialogConstants.OK_ID).setEnabled(!statesBtnEnabled);
	}
	
	/*
	 * (non-Javadoc) Returns true because gfa is always closed.
	 * 
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg#isClosedLine()
	 */
	public Boolean isClosedLine() {
		return true;
	}

	/*
	 * (non-Javadoc) Returns false because gfa cannot be filled.
	 * 
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg#isFilled()
	 */
	public Boolean isFilled() {
		return false;
	}

	/*
	 * (non-Javadoc) Returns color selection from the attribute dialog
	 * 
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg#getColors()
	 */
	public Color[] getColors() {
		//if(cs == null) return null;
		// IAttribute requires to return an array of colors
		// Only the first color is used at this time.
		Color[] colors = new Color[2];

		colors[0] = new java.awt.Color(cs.getColorValue().red, cs.getColorValue().green, cs
				.getColorValue().blue);

		colors[1] = colors[0];

		return colors;
	}
	
	@Override
	/**
	 *	REtrun size scale. (none for GFA)
	 */
	public double getSizeScale(){
		
		return java.lang.Double.NaN;
		
	}

	/*
	 * (non-Javadoc) Returns line with from the attribute dialog
	 * 
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg#getLineWidth()
	 */
	public float getLineWidth() {

/*
		String xPath = null;
		if(getGfaFcstHr().indexOf("-") == -1) {
			xPath = FCSTHR_XPATH + "[@type='snapshot']";
		} else {
			xPath = FCSTHR_XPATH + "[@name='" + getGfaFcstHr() + "']";
		}
		Node n = GfaInfo.getDocument().selectSingleNode(xPath);
		try{
			return Float.parseFloat(n.valueOf("@linewidth"));
		} catch (Exception e){
			return 2.0F;
		}
*/
		return GfaInfo.getLineWidth( getGfaFcstHr() );
	}

	/*
	 * (non-Javadoc) Returns smooth level from the attribute dialog.
	 * 
	 * @see gov.noaa.nws.ncep.ui.pgen.attrDialog.LineAttrDlg#getSmoothFactor()
	 */
	public int getSmoothFactor() {
		return 0;
	}

	/**
	 * Sets values of all attributes of the dialog.
	 */
	public void setAttrForDlg(IAttribute iattr) {
		if ( iattr instanceof IGfa ){
			IGfa attr = (IGfa)iattr;

			Color clr = attr.getColors()[0];
			if (clr != null) {
				cs.setColorValue(new RGB(clr.getRed(), clr.getGreen(), clr.getBlue()));
			}

			if (hazardCbo != null) {
				int index = hazardCbo.indexOf(attr.getGfaHazard());
				if (index == -1 && attr.getGfaHazard() != null && !attr.getGfaHazard().contains("-")){
					index = 1;
				}
				hazardCbo.select(index);
				hazardIndexLastUsed = index;
			}
			
			if (fcstHrCbo != null) {
				String str = attr.getGfaFcstHr();
				if(nvl(str).indexOf("-") == -1 && !nvl(str).isEmpty()) { // no dash, it is a snapshot
					try {
						int i = Integer.parseInt(str); // snapshot hour
//						int h = attr.getGfaCycleHour(); // cycle hour
						int h = this.getGfaCycleHour(); // cycle hour

						h = (h+i)%24; // to take care of the next day situation

						str += h < 10? " 0" + h + "Z":" "+ h + "Z";
					} catch (NumberFormatException ignore){
					}
				}
				
				int index = fcstHrCbo.indexOf(str);

				if(index == -1 && !nvl(str).isEmpty() && otherText != null && !otherText.isDisposed()){
					if(str.contains("Z")) {
						str = str.split(" ")[0];
					}
					if(!otherText.getText().equals(str)) {
						otherText.setText(str);
					}
					otherTextLastUsed = str;
				}
				
				index = index < 0 ? fcstHrCbo.indexOf("Other"): index; 				
				fcstHrCbo.select(index);
				fcstHrIndexLastUsed = index;
			}
			
			if (tagCbo != null) {
				int index = tagCbo.indexOf(attr.getGfaTag());
				if(index == -1) {
					index = tagCbo.indexOf(attr.getGfaTag()+ "*");
				}
				tagCbo.select(index);
				tagLastUsed = attr.getGfaTag();
			}
			if (deskCbo != null) {
				int index = deskCbo.indexOf(attr.getGfaDesk());
				deskCbo.select(index);
				deskIndexLastUsed = index;
			}
			if (issueTypeCbo != null) {
				int index = issueTypeCbo.indexOf(attr.getGfaIssueType());
				issueTypeCbo.select(index);
				issueTypeIndexLastUsed = index;
			}
			if (type != null) {
				typeLastUsed = (attr.getGfaType() == null) ? "": attr.getGfaType();
				type.setText(typeLastUsed);

				/*
				 *  parse type and update typeCheckboxes and addRemoveDlgCheckboxes
				 *  Note - The "/" to separate CIG and VIS is temporarily replaced 
				 *         as ":" since types for VIS is also separated by "/".
				 */
				String[] types = typeLastUsed.replace("/VIS", ":VIS").split(":");
				for(String key: typeCheckboxes.keySet()){
					if(key.isEmpty()) continue;
					typeCheckboxes.put(key, false);
				}
				for(String key: addRemoveDlgCheckboxes.keySet()){
					addRemoveDlgCheckboxes.put(key, false);
				}
				for(String s: types){
					if (s.isEmpty()) continue;
					if (s.indexOf(addRemoveBtnLabel) == -1){
						typeCheckboxes.put(s, true);
					} else {
						s = s.replaceAll(addRemoveBtnLabel, "").trim();
						for(String key: s.split("/")){
							addRemoveDlgCheckboxes.put(key, true);
						}
					}
				}
			}

			if(iattr instanceof Gfa) {
				// update the points to be use for "Move Text" button 
				lastUsedGfa = (Gfa)iattr;
				de = lastUsedGfa;
				moveTextBtn.setEnabled(true);
				Color c = lastUsedGfa.getColors() == null ? null:lastUsedGfa.getColors()[0];
				if (c != null){
					rgbLastUsed = new RGB(c.getRed(), c.getGreen(), c.getBlue());
				}

				if (areaText != null && !areaText.isDisposed() && attr.getGfaArea() != null) {
					areaText.setText(attr.getGfaArea());
				}
				if (beginningText != null && !beginningText.isDisposed() && attr.getGfaBeginning() != null) {
					beginningText.setText(attr.getGfaBeginning());
				}
				if (endingText != null && !endingText.isDisposed() && attr.getGfaEnding() != null) {
					endingText.setText(attr.getGfaEnding());
				}
				if (statesText != null && !statesText.isDisposed() && attr.getGfaStates() != null) {
					statesText.setText(attr.getGfaStates());
				}

				// VOR text
				updateVORText();
			}

			values = attr.getGfaValues();
		}
	}

	/**
	 * Updates the type textfield with the information based on the checked type buttons in this dialog and in addRemoveTypeDlg.
	 */
	private void updateType(){
		StringBuilder sb = new StringBuilder(200);
		for(String key: typeCheckboxes.keySet()){
			if(typeCheckboxes.get(key)){
				if(sb.length() > 0) sb.append("/");
				sb.append(key);
			}
		}
		
		StringBuilder sb2 = new StringBuilder(200);
		for(String key: addRemoveDlgCheckboxes.keySet()){
			if(addRemoveDlgCheckboxes.get(key)){
				if (sb2.length() == 0 ) sb2.append(addRemoveBtnLabel + " ");
				sb2.append(key).append("/");
			}
		}
		if(sb2.length() > 0) sb2.setLength(sb2.length() -1); // non empty sb2 always ends with "/"
		
		if(sb.length() == 0){
			sb = sb2;
		} else if (sb2.length() != 0){
			sb.append("/").append(sb2); 
		}
				
		String typeStr = sb.toString();
		if ( typeStr.contains("CIG") ) values.put( "CIG", "BLW_010" );
		if ( typeStr.contains("VIS") ) values.put( "VIS", "BLW_3SM" );
				
		type.setText( typeStr );
	}
	
	/**
	 * Updates the values map with the information from the screen. 
	 */
	private void updateValues(){
		for(String key: widgets.keySet()){
			Widget w = widgets.get(key);
			if(w == null || w.isDisposed()) continue;
			if(w instanceof Text) {
				String v = ((Text) w).getText();
				if(Gfa.TOP_BOTTOM.equalsIgnoreCase(key)){
					String[] splitValue = v.split("/"); 
					values.put(Gfa.TOP, (splitValue[0].isEmpty()) ? null:splitValue[0]);
					values.put(Gfa.BOTTOM, (splitValue.length > 1) ? splitValue[1]:null);
				}
				values.put(key, v);
			} else if (w instanceof Combo){
				values.put(key, ((Combo) w).getText());
			} else if (w instanceof Button){
				values.put(key, "" + ((Button) w).getSelection());
			}
		}
		
		updateVORText();
	}

	private void updateVORText() {
		if(drawingLayer == null) return;
		List<AbstractDrawableComponent> selected = drawingLayer.getAllSelected();
		Gfa selectedGfa = null;
		for(AbstractDrawableComponent adc: selected){
			if(adc instanceof Gfa) selectedGfa = (Gfa) adc;
		}
		if(selectedGfa != null) {
			setVorText( selectedGfa.getGfaVorText() );
		}
	}

	@Override
	public String getGfaHazard() {
		if (hazardCbo != null) {
			return hazardCbo.getText();
		}
		return null;
	}

	@Override
	public String getGfaDesk() {
		if (deskCbo != null) {
			return deskCbo.getText();
		}
		return null;
	}

	@Override
	public String getGfaFcstHr() {
		if (fcstHrCbo != null && !fcstHrCbo.isDisposed()) {
			String str = fcstHrCbo.getText();

			if(str.indexOf("Z") > -1) {
				// replace all the terms containing "Z" with "Z", removing everything else
				// leave non-Z terms intact
				String[] a = str.split(" ");
				String temp = "";
				for(String s: a){
					temp += (s.indexOf("Z") > -1) ? "": s;
				}
				str = temp.trim();

			} else if("Other".equalsIgnoreCase(str)) {
				if(otherText!=null && !otherText.isDisposed()){
					str = otherText.getText();
				} else {
					str = "";
				}
			}
			return str;
		}
		return null;
	}

	@Override
	public String getGfaIssueType() {
		if (issueTypeCbo != null) {
			return issueTypeCbo.getText();
		}
		return null;
	}

	@Override
	public String getGfaTag() {
		if (tagCbo != null) {
			String tag = tagCbo.getText();
			if("New".equalsIgnoreCase(tag)){
				tag = nextNewTag();
			}
			return tag.replace("*", "");
		}
		return null;
	}

	@Override
	public String getGfaType() {
		if (type != null && !type.isDisposed()) {
			return type.getText();
		}
		return null;
	}
	
	@Override
	public String getGfaArea() {
		if (areaText != null && !areaText.isDisposed()) {
			return areaText.getText();
		}
		return null;
	}
	
	public void setGfaArea(String value) {
		if (areaText != null && !areaText.isDisposed()) {
			areaText.setText(nvl(value));
		}
	}
	
	@Override
	public String getGfaBeginning() {
		if(beginningText != null && !beginningText.isDisposed()){
			return beginningText.getText(); 
		}
		return null;
	}
	
	public void setGfaBeginning(String value) {
		if(beginningText != null && !beginningText.isDisposed()){
			beginningText.setText(nvl(value));
		}
	}
	
	@Override
	public String getGfaEnding() {
		if(endingText != null && !endingText.isDisposed()){
			return endingText.getText();
		}
		return null;
	}
	
	public void setGfaEnding(String value) {
		if(endingText != null && !endingText.isDisposed()){
			endingText.setText(nvl(value));
		}
	}
	
	@Override
	public String getGfaStates() {
		if(statesText != null && !statesText.isDisposed()){
			return statesText.getText();
		}
		return null;
	}
	
	public void setGfaStates(String value) {
		if(statesText != null && !statesText.isDisposed()){
			statesText.setText(nvl(value));
		}
	}
	
	/**
	 * Returns a copy of a map with all the values from hazard dependent gui panel.
	 * 
	 * @return
	 */
	public HashMap<String, String> getGfaValues(){
		HashMap<String, String> copy = new HashMap<String, String> ();
		for(String key: values.keySet()){
			copy.put(key, values.get(key));
		}
		return copy;
	}

	/**
	 * initialize and open the dialog
	 * 
	 * @param dlg
	 */
	private void openAttrDlg(AttrDlg dlg) {
		dlg.setBlockOnOpen(false);
		dlg.setDrawingLayer(drawingLayer);
		dlg.setMapEditor(mapEditor);
		dlg.open();
	}
	
	public void redrawHazardSpecificPanel() {
		// save settings
		setAttrForDlg(GfaAttrDlg.this);
		// repaint a part of the dialog
		Shell shell = hazardSpecificGroup.getShell();
		if(hazardSpecificGroup != null){
			for(Control c: hazardSpecificGroup.getChildren()){
				c.dispose();
			}
		}
		if(bottomGroup != null && !bottomGroup.isDisposed()) {
			bottomGroup.dispose();
		}
		if(emptyLabel!=null && !emptyLabel.isDisposed()) {
			emptyLabel.dispose();
		}
		
		createOtherText();
		
		if ( !PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")) {
		populateTagCbo();
		}

		// update color 
		String hazard = hazardCbo.getText();
		rgbLastUsed = getRGB(hazard, fcstHrCbo.getSelectionIndex());
		cs.setColorValue(rgbLastUsed);
		
		shell.pack();
		if(warning != null && !warning.isDisposed()) {
			warning.dispose();
			warning = null;
		}
		createHazardSpecificPanel();
		createBottomPanel();
		shell.pack();
	}

	private void createOtherText() {
		if(otherText!=null && !otherText.isDisposed()) {
			otherText.dispose();
		}
		if("Other".equalsIgnoreCase(fcstHrCbo.getText())){
			emptyLabel = new Label(panelComboGroup, SWT.NONE);
			otherText = new Text(panelComboGroup, SWT.BORDER);
			otherText.addVerifyListener(new VerifyListenerOtherText());
			if(otherTextLastUsed != null) otherText.setText(otherTextLastUsed);
			GridData gd = new GridData(SWT.NONE, SWT.BEGINNING, false, true);
			gd.horizontalAlignment = GridData.FILL;
			otherText.setLayoutData(gd);
			textVOR.dispose();
			cs.dispose();
			textVOR = createTextAttr(panelComboGroup, TEXT_WIDTH, TEXT_HEIGHT, 6, true, false);
			createColorButtonSelector();
			panelComboGroup.pack();
		}
	}
	
	@Override
	public void okPressed() {

		// validate first
		if ( !PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect") && !validateRequiredFields())
			return;

		for (String key : widgets.keySet()) {
			Widget w = widgets.get(key);
			if (w == null || w.isDisposed())
				continue;
			if (w instanceof Button) {
				Boolean b = new Boolean(((Button) w).getSelection());
				values.put(key, b.toString());
			} else if (w instanceof Text) {
				values.put(key, ((Text) w).getText());
			} else if (w instanceof Combo) {
				values.put(key, ((Combo) w).getText());
			}
		}
		correctOtherText();

		ArrayList<AbstractDrawableComponent> adcList = null;
		ArrayList<AbstractDrawableComponent> newList = new ArrayList<AbstractDrawableComponent>();

		// get the list of selected elements
		if (drawingLayer != null) {
			adcList = (ArrayList<AbstractDrawableComponent>) drawingLayer.getAllSelected();
		}

		if (adcList != null && !adcList.isEmpty()) {

			// loop through the list and update attributes
			for (AbstractDrawableComponent adc : adcList) {
				DrawableElement el = adc.getPrimaryDE();
				if (el != null) {
					// Create a copy of the currently selected element
					DrawableElement newEl = (DrawableElement) el.copy();
					// Update the new Element with these current attributes
					newEl.update(this);
					newList.add(newEl);
					
					if (newEl instanceof Gfa) {
						((Gfa)newEl).snap();
						lastUsedGfa = (Gfa)newEl;
						de = lastUsedGfa;
						//lastUsedGfa.setGfaValues(values);
					}
				}
			}
			ArrayList<AbstractDrawableComponent> oldList = new ArrayList<AbstractDrawableComponent>(adcList);
			drawingLayer.replaceElements(oldList, newList);
		}
		drawingLayer.removeSelected();

		// set new elements as selected
		for (AbstractDrawableComponent adc : newList) {
			drawingLayer.addSelected(adc);
		}

		if (mapEditor != null) {
			mapEditor.refresh();
		}

		updateValues();
		
		if (fcstHrCbo != null && !fcstHrCbo.isDisposed()
				&& "Other".equalsIgnoreCase(fcstHrCbo.getText()) && otherText != null
				&& !otherText.isDisposed()) {
			otherTextLastUsed = otherText.getText();
		}
		if(tagCbo!= null && !tagCbo.isDisposed() && lastUsedGfa != null
			&& ( !PgenSession.getInstance().getPgenPalette().getCurrentAction()
						.equalsIgnoreCase("MultiSelect"))) {
			tagLastUsed = lastUsedGfa.getGfaTag();
			populateTagCbo();
		}
	}
	
	@Override
	public boolean close() {
		if(statesBtnEnabled) {
			return false;
		}
		return super.close();
	}
	
	@Override
	public int open() {
		if(statesBtnEnabled || (top != null && !top.isDisposed())) {
			return CANCEL;
		}
		
		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")) {
			initMultiSelect();
		}
		
		
		this.create();

		if ( PgenSession.getInstance().getPgenPalette().getCurrentAction()
				.equalsIgnoreCase("MultiSelect")) {
			configMultiSelect();
		}
		
		int open = super.open();
		
		switchHazard( PgenSession.getInstance().getPgenResource().getActiveLayer().getName() );
		
		return open;
		
//		return super.open();
	}

	private void correctOtherText() {
		if(otherText == null || otherText.isDisposed()) return;
		String str = otherText.getText();
		
		if(str.endsWith(":") || str.endsWith("-")) str = str.substring(0, str.length()-1);
		String[] s = str.split("-");
		for(int i=0; i<s.length; i++) {
			if(s[i].indexOf(":") > -1) {
				s[i] = padTime(s[i]); 
			}
			
		}
		switch (s.length) {
		case 1:
			str = s[0];
			break;
		case 2:
			str = s[0] + "-" + s[1];
			break;
		}
		if(!otherText.getText().equals(str)){
			otherText.setText(str);
		}
	}

	/** Time is padded to return x:00, x:15, x:30, or x:45 minutes only. */ 
	private String padTime(String in) {
		if(in.matches("[01234569]|12")) return in += ":00";
		if(in.matches("12:|[012345]:")) return in += "00";
		if(!in.matches(TIME)) return in;
		String[] hm = in.split(":");
		hm[0] = hm[0].isEmpty() ? "0" : hm[0];
		// hm[0] for hour, hm[1] min
		if(hm[1].matches("1") || hm[1].matches("4")) {
			hm[1] += "5";
		}
		if(hm[1].matches("0") || hm[1].matches("3")) {
			hm[1] += "0";
		}
		String out = hm[0] + ":" + hm[1];
		return out;
	}
	
	
	/**
	 * Pops up a dialog to indicate an error if a required field is not set up
	 * 
	 * @return
	 */
	public boolean validateRequiredFields() {
		
		if (new Boolean(true).equals(requiredFields.get("type")) 
				&& type != null
				&& !type.isDisposed() 
				&& type.getText().isEmpty()) {
			displayCrosses(true);
			return false;
		}

		for (String key : widgets.keySet()) {
			Widget w = widgets.get(key);
			if (w == null || w.isDisposed()) continue;
			
			if (w instanceof Text || w instanceof Combo) {
				if(new Boolean(true).equals(requiredFields.get(key)) 
						&& ((Text) w).getText().isEmpty()) {
					displayCrosses(true);
					return false;
				}
			}
		}
		
		displayCrosses(false);

		return true;
	}
	
	private void displayCrosses(boolean show){
		for(String key: requiredCrosses.keySet()){
			Label w = requiredCrosses.get(key);
			if( w!= null && !w.isDisposed()) {
				w.setVisible(show);
			}
		}
		Shell shell = top.getShell();
		if (warning != null && !warning.isDisposed()){
			warning.dispose();
			warning = null;
			shell.pack();
		}
		if(show){
			warning = new Label(top, SWT.LEFT);
			warning.setText("Fields marked with red crosses are required ");
			warning.setForeground(Display.getDefault().getSystemColor(SWT.COLOR_RED));
			shell.pack();
		}
	}
	
	private String nvl(String value) {
		return value == null ? "" : value;
	}


	private void updateColorWhenOtherTextChanged(String s) {
		String hazard = hazardCbo.getText();
		if("Other".equals(fcstHrCbo.getText())) {
			String str = s;
			if(str.endsWith(":") || str.endsWith("-")) str = str.substring(0, str.length()-1);
			Color[] c = GfaInfo.getDefaultColors(hazard, str);
			RGB rgb = new RGB(c[0].getRed(), c[0].getGreen(), c[0].getBlue());
			cs.setColorValue(rgb);
		} else {
			cs.setColorValue(getRGB(hazard, fcstHrCbo.getSelectionIndex()));
		}
	}


	/**
	 * A listener class on the text to check whether the input is digits or "/". 
	 */
	private class VerifyListenerDigitsOnly implements VerifyListener {
		private boolean slashAllowed;
		private int charLimit;

		private VerifyListenerDigitsOnly(boolean slashAllowed, int char_limit) {
			this.slashAllowed = slashAllowed;
			this.charLimit = char_limit;
		}

		@Override
		public void verifyText(VerifyEvent e) {

			String str = ((Text) e.widget).getText().substring(0, e.start) + e.text;
			str = str.trim().toUpperCase();
			if(str.length() > charLimit) e.doit = false;
			
			if(slashAllowed) {
				// from 0 to 3 digits (\\d{0,3}) or (|) from 1 to 3 digits + slash (/) + up to 3 digits again (\\d{0,3})
				e.doit = str.matches("\\d{0,3}|\\d{1,3}/|\\d{1,3}/\\d{0,3}|\\d{1,3}/S|\\d{1,3}/SF|\\d{1,3}/SFC|\\d{1,3}/F|\\d{1,3}/FZ|\\d{1,3}/FZL"); 
			} else {
				e.doit = str.matches("\\d{0," + charLimit + "}");
			}

			// backspace or delete
			if (e.character == '\u0008' || e.character == '\u007F' ) e.doit = true;
		}
	}

	/**
	 * A listener class on the other Text field. 
	 */
	private class VerifyListenerOtherText implements VerifyListener {
		
		@Override
		public void verifyText(VerifyEvent e) {
			
			String s = ((Text) e.widget).getText();
			s = s.substring(0, e.start) + e.text + s.substring(e.end);

			e.doit = s.matches(TIME);
			e.doit |= s.matches("(" + TIME + ")-");
			e.doit |= s.matches("(" + TIME + ")-(" + TIME + ")");
			if(!e.doit) return;
			
			check:
			if(s.contains("-")){
				String[] t =  s.split("-");
				if(t.length == 2 && !t[0].isEmpty() && !t[1].isEmpty()){
					// for example 2:30-4:15
					t[0] = padTime(t[0]);
					t[1] = padTime(t[1]);
					String[] t0 = t[0].split(":");
					String[] t1 = t[1].split(":");
					if(t0[1].isEmpty() || t1[1].isEmpty()) break check;
										
					if ( Integer.parseInt(t0[0]) > Integer.parseInt(t1[0] ) &&  
						 Integer.parseInt(t1[0]) != 1 ) {
						// 4:30-2:15
						// 4>2
						e.doit = false;
						return;
					}
				}
			}
			
			
			if(!e.doit) return;
			
			// update color 
			updateColorWhenOtherTextChanged(s);
		}
	}

	/**
	 * A listener class on the text, when tab out, pad with zeros. 
	 */
	private class FocusListenerPadding extends FocusAdapter {
		private final String digitsToPad;
		private boolean digitsOnly;
		String defaultBottom = "SFC";
		int maxFirstToken = 450;

		private FocusListenerPadding(String digitsToPad, boolean digitsOnly) {
			this.digitsToPad = digitsToPad;
			this.digitsOnly = digitsOnly;
		}

		@Override
		public void focusLost(FocusEvent e) {
			String str = ((Text) e.widget).getText();
			if(str.isEmpty()) return;
			String [] split = str.split("/");
			if(!digitsToPad.isEmpty()) {
				int padInt = Integer.parseInt(digitsToPad);
				for(int i=0; i<split.length; i++){
					if(!digitsOnly && i==1 && split[i].toUpperCase().startsWith("SFC".substring(0, split[i].length()))){
						split[i] = "SFC";
					} else if(!digitsOnly && i==1 && split[i].toUpperCase().startsWith("FZL".substring(0, split[i].length()))){
						split[i] = "FZL";
					} else {
						split[i] = padWithZeros(split[i], padInt);
					}
					if(i == 0 && Integer.parseInt(split[i]) > maxFirstToken) split[i] = "" + maxFirstToken; // for "Top" fields
				}
			}
			String out = "";
			for(int i=0; i<split.length; i++){
				if(!out.isEmpty()) out += "/";
				out += split[i].toUpperCase();
			}
			if(!digitsOnly && split.length == 1) out += "/" + defaultBottom;
			
			((Text) e.widget).setText(out);
			
			updateValues();
		}

		private String padWithZeros(String str, int length) {
			if(str.length() > length) return str;
			for(int i=str.length(); i<length; i++){
				str+="0";
			}
			return str;
		}
	}
	
	/**
	 * Listener to tab out of the text field using tab, not only ctrl + tab. 
	 */
	static class TraverseListenerTab implements TraverseListener {
		public void keyTraversed(TraverseEvent e) {
			if (e.detail == SWT.TRAVERSE_TAB_NEXT || e.detail == SWT.TRAVERSE_TAB_PREVIOUS) {
				e.doit = true;
			}
		}
	}

	public void setEnableStatesButton(boolean enabled) {
		if(statesBtn == null || statesBtn.isDisposed()) return;
		if(enabled){ 
			statesBtn.setBackground(Display.getDefault().getSystemColor(SWT.COLOR_YELLOW));
		} else {
			statesBtn.setBackground(statesBtnBackground);
		}
		statesBtn.setEnabled(enabled);
		statesBtnEnabled = enabled;
		if(getButton(IDialogConstants.OK_ID) != null) {
			getButton(IDialogConstants.OK_ID).setEnabled(!enabled);
		}
	}
	
	/**
	 * AddRemoveTypeDlg attribute dialog.
	 * 
	 * @author mlaryukhin
	 * 
	 */
	private class AddRemoveTypeDlg extends AttrDlg {
		
		String parentLabel;
		protected Composite top;

		private AddRemoveTypeDlg(Shell parShell) throws VizException {
			super(parShell);

		}

		@Override
		public void createButtonsForButtonBar(Composite parent) {
			// create OK only
			createButton(parent, IDialogConstants.CLOSE_ID, IDialogConstants.CLOSE_LABEL, true);
			getButton(IDialogConstants.CLOSE_ID).addSelectionListener(new SelectionAdapter() {
				@Override
				public void widgetSelected(SelectionEvent e) {
					okPressed();
				}
			});
		}

		@Override
		public void handleShellCloseEvent() {
			// do not need to go to SelectingMode, so overriding the AttrDlg's method. 
			setReturnCode(CANCEL);
			close();
		}

		@Override
		public void okPressed() {
			close();
			addRemoveTypeDlg = null;
		}

		/**
		 * Creates the dialog area
		 */
		@Override
		public Control createDialogArea(Composite parent) {
			top = (Composite) super.createDialogArea(parent);

			// Create the main layout for the shell.
			GridLayout mainLayout = new GridLayout(1, false);
			mainLayout.marginHeight = 3;
			mainLayout.marginWidth = 3;
			top.setLayout(mainLayout);

			Group group = new Group(top, SWT.BORDER | SWT.OK);
			GridLayout layout1 = new GridLayout(1, false);
			layout1.marginHeight = 3;
			layout1.marginWidth = 3;
			group.setLayout(layout1);

			for (final String labelStr : popupCheckboxes) {
				
				final Button btn = new Button(group, SWT.CHECK | SWT.CENTER);
				btn.setText(labelStr);
				boolean checked = false; 
				if(addRemoveDlgCheckboxes.get(labelStr) == null) {
					addRemoveDlgCheckboxes.put(labelStr, false);
				} else {
					checked = addRemoveDlgCheckboxes.get(labelStr);
				}
				btn.setSelection(checked);
				btn.addSelectionListener(new SelectionAdapter(){
					@Override
					public void widgetSelected(SelectionEvent e) {
						addRemoveDlgCheckboxes.put(labelStr, btn.getSelection());
						updateType();
					}
				});
			}

			return top;
		}

		@Override
		public void setAttrForDlg(IAttribute ia) {
		}
		
		public boolean isDisposed() {
			return top.isDisposed();
		}
	}

	@Override
	public String getPatternName() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getSymbolType() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getGfaCycleDay() {
		// TODO Auto-generated method stub
		return PgenCycleTool.getCycleDay();
	}

	@Override
	public int getGfaCycleHour() {
		// TODO Auto-generated method stub
		return PgenCycleTool.getCycleHour();
	}
	
	/**
	 * Check if Gfa attribute window is open
	 */
	public boolean isGfaOpen() {		
		return ( hazardCbo != null && !hazardCbo.isDisposed() );	
	}
	
	/**
	 * Switch Gfa hazard type to a given type.
	 */
	public void switchHazard( String hazard ) {

		if ( isGfaOpen() ) {
			
			String currentHaz = hazardCbo.getText();
			if ( hazard != null && !currentHaz.equals( hazard ) ) {
			   
				int index = hazardCbo.indexOf( hazard );
			
			    if ( index >= 0 ) {
				
			        hazardCbo.select(index);
			        hazardIndexLastUsed = index;
				
			        updateHazard();
			    }
			}
		}		
	}
	
	/*
	 * Update GFA GUI using the current Gfa hazard.
	 */
	private void updateHazard() {
				
		if ( addRemoveTypeDlg != null&& !addRemoveTypeDlg.isDisposed() ) {
			addRemoveTypeDlg.close();
			addRemoveTypeDlg = null;
		}

		if ( type != null ) type.setText( "" );

		typeCheckboxes.clear();
		values.clear();
		widgets.clear();
		addRemoveDlgCheckboxes.clear();
		redrawHazardSpecificPanel();					
	}	
	
	/*
	 * Update the active layer in the product manage GUI or the layering control window
	 * to the current Gfa hazard type.
	 */
	private void linkHazardWithLayer( String layer ) {
    	if ( PgenSession.getInstance().getPgenResource().getProductManageDlg() != null &&
    			PgenSession.getInstance().getPgenResource().getProductManageDlg().isOpen()	) {
		    PgenSession.getInstance().getPgenResource().getProductManageDlg().switchLayer( layer );
    	}	
    	else if ( PgenSession.getInstance().getPgenResource().getLayeringControlDlg() != null &&
    			      PgenSession.getInstance().getPgenResource().getLayeringControlDlg().isOpen() ) {
		    PgenSession.getInstance().getPgenResource().getLayeringControlDlg().switchLayer( layer );
    	}	
	}	
	
	/**
	 * Sets vorText field in the GFA dialog.
	 * @param vorText
	 */
	public void setVorText( String vorText ){
	    //vorText maybe null when multi-selecting
		if ( vorText != null ) textVOR.setText( vorText );
	}

	/**
	 * check if the MoveText button is enabled.
	 * @return
	 */
	public boolean isMoveTextEnable(){
		return moveTextBtn.isEnabled();
	}
	/**
	 * Enable/Disable the move text button
	 * @param flag
	 */
	public void enableMoveTextBtn( boolean flag ){
		moveTextBtn.setEnabled(flag);
	}
	
	/**
	 *	Prepare to create attribute dialog for multiple selection 
	 */
	private void initMultiSelect(){
		typeCheckboxes.clear();
		this.addRemoveDlgCheckboxes.clear();
		typeLastUsed ="";
		
		values.clear();
	}
	
	/**
	 *  Add empty items in menus for multiple selection
	 */
	private void configMultiSelect(){
		tagCbo.add("");
		int index = tagCbo.indexOf("");
		tagCbo.select(index);
		
		deskCbo.add("");
		index = deskCbo.indexOf("");
		deskCbo.select(index);
		
		issueTypeCbo.add("");
		index = issueTypeCbo.indexOf("");
		issueTypeCbo.select(index);

		index = fcstHrCbo.indexOf("Other");
		fcstHrCbo.select(index);
	}
}

