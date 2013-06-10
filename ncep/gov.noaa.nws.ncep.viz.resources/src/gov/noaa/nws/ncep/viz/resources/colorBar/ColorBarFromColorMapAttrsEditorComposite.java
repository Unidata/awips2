/*
 * This code has been developed by NCEP-SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.resources.colorBar;

import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;

import javax.measure.converter.ConversionException;
import javax.measure.converter.UnitConverter;

import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.common.ui.color.ColorButtonSelector;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.resources.INatlCntrsResourceData;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet;
import gov.noaa.nws.ncep.viz.resources.attributes.ResourceAttrSet.RscAttrValue;
import gov.noaa.nws.ncep.viz.resources.manager.ResourceCategory;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.ColorBarFromColormap;
import gov.noaa.nws.ncep.viz.ui.display.IColorBar;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarAnchorLocation;
import gov.noaa.nws.ncep.gempak.parameters.colorbar.ColorBarOrientation;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import org.eclipse.core.commands.Command;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.KeyListener;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;

import org.eclipse.swt.graphics.Color;
import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.IResourceDataChanged.ChangeType;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.core.imagery.ImageCombiner;
import com.raytheon.viz.core.imagery.ImageCombiner.IImageCombinerListener;
import com.raytheon.viz.ui.dialogs.colordialog.ColorUtil;
import com.raytheon.viz.ui.editor.AbstractEditor;

/**
 * 
 * Creates the widgets that constitute the dialogs for editing the color bar
 * from a color map
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 2/29/2012     651        Archana       Initial Creation
 * 04/02/2012               S. Gurung     Increased height for colorBarLocOptionsGroup
 * 04/06/2012    651        Archana       Added a call to refreshColorBar(), when the showLabelBtn
 *                                        is toggled
 * 07/17/2012    743        Archana       Refactored the packages for 
 *                                        ColorBarAnchorLocation and ColorBarOrientation                                              
 * 02/09/2013    972        Greg Hull     ResourceCategory class
 * </pre>
 * 
 * @author archana
 * @version 1
 */

public class ColorBarFromColorMapAttrsEditorComposite extends Composite {

	private ColorBarAnchorLocation[] availAnchorLocs = new ColorBarAnchorLocation[] {
			ColorBarAnchorLocation.UpperLeft,
//			ColorBarAnchorLocation.UpperCenter,
			ColorBarAnchorLocation.UpperRight,
//			ColorBarAnchorLocation.CenterLeft,
//			ColorBarAnchorLocation.CenterCenter,
//			ColorBarAnchorLocation.CenterRight,			
			ColorBarAnchorLocation.LowerLeft, 
//			ColorBarAnchorLocation.LowerCenter,
			ColorBarAnchorLocation.LowerRight,
	        };

	private static final transient IUFStatusHandler statusHandler = UFStatus
			.getHandler(ColorBarFromColorMapAttrsEditorComposite.class);
	private String[] arrayOfSliderTextStrings = null;
	private Float cmapMin = null;
	private Float cmapMax = null;
	private Float cmapWidth = null;
	private Float cmapIncrement = null;
	private Float currentMax = null;
	private Float currentMin = null;
	private ColorMapCapability cMapCapability;
	private DecimalFormat format = null;
	private ArrayList<AbstractNatlCntrsResource<?, ?>> imageResources;
	private ArrayList<AbstractNatlCntrsResource<?, ?>> cmapResources;
	private String selectedColorMapName = null;
	private ColorMap theSelectedColorMap = null;
	private ColorMapParameters cMapParam;
	final Combo orientationCombo = null;
	final Combo anchorCombo = null;
	final Combo unitsCombo = null;
	private IColorBar editedColorBar = null;
	private ResourceCategory colorMapCategory = null;
	private String colorMapName;

	private Scale minSlider = null;
	private Scale maxSlider = null;
	private Text minValueText = null;
	private Text maxValueText = null;
	private Scale brightnessScale = null;
	private Label brightnessText = null;
	private Scale contrastScale = null;
	private Label contrastText = null;
	private Scale alphaScale = null;
	private Label alphaLabel = null;
	private Label alphaText = null;
	private Button interpolationChk = null;
	private Button combineNextImage = null;
	private AbstractEditor currEditor = null;
	private INatlCntrsResourceData theResourceData;
	private ResourceAttrSet resAttrSet = null;
    private Button showColorBarEditOptionsBtn = null;
	private int defaultDialogHeight = 205;
	private int shellWidth = 764;
	private Shell shell = null;

	private IImageCombinerListener combineNextImageListener = new IImageCombinerListener() {
		@Override
		public void preferenceChanged(boolean newPref) {
			combineNextImage.setSelection(newPref);
		}
	};

	protected Color labelColor;

	protected Device colorDevice;

	public ColorBarFromColorMapAttrsEditorComposite(Composite parent,
			int style, INatlCntrsResourceData resourceData) {

		super(parent, style);
		theResourceData = resourceData;
		colorDevice = parent.getDisplay();
		initialization();

		Composite topForm = this;
		FormData fd = new FormData(shellWidth, defaultDialogHeight);
		fd.top = new FormAttachment(0, 0);
		fd.left = new FormAttachment(0, 0);
		fd.right = new FormAttachment(100, 0);
		fd.bottom = new FormAttachment(100, 0);
		topForm.setLayoutData(fd);
		topForm.setLayout(new FormLayout());

		topForm.setLayout( new GridLayout(1, true) );
		shell = parent.getShell();
		final Composite colorMapComp = new Composite(topForm, SWT.NONE);		
		createColorMapComboAndEditButton(colorMapComp);

		fd = new FormData(163, 30);
		fd.left = new FormAttachment(colorMapComp, 525, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 15, SWT.BOTTOM);
		showColorBarEditOptionsBtn = new Button(colorMapComp, SWT.PUSH);
		showColorBarEditOptionsBtn.setLayoutData(fd);
		showColorBarEditOptionsBtn.setText("Edit Color Bar Options ...");
		
		Group imagingGrp = new Group(colorMapComp, SWT.NONE);
		imagingGrp.setText("Image Properties");
		fd = new FormData(350, 120);
		fd.left = new FormAttachment(colorMapComp, 10, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 60, SWT.BOTTOM);

		imagingGrp.setLayout(new FormLayout());
		imagingGrp.setLayoutData(fd);

		// Create the slider controls for Alpha, Brightness and Contrast
		initializeABCControls(imagingGrp);

		Group minMaxValuesGroup = new Group(colorMapComp, SWT.NONE);
		minMaxValuesGroup.setText("Colormap Range:");
		fd = new FormData(360, 75);
		fd.left = new FormAttachment(imagingGrp, 20, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 65, SWT.BOTTOM);
		minMaxValuesGroup.setLayout(new FormLayout());
		minMaxValuesGroup.setLayoutData(fd);
		createColorMapRangeControlSliders(minMaxValuesGroup);



        final Group colorBarLocOptionsGroup = new Group(colorMapComp,
				SWT.SHADOW_ETCHED_OUT);
		colorBarLocOptionsGroup.setText("Edit Color Bar Options");

		colorBarLocOptionsGroup.setLayout(new FormLayout());
		createColorBarLocationDimensionAndLabelDisplayControls(colorBarLocOptionsGroup);
		showColorBarEditOptionsBtn.addSelectionListener( new SelectionAdapter() {

			
			public void widgetSelected(SelectionEvent e){
				if ( ((Button)e.widget).getText().startsWith("Edit") ){
					 ((Button)e.widget).setText("Hide ...");
					 
					    shell.setSize(new Point ( shell.getSize().x, shell.getSize().y + colorBarLocOptionsGroup.getSize().y + 30));
					    colorBarLocOptionsGroup.setVisible(true);
				 }
				else{
					  ((Button)e.widget).setText("Edit Color Bar Options ...");
					    colorBarLocOptionsGroup.setVisible(false);
					    shell.setSize(new Point ( shell.getSize().x, shell.getSize().y - colorBarLocOptionsGroup.getSize().y - 30 ));
				}
				
			}
		});
		
		fd = new FormData(730, 95);
		
		fd.left = new FormAttachment(colorMapComp, 10, SWT.RIGHT);
		fd.top = new FormAttachment(imagingGrp, 20, SWT.BOTTOM);
		colorBarLocOptionsGroup.setLayoutData(fd);
		colorBarLocOptionsGroup.setVisible(false);

	}

	private void initialization() {
		
		imageResources    = new ArrayList<AbstractNatlCntrsResource<?, ?>>();
		cmapResources     = new ArrayList<AbstractNatlCntrsResource<?, ?>>();
		resAttrSet        = theResourceData.getRscAttrSet();
		colorMapCategory  = theResourceData.getResourceName().getRscCategory();
		colorMapName      = (String)( resAttrSet.getRscAttr("colorMapName").getAttrValue());
		editedColorBar    =  (ColorBarFromColormap) resAttrSet.getRscAttr("colorBar").getAttrValue();
		
		labelColor = new Color(colorDevice, editedColorBar.getLabelColor());
		currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();

		if (currEditor != null) {
  		    //TODO: might need to remove if we decide that we don't need the interpolation/image combiner
			
			ResourceList resList = currEditor.getActiveDisplayPane()
					.getRenderableDisplay().getDescriptor().getResourceList();
			if (resList != null && resList.size() > 0) {
				for (ResourcePair rp : resList) {
					AbstractVizResource<?, ?> resource = rp.getResource();
					if ( resource instanceof AbstractNatlCntrsResource<?, ?>){
					    if (resource.hasCapability(ImagingCapability.class)) {
						      imageResources.add((AbstractNatlCntrsResource<?, ?>) resource);
					     } else if (resource.hasCapability(ColorMapCapability.class)) {
						        cmapResources.add((AbstractNatlCntrsResource<?, ?>) resource);
					     }
					}
				}
			}
		}

	}

	private void createColorBarLocationDimensionAndLabelDisplayControls(
			Composite colorBarLocOptionsGroup) {

		Composite newComp = new Composite(colorBarLocOptionsGroup, SWT.NONE);
		newComp.setLayout(new FormLayout());

		Label orLabel = new Label(newComp, SWT.NONE);
		orLabel.setText("Orientation");
		FormData fd = new FormData();
		fd.left = new FormAttachment(newComp, 8, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		orLabel.setLayoutData(fd);

		final Combo orientationCombo = new Combo(newComp, SWT.DROP_DOWN
				| SWT.READ_ONLY);
		fd = new FormData();
		fd.left = new FormAttachment(orLabel, 20, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		orientationCombo.setLayoutData(fd);

		Label anchorLabel = new Label(newComp, SWT.NONE);
		anchorLabel.setText("Anchor");
		fd = new FormData();
		fd.left = new FormAttachment(newComp, 8, SWT.RIGHT);
		fd.top = new FormAttachment(orientationCombo, 20, SWT.BOTTOM);
		anchorLabel.setLayoutData(fd);

		final Combo anchorCombo = new Combo(newComp, SWT.DROP_DOWN
				| SWT.READ_ONLY);
		fd = new FormData();
		fd.left = new FormAttachment(anchorLabel, 45, SWT.RIGHT);
		fd.top = new FormAttachment(orientationCombo, 20, SWT.BOTTOM);
		anchorCombo.setLayoutData(fd);

		Label lengthLabel = new Label(newComp, SWT.NONE);
		lengthLabel.setText("Length");
		fd = new FormData();
		fd.left = new FormAttachment(anchorCombo, 40, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		lengthLabel.setLayoutData(fd);

		final Spinner lenSpinner = new Spinner(newComp, SWT.BORDER);
		fd = new FormData();
		fd.left = new FormAttachment(lengthLabel, 10, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		lenSpinner.setLayoutData(fd);
		lenSpinner.setToolTipText("ColorBar length as a percentage of the screen size");

		lenSpinner.setMinimum(10);
		lenSpinner.setMaximum(100);
		lenSpinner.setIncrement(5);
        lenSpinner.setSelection((int)(editedColorBar.getLengthAsRatio()*100));
		lenSpinner.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				editedColorBar.setLengthAsRatio(((float) lenSpinner
						.getSelection()) / 100f);
				refreshColorBar();

			}
		});

		Label percentLabel = new Label(newComp, SWT.NONE);
		percentLabel.setText("%");
		fd = new FormData();
		fd.left = new FormAttachment(lenSpinner, 7, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		percentLabel.setLayoutData(fd);

		Label widthLabel = new Label(newComp, SWT.NONE);
		widthLabel.setText("Width");
		fd = new FormData();
		fd.left = new FormAttachment(anchorCombo, 40, SWT.RIGHT);
		fd.top = new FormAttachment(lengthLabel, 20, SWT.BOTTOM);
		widthLabel.setLayoutData(fd);

		final Spinner widthSpinner = new Spinner(newComp, SWT.BORDER);

		widthSpinner.setMinimum(2);
		widthSpinner.setMaximum(50);
		widthSpinner.setIncrement(1);
		widthSpinner.setSelection( editedColorBar.getWidthInPixels());
		widthSpinner.addModifyListener(new ModifyListener() {
			@Override
			public void modifyText(ModifyEvent e) {
				editedColorBar.setWidthInPixels(widthSpinner.getSelection());
				refreshColorBar();
			}
		});

		fd = new FormData();
		fd.left = new FormAttachment(widthLabel, 20, SWT.RIGHT);
		fd.top = new FormAttachment(lengthLabel, 20, SWT.BOTTOM);
		widthSpinner.setLayoutData(fd);

		Label pixelLabel = new Label(newComp, SWT.NONE);
		pixelLabel.setText("pixels");
		fd = new FormData();
		fd.left = new FormAttachment(widthSpinner, 7, SWT.RIGHT);
		fd.top = new FormAttachment(lengthLabel, 20, SWT.BOTTOM);
		pixelLabel.setLayoutData(fd);

		final Button showLabelsBtn = new Button(newComp, SWT.CHECK);
		showLabelsBtn.setText("Show Label");
		fd = new FormData();
		fd.left = new FormAttachment(pixelLabel, 40, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		showLabelsBtn.setLayoutData(fd);
		showLabelsBtn.setSelection(editedColorBar.getShowLabels());

		showLabelsBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				editedColorBar.setShowLabels(showLabelsBtn.getSelection());
				refreshColorBar();
			}
		});

		Composite labelColorComp = new Composite(newComp, SWT.NONE);
		labelColorComp.setLayout(new FormLayout());
		fd = new FormData();
		fd.left = new FormAttachment(pixelLabel, 25, SWT.RIGHT);
		fd.top = new FormAttachment(showLabelsBtn, 20, SWT.BOTTOM);
		labelColorComp.setLayoutData(fd);
		final ColorButtonSelector labelColorSelector = new ColorButtonSelector(
				labelColorComp, 50, 25);
		labelColorSelector.setColorValue(labelColor.getRGB());
		labelColorSelector.addListener(new IPropertyChangeListener() {
			public void propertyChange(PropertyChangeEvent event) {
				if (labelColor != null) {
					labelColor.dispose();
				}
				labelColor = new Color(colorDevice, labelColorSelector
						.getColorValue());
				editedColorBar.setLabelColor(labelColor.getRGB());
                refreshColorBar();

			}
		});

		Label labelColorLbl = new Label(newComp, SWT.NONE);
		labelColorLbl.setText("Label Color");
		fd = new FormData();
		fd.left = new FormAttachment(pixelLabel, 80, SWT.RIGHT);
		fd.top = new FormAttachment(showLabelsBtn, 20, SWT.BOTTOM);
		labelColorLbl.setLayoutData(fd);

		orientationCombo.add(ColorBarOrientation.Vertical.name());
		orientationCombo.add(ColorBarOrientation.Horizontal.name());

		orientationCombo
				.select(editedColorBar.getOrientation() == ColorBarOrientation.Vertical ? 0
						: 1);

		orientationCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				editedColorBar.setOrientation((orientationCombo
						.getSelectionIndex() == 0 ? ColorBarOrientation.Vertical
						: ColorBarOrientation.Horizontal));
				refreshColorBar();
			}
		});

		for (ColorBarAnchorLocation anchorLoc : availAnchorLocs) {
			anchorCombo.add(anchorLoc.name());
		}
		//
		for (int a = 0; a < availAnchorLocs.length; a++) {
			if (editedColorBar.getAnchorLoc() == availAnchorLocs[a]) {
				anchorCombo.select(a);
			}
		}

		anchorCombo.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				editedColorBar.setAnchorLoc(availAnchorLocs[anchorCombo
						.getSelectionIndex()]);
				refreshColorBar();
			}
		});
		
		
		final Button reverseColorsBtn = new Button(newComp, SWT.CHECK);
		reverseColorsBtn.setText("Reverse colors");
		fd = new FormData();
		fd.left = new FormAttachment(showLabelsBtn, 40, SWT.RIGHT);
		fd.top = new FormAttachment(newComp, 20, SWT.BOTTOM);
		reverseColorsBtn.setLayoutData(fd);
		reverseColorsBtn.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
                  if ( editedColorBar.getReverseOrder())
                	    editedColorBar.setReverseOrder(false);
                  else
                	  editedColorBar.setReverseOrder(true);
				refreshColorBar();
			}
		});
		
	}

	private void createColorMapRangeControlSliders(Composite theColorRangeGroup) {
//		setColorBarFromColorMap(selectedColorMapName, false, true);
		buildColorMapData();

		Composite minMaxValues = new Composite(theColorRangeGroup, SWT.NONE);
		minMaxValues.setLayout(new GridLayout(3, false));
		Label maxLabel = new Label(minMaxValues, SWT.None);
		maxLabel.setText("Max:");

		maxSlider = new Scale(minMaxValues, SWT.HORIZONTAL);
		maxSlider.setMaximum(255);
		maxSlider.setMinimum(0);
		maxSlider.setIncrement(1);
		maxSlider.setSelection(maxSlider.getMaximum());
		GridData layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
		layoutData.minimumWidth = 250;
		maxSlider.setLayoutData(layoutData);

		GridData labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false,
				false);
		// labelLayoutData.widthHint = convertWidthInCharsToPixels(10);
		maxValueText = new Text(minMaxValues, SWT.SINGLE | SWT.BORDER
				| SWT.RIGHT);
		maxValueText.setLayoutData(labelLayoutData);

		Label minLabel = new Label(minMaxValues, SWT.None);
		minLabel.setText("Min:");

		minSlider = new Scale(minMaxValues, SWT.HORIZONTAL);
		minSlider.setMaximum(255);
		minSlider.setMinimum(0);
		minSlider.setIncrement(1);
		minSlider.setSelection(minSlider.getMinimum());
		layoutData = new GridData(SWT.FILL, SWT.DEFAULT, true, false);
		layoutData.minimumWidth = 250;
		minSlider.setLayoutData(layoutData);

		labelLayoutData = new GridData(SWT.FILL, SWT.DEFAULT, false, false);

		minValueText = new Text(minMaxValues, SWT.SINGLE | SWT.BORDER
				| SWT.RIGHT);
		minValueText.setLayoutData(labelLayoutData);

		maxSlider.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (maxSlider.getSelection() <= minSlider.getSelection()) {
					maxSlider.setSelection(minSlider.getSelection() + 1);
				}

				changeMax(maxSlider.getSelection());
				maxValueText.setText(selectionToText(maxSlider.getSelection()));
			}
		});

		minSlider.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (minSlider.getSelection() >= maxSlider.getSelection()) {
					minSlider.setSelection(maxSlider.getSelection() - 1);
				}

				changeMin(minSlider.getSelection());
				minValueText.setText(selectionToText(minSlider.getSelection()));
			}
		});

		maxValueText.addKeyListener(new KeyListener() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == SWT.CR) {
					maxTextChanged();
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {


			}
		});

		minValueText.addKeyListener(new KeyListener() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.character == SWT.CR) {
					minTextChanged();
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {
				

			}
		});

		initializeMinMaxTextWithColorMapRanges();
	}

	private void initializeMinMaxTextWithColorMapRanges() {
		if (cMapParam != null) {
			currentMax = cMapParam.getColorMapMax();
			currentMin = cMapParam.getColorMapMin();
			maxSlider.setSelection(cmapToSelection(currentMax));
			minSlider.setSelection(cmapToSelection(currentMin));
			setMaxText();
			setMinText();
		}
	}

	private void createColorMapComboAndEditButton(Composite colorMapComp) {
		FormData fd = new FormData();
		colorMapComp.setLayout(new FormLayout());
		Label selectColorMapLabel = new Label(colorMapComp, SWT.NONE);
		selectColorMapLabel.setText("Select ColorMap");
		fd = new FormData();
		fd.left = new FormAttachment(colorMapComp, 17, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 20, SWT.BOTTOM);
		selectColorMapLabel.setLayoutData(fd);
		selectColorMapLabel.setVisible(true);
		final Combo colorMapNamesCombo = new Combo(colorMapComp, SWT.READ_ONLY
				| SWT.DROP_DOWN);
		fd = new FormData();
		fd.left = new FormAttachment(selectColorMapLabel, 20, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 17, SWT.BOTTOM);
		colorMapNamesCombo.setLayoutData(fd);

		if (colorMapCategory != null && colorMapCategory != ResourceCategory.NullCategory ) {
			final String[] listOfColorMapNames = ColorMapUtil
					.listColorMaps(colorMapCategory.getCategoryName());
			if (listOfColorMapNames != null && listOfColorMapNames.length > 0) {
				colorMapNamesCombo.setItems(listOfColorMapNames);

				int seldColorMapIndx = -1;
				if (colorMapName != null) {
					for (int c = 0; c < listOfColorMapNames.length; c++) {
						if (listOfColorMapNames[c].compareTo(colorMapName) == 0) {
							colorMapNamesCombo.select(c);
							selectedColorMapName = listOfColorMapNames[c];
							seldColorMapIndx = c;
						}
					}

					if (seldColorMapIndx == -1) {
						System.out.println("The current colorMap '"
								+ colorMapName + "' was not found.");
						seldColorMapIndx = 0;
						colorMapNamesCombo.select(0);
					}
				}

				colorMapNamesCombo.addSelectionListener(new SelectionAdapter() {
					public void widgetSelected(SelectionEvent e) {
						// update the selected color map name in the attrSet and
						// in the colorbar editor.
						selectedColorMapName = new String( listOfColorMapNames[ colorMapNamesCombo.getSelectionIndex() ] );
						if ( ( selectedColorMapName != null && !selectedColorMapName.isEmpty() ) && ( colorMapName != null )) {
							resAttrSet.getRscAttr( "colorMapName" ).setAttrValue( selectedColorMapName );
							theResourceData.setRscAttrSet(resAttrSet);
							setColorBarFromColorMap(selectedColorMapName, true, false);
							refreshColorBar();
						}
					}
				});

			}

		}

		Button editColorsBtn = new Button(colorMapComp, SWT.PUSH);
		editColorsBtn.setText("Edit Colors...");
		fd = new FormData();
		fd.left = new FormAttachment(colorMapNamesCombo, 20, SWT.RIGHT);
		fd.top = new FormAttachment(colorMapComp, 17, SWT.BOTTOM);
		editColorsBtn.setLayoutData(fd);
		editColorsBtn.addSelectionListener(new SelectionAdapter() {

			public void widgetSelected(SelectionEvent e) {
				if (currEditor != null) {
					String cmapEditorCmdStr = "gov.noaa.nws.ncep.viz.colorMapEditor";

					ICommandService service = (ICommandService) currEditor
							.getSite().getService(ICommandService.class);
					Command cmd = service.getCommand(cmapEditorCmdStr);
					if (cmd != null) {
						try {
							HashMap<String, Object> params = new HashMap<String, Object>();
							ExecutionEvent exec = new ExecutionEvent(cmd,
									params, null, null);
							cmd.executeWithChecks(exec);
						} catch (Exception ex) {
							ex.printStackTrace();
							System.out.println("Error executing cmd: "
									+ cmapEditorCmdStr);
						}
					}

					if (currEditor != null)
						currEditor.refresh();
				}
			}

		});

	}

	/**
	 * Creates the sliders to control the brightness and contrast of the image.
	 * Refactored from Raytheon's ImagingDialog class
	 * 
	 * @param mainComp
	 */

	private void initializeABCControls(Composite mainComp) {
		Composite body = new Composite(mainComp, SWT.NONE);
		body.setLayout(new GridLayout(3, false));
		Label label = new Label(body, SWT.BOLD);
		label.setText("Brightness: ");

		brightnessScale = new Scale(body, SWT.NONE);
		brightnessScale.setLayoutData(new GridData(200, SWT.DEFAULT));

		brightnessText = new Label(body, SWT.NONE);
		brightnessText.setLayoutData(new GridData(50, SWT.DEFAULT));

		brightnessScale.setMinimum(0);
		brightnessScale.setMaximum(100);
		brightnessScale.setIncrement(1);
		brightnessScale.setPageIncrement(5);

		brightnessScale.setSelection(((Float) resAttrSet.getRscAttr(
				"brightness").getAttrValue()).intValue() * 100);
		brightnessText.setText(brightnessScale.getSelection() + "%");

		Label label2 = new Label(body, SWT.BOLD);
		label2.setText("Contrast: ");

		contrastScale = new Scale(body, SWT.NONE);
		contrastScale.setLayoutData(new GridData(200, SWT.DEFAULT));

		contrastText = new Label(body, SWT.NONE);
		contrastText.setLayoutData(new GridData(50, SWT.DEFAULT));

		contrastScale.setMinimum(0);
		contrastScale.setMaximum(100);
		contrastScale.setIncrement(1);
		contrastScale.setPageIncrement(5);
		int contrastVal = ((Float) resAttrSet.getRscAttr("contrast")
				.getAttrValue()).intValue() * 100;
		contrastScale.setSelection(contrastVal);
		contrastText.setText(contrastScale.getSelection() + "%");
		initializeAlphaScale(body);

		Composite checkBoxComp = new Composite(body, SWT.NONE);
		checkBoxComp.setLayoutData(new GridData(SWT.CENTER, SWT.CENTER, true,
				true, 3, 1));
		checkBoxComp.setLayout(new GridLayout(2, false));

		interpolationChk = new Button(checkBoxComp, SWT.CHECK);
		interpolationChk.setText("Interpolate");
		GridData gd = new GridData(SWT.LEFT, SWT.CENTER, false, true);
		interpolationChk.setLayoutData(gd);
		interpolationChk.setEnabled(false);
		interpolationChk.setGrayed(true);
		combineNextImage = new Button(checkBoxComp, SWT.CHECK);
		combineNextImage.setText("Combine Next Image Load");
		gd = new GridData(SWT.LEFT, SWT.CENTER, true, true);
		combineNextImage.setLayoutData(gd);
		combineNextImage.setSelection(ImageCombiner.isCombineImages());
		combineNextImage.setEnabled(false);
		combineNextImage.setGrayed(true);
		combineNextImage.addSelectionListener(new SelectionAdapter() {
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Lets call our command
				IHandlerService handlerService = (IHandlerService) PlatformUI
						.getWorkbench().getActiveWorkbenchWindow()
						.getService(IHandlerService.class);
				try {
					handlerService
							.executeCommand(
									"com.raytheon.uf.viz.d2d.ui.actions.imageCombination",
									null);
				} catch (Exception ex) {

				}
				combineNextImage.setSelection(ImageCombiner.isCombineImages());
				if (currEditor != null)
					currEditor.refresh();
			}
		});

		ImageCombiner.addListener(combineNextImageListener);

		brightnessScale.addSelectionListener(new SelectionAdapter() {

			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
			 * .eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				
				brightnessText.setText(brightnessScale.getSelection() + "%");
				
				if (theResourceData != null) {
					float brightnessValue = brightnessScale.getSelection()/100.0f;
					resAttrSet.getRscAttr("brightness").setAttrValue(
							brightnessValue);
					theResourceData.setRscAttrSet(resAttrSet);
					ImagingCapability imgCap = new ImagingCapability();
					imgCap.setBrightness(brightnessValue, false);
					imgCap.setContrast(((Float) resAttrSet.getRscAttr(
							"contrast").getAttrValue()).floatValue(), false);
					imgCap.setAlpha(((Float) resAttrSet.getRscAttr("alpha")
							.getAttrValue()).floatValue(), false);
					((AbstractResourceData) theResourceData)
							.fireChangeListeners(ChangeType.CAPABILITY, imgCap);

				}

			}

		});

		contrastScale.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
			 * .eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				
				contrastText.setText(contrastScale.getSelection() + "%");
				
				if (theResourceData != null) {
					float cValue = contrastScale.getSelection()/100.0f;
					resAttrSet.getRscAttr("contrast").setAttrValue(
							cValue);
					theResourceData.setRscAttrSet(resAttrSet);
					ImagingCapability imgCap = new ImagingCapability();
					imgCap.setContrast(cValue, false);
					
					imgCap.setBrightness(((Float) resAttrSet.getRscAttr(
							"brightness").getAttrValue()).floatValue(), false);
					imgCap.setAlpha(((Float) resAttrSet.getRscAttr("alpha")
							.getAttrValue()).floatValue(), false);
					((AbstractResourceData) theResourceData)
							.fireChangeListeners(ChangeType.CAPABILITY, imgCap);					

				}
				
			}

		});

		interpolationChk.addSelectionListener(new SelectionAdapter() {

			@Override
			public void widgetSelected(SelectionEvent e) {
				if (imageResources != null) {
					for (AbstractNatlCntrsResource<?, ?> rsc : imageResources) {
						ResourceCategory rscCat = rsc.getResourceData().getResourceName()
								.getRscCategory();
						if( rscCat == colorMapCategory ) {
							ImagingCapability imgcap = rsc
									.getCapability(ImagingCapability.class);
							imgcap.setInterpolationState(interpolationChk
									.getSelection());
						}
					}

					if (currEditor != null)
						currEditor.refresh();
				}
			}

		});
	}

	/***
	 * Creates the slider for changing Alpha
	 * 
	 * @param parent
	 */

	private void initializeAlphaScale(Composite parent) {
		alphaLabel = new Label(parent, SWT.BOLD);
		alphaLabel.setText("Alpha: ");
		alphaLabel.setLayoutData(new GridData());

		alphaScale = new Scale(parent, SWT.NONE);
		alphaScale.setLayoutData(new GridData(200, SWT.DEFAULT));

		alphaText = new Label(parent, SWT.NONE);
		alphaText.setLayoutData(new GridData(50, SWT.DEFAULT));

		alphaScale.setMinimum(0);
		alphaScale.setMaximum(100);
		alphaScale.setIncrement(1);
		alphaScale.setPageIncrement(5);

		RscAttrValue alphaRscAttrValue = resAttrSet.getRscAttr("alpha");
        int alphaVal = ((Float) alphaRscAttrValue.getAttrValue()).intValue();
		alphaScale.setSelection(alphaVal * 100);
		alphaText.setText(alphaScale.getSelection() + "%");
		alphaScale.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * 
			 * @see org.eclipse.swt.events.SelectionListener#widgetSelected(org
			 * .eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				alphaText.setText(alphaScale.getSelection() + "%");
				
					
					float alphaValue = (float) (alphaScale.getSelection() / 100.0f);
					resAttrSet.getRscAttr("alpha").setAttrValue(
							alphaValue);

					theResourceData.setRscAttrSet(resAttrSet);
					ImagingCapability imgCap = new ImagingCapability();
					imgCap.setAlpha(alphaValue, false);
					imgCap.setBrightness(((Float) resAttrSet.getRscAttr(
							"brightness").getAttrValue()).floatValue(), false);
					imgCap.setContrast(((Float) resAttrSet.getRscAttr("contrast")
							.getAttrValue()).floatValue(), false);
					if (theResourceData != null) {
					   ((AbstractResourceData) theResourceData).fireChangeListeners(ChangeType.CAPABILITY, imgCap);
					   ColorMapCapability colorCap = new ColorMapCapability();
					  if ( cMapParam != null ){
					     colorCap.setColorMapParameters(cMapParam, false);
					     ((AbstractResourceData) theResourceData).fireChangeListeners(ChangeType.CAPABILITY, colorCap);
					  }
					   
				}
				
			}
		});
	}

	private void setColorBarFromColorMap(String cmapName,
			boolean fireChangeListeners, boolean isFirstTimeSetup) {

		try {
			if ((colorMapCategory != null && colorMapCategory != ResourceCategory.NullCategory )
					&& (cmapName != null && (!cmapName.isEmpty()))

			) {
				theSelectedColorMap = (ColorMap) ColorMapUtil.loadColorMap(
						colorMapCategory.getCategoryName(), cmapName);
			}
		} catch (VizException e) {
			System.out.println(e.getMessage());
			return;
		}
		if (theSelectedColorMap != null) {
			((ColorBarFromColormap) editedColorBar)
					.setColorMap(theSelectedColorMap);
			resAttrSet.getRscAttr("colorMapName").setAttrValue(cmapName);

			cMapCapability = new ColorMapCapability();
			cMapParam = new ColorMapParameters();
			cMapParam.setColorMapMin(0);
			cMapParam.setDataMax( 255 );
			cMapParam.setDataMin( 0 );
			cMapParam.setColorMapMax(255 );
			
	//		initializeAlphaMaskInColorMapParam();

			cMapParam.setColorMap(theSelectedColorMap);
			currentMax = cMapParam.getColorMapMax();
			currentMin = cMapParam.getColorMapMin();
			if(!isFirstTimeSetup){
			   maxSlider.setSelection(cmapToSelection(currentMax));
			   minSlider.setSelection(cmapToSelection(currentMin));
			   setMaxText();
			   setMinText();
			}
			
			cMapCapability.setColorMapParameters(cMapParam, false);
			
			
			if (theResourceData != null && fireChangeListeners) {

				((AbstractResourceData) theResourceData).fireChangeListeners(
						ChangeType.CAPABILITY, cMapCapability);
			}

		}
	}

	private void refreshColorBar() {
		if (theResourceData != null && editedColorBar != null) {
			resAttrSet.setAttrValue("colorBar", editedColorBar);
			theResourceData.setRscAttrSet(resAttrSet);
		}
	}

	private void buildColorMapData() {
		arrayOfSliderTextStrings = new String[256];
		
		if (cMapParam == null && ( colorMapName != null && !colorMapName.isEmpty() ) ){
              setColorBarFromColorMap(colorMapName, false, true );
		}

		if (cMapParam != null) {
			
			cmapMin = cMapParam.getDataMin();
			cmapMax = cMapParam.getDataMax();
			cmapWidth = cmapMax - cmapMin;
			cmapIncrement = cmapWidth / ColorUtil.MAX_VALUE;
			initializeAlphaMaskInColorMapParam();

			float start = cmapMin.floatValue();
			String units = "";

			UnitConverter unitConv = cMapParam.getImageToDisplayConverter();

			Double lastVal = Double.NaN;

			for (int i = 0; i < arrayOfSliderTextStrings.length; ++i) {
				double value = start;

				// handle precision errors
				if (value > cmapMax) {
					// if the difference is .1 the increment between steps
					// assume
					// that cmapMax is ok
					if ((value - cmapMax) < (.1 * cmapIncrement)) {
						value = cmapMax;
					}
				}

				String textStr = "";

				if (cMapParam.isLogarithmic()) {
					// TODO: Handle case where min/max go from neg to pos
					if (cMapParam.getColorMapMax() >= 0
							&& cMapParam.getColorMapMin() >= 0) {
						double index = (i) / ColorUtil.MAX_VALUE;
						value = Math
								.pow(Math.E, (Math.log(cMapParam
										.getColorMapMin()) + (index * (Math
										.log(cMapParam.getColorMapMax()) - Math
										.log(cMapParam.getColorMapMin())))));
					}
					if (format == null) {
						format = new DecimalFormat("0.000");
					}
				}

				if (unitConv != null) {
					value = unitConv.convert(value);

					/*
					 * Check if the last value is non a number.
					 */
					if (lastVal.isNaN()) {
						// If value is not a number then set the text to
						// "NO DATA".
						if (((Double) value).isNaN()) {
							textStr = "NO DATA";
						}
						lastVal = value;
					} else {
						// If value is not a number then prepend ">"
						// to the value.
						if (((Double) value).isNaN()) {
							textStr = "> " + lastVal;
						} else {
							lastVal = value;
						}
					}
				}

				if (!Double.isNaN(value)) {
					int zeros = 0;
					String val = "" + value;
					char[] vals = val.substring(val.indexOf(".") + 1)
							.toCharArray();
					for (int j = 0; j < vals.length; ++j) {
						if (vals[j] == '0') {
							++zeros;
						} else {
							++zeros;
							break;
						}
					}
					zeros = Math.min(3, zeros);

					String f = "0.";
					for (int j = 0; j < zeros; ++j) {
						f += "0";
					}

					if (format == null)
						format = new DecimalFormat(f);
				}

				String txt;
				if (textStr.length() == 0) {
					txt = format.format(value);
				} else {
					txt = textStr;
				}

				if (units != null && units.length() != 0) {
					txt += " " + units;
				}

				arrayOfSliderTextStrings[i] = txt;
				start += cmapIncrement;

			}

		}

	}

	private void initializeAlphaMaskInColorMapParam(){

		if ( cMapParam != null ){
//             int rangeStart = (int) cMapParam.getColorMapMin();
             int rangeEnd = (int) cMapParam.getColorMapMax();
             byte[] mask = cMapParam.getAlphaMask();
             if ( ( mask == null || mask.length == 0 ) && rangeEnd > 0 ){
            	 mask = new byte[rangeEnd];
                 for (int i = 0; i < rangeEnd; ++i) {
                     mask[i] = 1;
       }
             }
             else{
             for (int i = 0; i < mask.length; ++i) {
                            mask[i] = 1;
              }
             }
              cMapParam.setAlphaMask(mask);

		}
	}

	
	private float selectionToCmap(int selection) {
		float percentOffset = selection / 255.0f;
		float value = percentOffset * cmapWidth + cmapMin;
		return value;
	}

	private int cmapToSelection(float value) {
		int selection = (int) ((value - cmapMin) * 255.0f / cmapWidth);
		return selection;
	}

	private void changeMax(int position) {
		// slider min and max is based on the color map, so position is the new
		// color map max
		currentMax = new Float(selectionToCmap(position));
		cMapParam.setColorMapMax(currentMax, true);
		cMapCapability.setColorMapParameters(cMapParam);
		if (theResourceData != null) {
			((AbstractResourceData) theResourceData).fireChangeListeners(
					ChangeType.CAPABILITY, cMapCapability);
		}
	}

	private void changeMin(int position) {
		// slider min and max is based on the color map, so position is the new
		// color map min
		currentMin = new Float(selectionToCmap(position));
		cMapParam.setColorMapMin(currentMin, true);
		cMapCapability.setColorMapParameters(cMapParam);
		if (theResourceData != null) {
			((AbstractResourceData) theResourceData).fireChangeListeners(
					ChangeType.CAPABILITY, cMapCapability);
		}
	}

	private void setColorMapMax(float f) {
		if (currentMax != f) {
			currentMax = f;
			cMapParam.setColorMapMax(f, true);
			if (theResourceData != null) {
				((AbstractResourceData) theResourceData).fireChangeListeners(
						ChangeType.CAPABILITY, cMapCapability);
			}
		}
	}

	private void setColorMapMin(float f) {
		if (currentMin != f) {
			currentMin = f;
			cMapParam.setColorMapMin(f, true);
			if (theResourceData != null) {
				((AbstractResourceData) theResourceData).fireChangeListeners(
						ChangeType.CAPABILITY, cMapCapability);
			}
		}
	}

	private void setMaxText() {
		maxValueText.setText(cmapToText(currentMax));
	}

	private void setMinText() {
		minValueText.setText(cmapToText(currentMin));
	}

	private String cmapToText(double value) {
		String txt = null;

		if (cMapParam != null) {
			UnitConverter unitConv = cMapParam.getImageToDisplayConverter();
			String textStr = "";

			if (unitConv != null) {
				value = unitConv.convert(value);
				if (((Double) value).isNaN()) {
					textStr = "NO DATA";
				}
			}

			if (textStr.length() == 0) {
				txt = format.format(value);
			} else {
				txt = textStr;
			}

		}

		return txt;
	}

	private void maxTextChanged() {
		String text = maxValueText.getText().trim().split(" ")[0];
		try {
			float f = Float.valueOf(text);
			UnitConverter unitConv = cMapParam.getImageToDisplayConverter();
			if (unitConv != null) {
				f = (float) unitConv.inverse().convert(f);
			}
			if (currentMin >= f) {
				statusHandler
						.handle(Priority.ERROR,
								"Maximum of colormap range cannot be below the minimum.");
				setMaxText();
			} else if (f >= cmapMax) {
				setColorMapMax(cmapMax);
				maxSlider.setSelection(255);
				setMaxText();
			} else {
				setColorMapMax(f);
				maxSlider.setSelection(cmapToSelection(f));
			}
		} catch (NumberFormatException ex) {
			statusHandler.handle(Priority.ERROR,
					"Maximum of colormap range cannot be parsed: " + text);
			setMaxText();
		} catch (ConversionException ex) {
			statusHandler.handle(Priority.ERROR, "Unit converter error.", ex);
			setMaxText();
		}

	}

	private void minTextChanged() {
		String text = minValueText.getText().trim().split(" ")[0];
		try {
			float f = Float.valueOf(text);
			UnitConverter unitConv = cMapParam.getImageToDisplayConverter();
			if (unitConv != null) {
				f = (float) unitConv.inverse().convert(f);
			}
			if (f >= currentMax) {
				setMinText();
				statusHandler.handle(Priority.ERROR,
						"Minimum of colormap range cannot exceed the maximum.");
			} else if (cmapMin >= f) {
				setColorMapMin(cmapMin);
				minSlider.setSelection(0);
				setMinText();
			} else {
				setColorMapMin(f);
				minSlider.setSelection(cmapToSelection(f));
			}
		} catch (NumberFormatException ex) {
			statusHandler.handle(Priority.ERROR,
					"Minimum of colormap range cannot be parsed: " + text);
			setMinText();
		} catch (ConversionException ex) {
			statusHandler.handle(Priority.ERROR, "Unit converter error.", ex);
			setMinText();
		}
	}

	private String selectionToText(int selection) {
		String rval = "ERR";

		if (selection > -1 && selection < arrayOfSliderTextStrings.length) {
			// exact match into sliderText array
			rval = arrayOfSliderTextStrings[selection];
		} else {
			statusHandler.handle(Priority.CRITICAL, "index " + selection
					+ " out of range, max "
					+ (arrayOfSliderTextStrings.length - 1));
		}
		return rval;
	}

	@Override
	public void dispose() {
		editedColorBar.dispose();
		super.dispose();
	}

}
