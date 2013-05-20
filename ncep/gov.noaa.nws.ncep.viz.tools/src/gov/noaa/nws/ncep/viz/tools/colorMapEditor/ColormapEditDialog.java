package gov.noaa.nws.ncep.viz.tools.colorMapEditor;

import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.common.LockedColorMaps;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.io.File;
import java.text.DecimalFormat;
import java.util.ArrayList;

import javax.measure.converter.UnitConverter;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.viz.core.drawables.ColorMapParameters;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.viz.ui.EditorUtil;
import com.raytheon.viz.ui.dialogs.colordialog.ColorData;
import com.raytheon.viz.ui.dialogs.colordialog.ColorUtil;
import com.raytheon.viz.ui.dialogs.colordialog.ColorWheelComp;
import com.raytheon.viz.ui.dialogs.colordialog.IColorBarAction;
import com.raytheon.viz.ui.dialogs.colordialog.IColorWheelAction;

/**
 * This is the main dialog for the Color Edit Dialog.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 *                         lvenable    Initial Creation.
 * Jul 24, 2007            njensen     Hooked into backend.
 * Apr 10, 2010   #259     ghull       Copied and modified from Raytheon
 * July 18 2011   #450     ghull       use NcPathManager
 * Feb 10 2012    #686     sgurung     Added fix for java.lang.IndexOutOfBoundsException while updating colormaps
 * March 15 2012  #621     sgurung     Check for locked colormaps
 * 02/11/13      #972        G. Hull     AbstractEditor instead of NCMapEditor
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class ColormapEditDialog extends Dialog implements IColorBarAction,
        IColorWheelAction {
    private Shell shell;

    /**
     * Upper color wheel (composite object).
     */
    private ColorWheelComp upperColorWheel;

    /**
     * Lower color wheel (composite object).
     */
    private ColorWheelComp lowerColorWheel;

    /**
     * Color bar (composite object).
     */
    private ColorBarViewer colorBar;

    /**
     * RGB radio button.
     */
    private Button rgbRdo;

    /**
     * HSB radio button.
     */
    private Button hsbRdo;

    /**
     * Interpolate button.
     */
    private Button interpolateBtn;

    /**
     * Undo button.
     */
    private Button undoBtn;

    /**
     * Redo button.
     */
    private Button redoBtn;

    /**
     * Revert button.
     */
    private Button revertBtn;

    /**
     * Save button.
     */
    private Button saveBtn;

    /**
     * Delete button.
     */
    private Button deleteBtn;

    /**
     * Title for the upper color wheel.
     */
    private final String upperWheelTitle = " Upper Color ";

    /**
     * Title for the lower color wheel.
     */
    private final String lowerWheelTitle = " Lower Color ";

    /**
     * Array of text to be displayed on the upper and lower sliders of the color
     * bar.
     */
    private String[] sliderText;

    /**
     * Array of color data that represents the starting colors to be displayed.
     */
    private ArrayList<ColorData> colorArray;

//    private final ColorMapCapability cap;
//
    private ColorMapParameters cmapParams;
    private ColorMap colorMap;

    private String seldCmapCat;
    private String seldCmapName;

    private ArrayList<String> availColorMaps;
    private String[]          availColorMapCats;
    
    private  Combo selCmapCombo;
    
    private LockedColorMaps lockedCmaps;
    
    /**
     * Constructor.
     * 
     * @param parent
     *            Parent shell.
     */
    public ColormapEditDialog(Shell parent, String cmapName ) { //, ColorMapCapability aCap) {
        super(parent, 0);
        seldCmapName = cmapName; // cap.getColorMapParameters().getColorMapName();
        cmapParams = new ColorMapParameters();
//        cmapParams.setColorMapMin(0);
//        cmapParams.setColorMapMax(256);
//        colorMap = new ColorMap( seldCmapName, new float[256], new float[256], new float[256] );
//        cmapParams.setColorMap( colorMap );
    }

    /**
     * Opens the dialog (makes visible).
     * 
     * @return Null
     */
    public Object open() {
        // Create a new shell object and set the text for the dialog.
        Shell parent = getParent();
        shell = new Shell(parent, SWT.DIALOG_TRIM | SWT.MIN);
        shell.setText("Edit Colormaps");

        // Setup all of the layouts and the controls.
        setup();

        // Open the shell to display the dialog.
        shell.open();

        // Wait until the shell is disposed.
        Display display = parent.getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch())
                display.sleep();
        }

        return null;
    }

    /**
     * Setup all of the layouts and controls.
     */
    private void setup() {
        // Set the shell layout to a Grid layout.
        shell.setLayout(new GridLayout(1, false));
        
        // Read lockedColorMaps.tbl to get the list of locked color maps
        lockedCmaps = ColorMapUtil.readLockedColorMapFile();
        
        availColorMaps = new ArrayList<String>();
        availColorMapCats = ColorMapUtil.getColorMapCategories();
        
        if( seldCmapCat == null ) {
        	seldCmapCat = availColorMapCats[0];
        }
        
//      for( String cat : availColorMapCats ) {
        for( String cmap : ColorMapUtil.listColorMaps(seldCmapCat) ) {
        	if( seldCmapName == null ) {
        		seldCmapName = cmap;
        		if( !initColorMap() ) {
            		seldCmapName = null;
        			continue; // don't add to the list
        		}
        	}
        	availColorMaps.add(cmap);
        }
//        }
        	
        createSliderData();

        // Initialize the components.
        initComponents();        
        
        // Pack the components.
        shell.pack();
    }
    
    private boolean initColorMap( ) {
		try {
			colorMap = (ColorMap) ColorMapUtil.loadColorMap( seldCmapCat, seldCmapName, lockedCmaps != null && lockedCmaps.isLocked(seldCmapName));
			cmapParams.setColorMap(colorMap);
			cmapParams.setColorMapMin(0);
			cmapParams.setColorMapMax(colorMap.getSize()-1);
			return true;
		} catch (VizException e) {
			System.out.println("Error Loading colorMap "+seldCmapCat+File.separator+seldCmapName);
			return false;
		}
    }

    /**
     * Initialize the dialog components.
     */
    private void initComponents() {
        // Create the RGB and the HSB radio buttons.
        createRgbHsbButtons();

        
        
        ColorData initial = new ColorData(new RGB(255, 255, 255), 255);

        // Create the upper color wheel for the display.
        upperColorWheel = new ColorWheelComp(shell, this, upperWheelTitle);
        // upperColorWheel.setColor(colorArray.get(0));
        upperColorWheel.setColor(initial);

        // Create the color bar object that is displayed
        // in the middle of the dialog.
        colorBar = new ColorBarViewer(shell, this, sliderText, cmapParams);

        // Create the lower color wheel for the display.
        lowerColorWheel = new ColorWheelComp(shell, this, lowerWheelTitle);
        // lowerColorWheel.setColor(colorArray.get(colorArray.size() - 1));
        lowerColorWheel.setColor(initial);

        // Create the bottom control buttons.
        createBottomButtons();
    }

    /**
     * Create the RGB and the HSB radio buttons and the select colormapname dropdown.
     */
    private void createRgbHsbButtons() {
    	
        Composite comp1 = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, false);
        gl.horizontalSpacing = 10;
        comp1.setLayout(gl);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        comp1.setLayoutData(gd);

        // Create a group to contain the RGB and HSB radio buttons.
        Group colorGroup = new Group(comp1, SWT.NONE);
        colorGroup.setText(" Use color model: ");

        RowLayout groupRowLayout = new RowLayout();
        groupRowLayout.marginLeft = 10;
        groupRowLayout.marginRight = 10;
        groupRowLayout.spacing = 10;
        colorGroup.setLayout(groupRowLayout);

        // Create the RGB radio button. When the radio button is selected
        // update the upper and lower color wheel objects to display the RGB
        // sliders.
        rgbRdo = new Button(colorGroup, SWT.RADIO);
        rgbRdo.setText("RGB");
        rgbRdo.setSelection(true);
        rgbRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeColorWheels();
            }
        });

        // Create the HSB radio button. When the radio button is selected
        // update the upper and lower color wheel objects to display the HSB
        // sliders.
        hsbRdo = new Button(colorGroup, SWT.RADIO);
        hsbRdo.setText("HSB");
        hsbRdo.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                changeColorWheels();
            }
        });
        
        Composite catComp = new Composite(comp1, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.horizontalSpacing = 20;
        catComp.setLayout(gl);
        gd = new GridData(GridData.CENTER);
        gd.horizontalAlignment = SWT.CENTER;
        
        catComp.setLayoutData(gd);

        Label selCatLbl = new Label( catComp, SWT.None );
        selCatLbl.setText("Category");
        
        final Combo selCmapCatCombo = new Combo( catComp, SWT.DROP_DOWN | SWT.READ_ONLY );
        selCmapCatCombo.setItems( availColorMapCats );
        selCmapCatCombo.select(0);
        
        selCmapCatCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		String seldCat = selCmapCatCombo.getText();
        		if( seldCat.equals( seldCmapCat ) ) {
        			return;
        		}
        		
        		seldCmapCat = seldCat;
        		seldCmapName = null;
        		availColorMaps.clear();
        		
                for( String cmap : ColorMapUtil.listColorMaps(seldCmapCat) ) {
                	if( seldCmapName == null ) {
                		seldCmapName = cmap;
                		if( !initColorMap() ) {
                    		seldCmapName = null;
                			continue; // don't add to the list
                		}
                	}
                	availColorMaps.add(cmap);
                }
                	
                createSliderData();
                selCmapCombo.setItems( availColorMaps.toArray(new String[0] ) );
                selCmapCombo.select(0);
                colorBar.setNewColorMap(sliderText, colorArray);
        	}
        });
        
        Composite cmapComp = new Composite(comp1, SWT.NONE);
        gl = new GridLayout(1, false);
        gl.horizontalSpacing = 10;
        cmapComp.setLayout(gl);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        cmapComp.setLayoutData(gd);

        Label selCmapLbl = new Label( cmapComp, SWT.None );
        selCmapLbl.setText("Colormap");
        
        selCmapCombo = new Combo( cmapComp, SWT.DROP_DOWN );
        selCmapCombo.setItems( availColorMaps.toArray(new String[0] ) );
        selCmapCombo.select(0);
        
        selCmapCombo.addSelectionListener( new SelectionAdapter() {
        	public void widgetSelected(SelectionEvent event) {
        		String seldCmap = selCmapCombo.getText();
        		if( seldCmap.equals( seldCmapName ) ) {
        			return;
        		}
        		
        		seldCmapName = seldCmap;

        		if( initColorMap() ) {
                    createSliderData();
        		}
                colorBar.setNewColorMap(sliderText, colorArray);
        	}
        });
        
//        selCmapCombo.addModifyListener( new ModifyListener() {
//			@Override
//			public void modifyText(ModifyEvent e) { 
//				seldCmapName = selCmapCombo.getText();
//			}
//        });
    }

    /**
     * Create the bottom control buttons.
     */
    private void createBottomButtons() {
        // Create a composite that will contain the control buttons.
        Composite bottonBtnComposite = new Composite(shell, SWT.NONE);
        GridLayout gl = new GridLayout(3, true);
        gl.horizontalSpacing = 10;
        bottonBtnComposite.setLayout(gl);
        GridData gd = new GridData(GridData.FILL_HORIZONTAL);
        bottonBtnComposite.setLayoutData(gd);

        // Create the Interpolate button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        interpolateBtn = new Button(bottonBtnComposite, SWT.PUSH);
        interpolateBtn.setText("Interpolate");
        interpolateBtn.setLayoutData(gd);
        interpolateBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ColorData upperColorData = upperColorWheel.getColorData();
                ColorData lowerColorData = lowerColorWheel.getColorData();

                colorBar.interpolate(upperColorData, lowerColorData, rgbRdo
                        .getSelection());
                undoBtn.setEnabled(true);
                updateColorMap();
            }
        });

        // Create the Undo button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        undoBtn = new Button(bottonBtnComposite, SWT.PUSH);
        undoBtn.setText("Undo");
        undoBtn.setEnabled(false);
        undoBtn.setLayoutData(gd);
        undoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                undoBtn.setEnabled(colorBar.undoColorBar());
                updateColorMap();
                redoBtn.setEnabled(true);
            }
        });

        // Create the Redo button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        redoBtn = new Button(bottonBtnComposite, SWT.PUSH);
        redoBtn.setText("Redo");
        redoBtn.setEnabled(false);
        redoBtn.setLayoutData(gd);
        redoBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                redoBtn.setEnabled(colorBar.redoColorBar());
                updateColorMap();
                undoBtn.setEnabled(true);
            }
        });

        // Create the Revert button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        revertBtn = new Button(bottonBtnComposite, SWT.PUSH);
        revertBtn.setText("Revert");
        revertBtn.setLayoutData(gd);
        revertBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                colorBar.revertColorBar();
                updateColorMap();
                undoBtn.setEnabled(false);
                redoBtn.setEnabled(false);
            }
        });

        // Create the Save button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        saveBtn = new Button(bottonBtnComposite, SWT.PUSH);
        saveBtn.setText("Save");
        saveBtn.setLayoutData(gd);
        if( seldCmapName == null ) {
            saveBtn.setEnabled(false);
        }
        
        saveBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
                ColorMap cm = (ColorMap) cmapParams.getColorMap();
                seldCmapName = selCmapCombo.getText();
                
//                int sepIndx = seldCmapName.indexOf(File.separator);
//                String cmapCat = seldCmapName.substring(0,seldCmapName.indexOf(File.separator));
//                String cmapName = seldCmapName.substring( seldCmapName.indexOf(File.separator));
                if (lockedCmaps != null && lockedCmaps.isLocked(seldCmapName)) {
                	MessageDialog confirmDlg = new MessageDialog( 
                			NcDisplayMngr.getCaveShell(), 
                			"Save Colormap", null, 
                			"Colormap " +seldCmapCat+File.separator +seldCmapName + 
                			" already exists and is locked.\n\n" +
                			"You cannot overwrite it.",
                			MessageDialog.INFORMATION, new String[]{"OK"}, 0);
                	confirmDlg.open();
                	colorBar.undoColorBar();
                    updateColorMap();
                	return;
                }                
                else if( ColorMapUtil.colorMapExists( seldCmapCat, seldCmapName ) ) {
                	MessageDialog confirmDlg = new MessageDialog( 
                			NcDisplayMngr.getCaveShell(), 
                			"Save Colormap", null, 
                			"Colormap " +seldCmapCat+File.separator +seldCmapName + 
                			" already exists.\n\n" +
                			"Do you want to overwrite it?",
                			MessageDialog.QUESTION, new String[]{"Yes", "No"}, 0);
                	confirmDlg.open();

                	if( confirmDlg.getReturnCode() == MessageDialog.CANCEL ) {
                		return;
                	}
                }

                try {
                    ColorMapUtil.saveColorMap( cm, seldCmapCat, seldCmapName );
                    
                    MessageDialog msgDlg = new MessageDialog( 
                			NcDisplayMngr.getCaveShell(), 
                			"Colormap Saved", null, 
                			"Colormap " +seldCmapCat+File.separator +seldCmapName + 
                			" Saved.",
                			MessageDialog.INFORMATION, new String[]{"OK"}, 0);
                	msgDlg.open();
                } catch (VizException e) {
                    MessageDialog msgDlg = new MessageDialog( 
                			NcDisplayMngr.getCaveShell(), 
                			"Error", null, 
                			"Error Saving Colormap " +seldCmapCat+File.separator +seldCmapName + 
                			"\n"+e.getMessage(),
                			MessageDialog.ERROR, new String[]{"OK"}, 0);
                	msgDlg.open();
                }

                completeSave();
            }
        });


        // 
        // Create the Delete button.
        gd = new GridData(GridData.FILL_HORIZONTAL);
        gd.grabExcessHorizontalSpace = false;
        deleteBtn = new Button(bottonBtnComposite, SWT.PUSH);
        deleteBtn.setText("Delete");
        deleteBtn.setLayoutData(gd);
        deleteBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	deleteColormap();
            }
        });
        Label sep = new Label(shell, SWT.SEPARATOR|SWT.HORIZONTAL);
        gd = new GridData(GridData.FILL_HORIZONTAL);
        sep.setLayoutData(gd);

        // 
        // Create the Delete button.
        gd = new GridData(GridData.HORIZONTAL_ALIGN_END);
        Button closeBtn = new Button(shell, SWT.PUSH);
        closeBtn.setText("   Close   ");
        closeBtn.setLayoutData(gd);
        closeBtn.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent event) {
            	shell.dispose();
            }
        });
    }

    /**
     * Change the upper and lower color wheel objects to display either RGB or
     * HSB.
     */
    private void changeColorWheels() {
        if (rgbRdo.getSelection() == true) {
            upperColorWheel.showRgbSliders(true);
            lowerColorWheel.showRgbSliders(true);
        } else {
            upperColorWheel.showRgbSliders(false);
            lowerColorWheel.showRgbSliders(false);
        }
    }

    private void createSliderData() {
        sliderText = new String[256];
        colorArray = new ArrayList<ColorData>();

        DecimalFormat format = new DecimalFormat("0.0");

        float difference = cmapParams.getColorMapMax()
                - cmapParams.getColorMapMin();
        float increment = difference / ColorUtil.MAX_VALUE;
        float start = cmapParams.getColorMapMin();
        String units = "";

        UnitConverter unitConv = cmapParams.getImageToDisplayConverter();

        Double lastVal = Double.NaN;

        for (int i = 0; i < sliderText.length; ++i) {
            double value = start;
            String textStr = "";

            if (unitConv != null) {
                value = unitConv.convert(start);

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

            String txt;

            /*
             * If textStr doesn't have any text then set txt to the value in the
             * value variable.
             */
            if (textStr.length() == 0) {
                txt = format.format(value);
            } else {
                txt = textStr;
            }

            if (units != null) {
                txt += " " + units;
            }

            sliderText[i] = txt;
            start += increment;
        }

        colorArray = ColorUtil.buildColorData((ColorMap)cmapParams.getColorMap());

        /*
         * If the colorArray is less then 256, then we spread all of the
         * elements of the colorArray out to fill a 256 element array.
         */
        if (colorArray.size() < 256) {
            ArrayList<ColorData> tmpColorArray = new ArrayList<ColorData>(256);
            double adjustNum = colorArray.size() / 256.0;

            ColorData colorData;

            for (int j = 0; j < 256; j++) {

                int index = (int) Math.round(j * adjustNum);
                index = (index < colorArray.size() ? index : colorArray.size()-1 );
                colorData = new ColorData(colorArray.get(index).rgbColor,
                        colorArray.get(index).alphaValue);
                tmpColorArray.add(colorData);
            }

            colorArray = new ArrayList<ColorData>(tmpColorArray);
        }
    }

    /**
     * Fill the area between the sliders in the color bar using the color data
     * provided.
     * 
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     */
    public void fillColor(ColorData colorData) {
        colorBar.fillColorBarColor(colorData);
        updateColorMap();
        undoBtn.setEnabled(true);
    }

    /**
     * Set the color where the top or bottom slider is pointing. The color wheel
     * title is used to determine if the color is from the upper color wheel or
     * the lower color wheel.
     * 
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     * @param colorWheelTitle
     *            The title of the color wheel that is calling the method.
     */
    public void setColor(ColorData colorData, String colorWheelTitle) {
        if (colorWheelTitle.compareTo(upperWheelTitle) == 0) {
            colorBar.setColorBarColor(colorData, true);
        } else if (colorWheelTitle.compareTo(lowerWheelTitle) == 0) {
            colorBar.setColorBarColor(colorData, false);
        }
        updateColorMap();
        undoBtn.setEnabled(true);
    }

    /**
     * A callback method used by the ColorBar class. This method is called to
     * update the upper or lower color wheel when the mouse is clicked in the
     * color bar and moved around.
     * 
     * @param colorData
     *            The color data object containing the RGB color and the alpha
     *            value.
     * @param upperFlag
     *            A flag indicating if the upper or lower color wheel is to be
     *            updated.
     */
    public void updateColor(ColorData colorData, boolean upperFlag) {
        if (upperFlag) {
            upperColorWheel.setColor(colorData);
        } else {
            lowerColorWheel.setColor(colorData);
        }
    }

    /**
     * Updates the color map currently displayed
     */
    private void updateColorMap() {
        //if (colorMap == null) {
            colorMap = ColorUtil.buildColorMap(colorBar.getCurrentColors(),
                    null);
       /* } else {
            colorMap = ColorUtil.updateColorMap(colorBar.getCurrentColors(),
                    colorMap);
        }*/
        cmapParams.setColorMap(colorMap);
        cmapParams.setColorMapName(null);
        ((AbstractNcEditor) EditorUtil.getActiveEditor()).refresh();
    }

    /**
     * Resets the colorbar to an initial state with the newly saved data
     */
    private void completeSave() {
        colorBar.updateRevertToCurrent();
        colorBar.revertColorBar();
        undoBtn.setEnabled(false);
        redoBtn.setEnabled(false);
        cmapParams.setColorMapName(seldCmapName);
        saveBtn.setEnabled(true);
    }

    private void deleteColormap() {
    	try{
    		ColorMapUtil.deleteColorMap(seldCmapCat, seldCmapName);

    		MessageDialog msgDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Deleted", null, 
    				"Colormap " +seldCmapCat+File.separator +seldCmapName + 
    				" Deleted.",
    				MessageDialog.INFORMATION, new String[]{"OK"}, 0);
    		msgDlg.open();

    		shell.dispose();
//    		availColorMaps = new ArrayList<String>();
//
//    		seldCmapName = null;
//    		selCmapCombo.setText("");
//
//    		for( String cmap : ColorMapUtil.listColorMaps(seldCmapCat) ) {
//    			if( seldCmapName == null ) {
//    				seldCmapName = cmap;
//            		if( initColorMap() ) {
//                        createSliderData();
//            		}
//            		else {
//            			seldCmapName = null;
//    					continue; // don't add to the list
//    				}
//                    colorBar.setNewColorMap(sliderText, colorArray);
//
//    			}
//    			availColorMaps.add(cmap);
//    		}

    	} catch (VizException e) {
    		MessageDialog msgDlg = new MessageDialog( 
    				NcDisplayMngr.getCaveShell(), 
    				"Error", null, 
    				"Error Deleting Colormap " +seldCmapCat+File.separator +seldCmapName + 
    				"\n"+e.getMessage(),
    				MessageDialog.ERROR, new String[]{"OK"}, 0);
    		msgDlg.open();
    	}
    }
   
}