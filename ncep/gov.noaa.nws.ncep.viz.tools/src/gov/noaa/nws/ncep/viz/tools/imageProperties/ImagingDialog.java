package gov.noaa.nws.ncep.viz.tools.imageProperties;

import gov.noaa.nws.ncep.viz.common.ColorMapUtil;
import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NCPaneManager;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.ArrayList;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Dialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Scale;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;

import com.raytheon.uf.common.colormap.ColorMap;
import com.raytheon.uf.common.colormap.IColorMap;
import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.BlendableCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;
import com.raytheon.viz.ui.dialogs.colordialog.ColorEditDialog;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.editor.ISelectedPanesChangedListener;

/**
 * Imaging Dialog. This code is based on Raytheon's class which supported Blended
 * Resources. Currently this doesn't support Blended resources but much of the 
 * code is left in place or commented out.   
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * 11/09/2009   187         Q. Zhou     Initial. com.raytheon.viz.ui.actions
 * 12/15/2009               G. Hull     display and pane listeners.
 * 03/25/2010   259         G. Hull     use colormap cat to load colormaps
 * 03/07/2011   migration   G. Hull     use raytheon's ISelectPaneChangeListener,
 *  					                and IVizEditorChangedListener
 * 02/11/13     #972        G. Hull     AbstractEditor instead of NCMapEditor
 * 
 * </pre>
 * 
 * @author chammack
 * @version 1
 */

// TODO : Rework to use IVizEditorChangedListener
public class ImagingDialog extends Dialog implements IPartListener { 
//IVizEditorChangedListener {
    private Shell shell;

    private final String dialogTitle;

    private Scale blendAlphaScale;

//    private Scale brightnessScale;

    private Scale contrastScale;
    private Label contrastText;

    private Scale alphaScale;
    private Label alphaText;
    
    private Button interpolationChk;

    private Button topColorMapButton;

    private Button bottomColorMapButton;

    private static Combo topColorMapsCombo;

    private Combo bottomColorMapsCombo;

    private ArrayList<AbstractNatlCntrsResource<?,?>> imageResources;
    private ArrayList<AbstractNatlCntrsResource<?,?>> cmapResources;

//    private AbstractNatlCntrsResource<?, ?> topResource;
    private AbstractNatlCntrsResource<?, ?> bottomResource; // not used

    private boolean blended = false;

    private AbstractEditor currEditor = null;
    private String currCmapCategory = null;
    
    private ISelectedPanesChangedListener paneListener=null;
    private IWorkbenchPage pageForListener = null;
    
    /**
     * Constructor
     * 
     * @param parentShell
     * @param dialogTitle
     */
    public ImagingDialog(Shell parentShell, String dialogTitle ) {
        super(parentShell, SWT.NONE);

        this.dialogTitle = dialogTitle;

        imageResources = new ArrayList<AbstractNatlCntrsResource<?,?>>();
        cmapResources = new ArrayList<AbstractNatlCntrsResource<?,?>>();

        paneListener = new ISelectedPanesChangedListener() {

			@Override
			public void selectedPanesChanged(String id, IDisplayPane[] pane) {
				if( !id.equals( NCPaneManager.NC_PANE_SELECT_ACTION ) ) {
					return;
				}
				resetDialog();
    		}
    	};

//        if (rsc instanceof BlendedResource) {
//            topResource = ((BlendedResourceData) ((BlendedResource) rsc)
//                    .getResourceData()).getResourceList().get(0).getResource();
//            bottomResource = ((BlendedResourceData) ((BlendedResource) rsc)
//                    .getResourceData()).getResourceList().get(1).getResource();
//            blended = true;
//        } else {
//            this.topResource = rsc;
//        }
    }
    
    // the display or pane has changed so we need to get the current editor
    // and set the resources based on the currently selected panes.
    //
    private  void resetDialog( ) {
    	if( contrastScale.isDisposed() ) {
    		return;
    	}
    	IDisplayPane[] seldPanes = NcEditorUtil.getSelectedPanes( currEditor );
    	
    	imageResources.clear();
    	cmapResources.clear();
    	
    	for( IDisplayPane pane : seldPanes ) {
            ResourceList rscList = pane.getDescriptor().getResourceList();
       
            for( ResourcePair rp : rscList ) {
            	if( rp.getResource().getCapabilities().hasCapability( ImagingCapability.class )) {
            		imageResources.add( (AbstractNatlCntrsResource<?, ?>)rp.getResource() );            		
            	}
            	if( rp.getResource().getCapabilities().hasCapability( ColorMapCapability.class )) {
            		cmapResources.add( (AbstractNatlCntrsResource<?, ?>)rp.getResource() );
            	}
            }
    	}
            
    	if( imageResources.size() == 1 ) {
    		contrastScale.setEnabled( true );
    		alphaScale.setEnabled( true );
    		
    		// load the widget with the current value from the image resource
    		ImagingCapability imgCap = imageResources.get(0).getCapability(ImagingCapability.class); 
    		
    		contrastScale.setSelection( (int) (imgCap.getContrast() * 100.0f) );
    		contrastText.setText(contrastScale.getSelection() + "%");

    		alphaScale.setSelection( (int) (imgCap.getAlpha() * 100.0f) );
            alphaText.setText(alphaScale.getSelection() + "%");
    	}
    	else {
    		contrastScale.setEnabled( false );
    		alphaScale.setEnabled( false );
    	}

    	if( !cmapResources.isEmpty() ) {
    		topColorMapsCombo.setEnabled( true );
    		// get the category of the first resource and load the appropriate colormaps    	
    		currCmapCategory = cmapResources.get(0).
    		           getResourceData().getResourceName().getRscCategory().getCategoryName();

    		//
    		String[] colormaps = getColormaps( currCmapCategory );
    		topColorMapsCombo.setItems( colormaps );

    		ColorMapCapability cmapCap = cmapResources.get(0).
    		                                 getCapability(ColorMapCapability.class); 
    		ColorMapParameters cmapParams = cmapCap.getColorMapParameters();
    		
    		if( cmapParams == null ) {
            	topColorMapsCombo.setEnabled( false );
            	//topColorMapButton.setText("No Top Image.");
            	topColorMapButton.setEnabled(false);    			
    		}
    		else if( cmapParams.getColorMapName() != null ){
    			topColorMapsCombo.setText( cmapParams.getColorMapName() );
    		}
    	}
    	else {        	
        	topColorMapsCombo.setEnabled( false );
        	//topColorMapButton.setText("No Top Image.");
        	topColorMapButton.setEnabled(false);
    	}
    	    	
    	currEditor.refresh();
    }

    /**
     * Opens the dialog (makes visible).
     * 
     * @return Null
     */
    public Object open() {

        shell = new Shell(getParent(), SWT.DIALOG_TRIM);
        shell.setText(dialogTitle);

        // Set the shell layout to a Grid layout.
        shell.setLayout(new GridLayout(1, false));

        initializeControls();

    	currEditor = NcDisplayMngr.getActiveNatlCntrsEditor();
    	
    	NcEditorUtil.addSelectedPaneChangedListener( currEditor, paneListener );
        resetDialog();
        
        pageForListener = currEditor.getSite().getPage();;
        pageForListener.addPartListener( this );
        
        shell.pack();

        // Open the shell to display the dialog.
        shell.open();

        // Wait until the shell is disposed.
        Display display = getParent().getDisplay();
        while (!shell.isDisposed()) {
            if (!display.readAndDispatch()) {
                display.sleep();
            }
        }

    	pageForListener.removePartListener( this );

        return null;
    }

    /**
     * Initialize controls on the display.
     */
    private void initializeControls() {

        Composite mainComp = new Composite(shell, SWT.NONE);
        mainComp.setLayout(new GridLayout(1, false));
        mainComp.setLayoutData(new GridData(SWT.FILL, SWT.DEFAULT, true, false));

        GridData buttonGD = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonGD.widthHint = 250;
        topColorMapButton = new Button(mainComp, SWT.PUSH);
        topColorMapButton.setAlignment(SWT.CENTER);
        topColorMapButton.setLayoutData(buttonGD);

        GridData comboGD = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        comboGD.widthHint = 250;
        topColorMapsCombo = new Combo(mainComp, SWT.SINGLE | SWT.DROP_DOWN
                | SWT.V_SCROLL | SWT.H_SCROLL);
        topColorMapsCombo.setLayoutData(comboGD);

        topColorMapsCombo.addSelectionListener(new SelectionAdapter() {
        	@Override
        	public void widgetSelected(SelectionEvent anE) {
        		int idx = topColorMapsCombo.getSelectionIndex();
        		String colormapfile = topColorMapsCombo.getItem(idx);

        		try {
        			ColorMap cxml = (ColorMap) ColorMapUtil.loadColorMap( 
        					                      currCmapCategory, colormapfile );


        			// apply the selected color map to all the resources that
        			// are in the appropriate category.
        			//
        			for( AbstractNatlCntrsResource<?, ?> rsc : cmapResources ) {
        				String rscCat = rsc.getResourceData().
        								getResourceName().getRscCategory().getCategoryName();
        				if( rscCat.equalsIgnoreCase( currCmapCategory ) ) {
        					ColorMapCapability cmcap = rsc.getCapability(ColorMapCapability.class);
//        					String currentCMap = cmcap.getColorMapParameters().
//        					getColorMap().getName();
//        					topColorMapsCombo.setText(currentCMap);
                			ColorMap glColorMap = new ColorMap( colormapfile, cxml );
        					cmcap.getColorMapParameters().setColorMap( glColorMap );
        				}
        			}
        			currEditor.refresh();
        		} catch (VizException e) {
        			e.printStackTrace();
        		}
        	}
        });

//       String topResourceName = (topResource).getName();
        // Truncate the name for the button if it is too long.
//        if (topResourceName.length() > 25) {
//        	topColorMapButton.setToolTipText("Edit "); // + topResourceName);
//        	topResourceName = topResourceName.substring(0, 24) + "...";
//        } else {
//        	topColorMapButton.setToolTipText(null);
//        }
        topColorMapButton.setText("Edit..."); // + topResourceName);

        topColorMapButton.addSelectionListener(new SelectionAdapter() {
        	@Override
        	public void widgetSelected(SelectionEvent e) {
        		ColorMapCapability[] cmCap = new ColorMapCapability[1];
        		cmCap[0] = cmapResources.get(0).
        		                    getCapability( ColorMapCapability.class);
        		//ColorEditDialog ced = new ColorEditDialog(shell, cmCap);
        		//ced.open();
        		ColorEditDialog.openDialog(shell, currEditor, null, false, true);
        		currEditor.refresh();
        	}
        });

        // Blended not implemented..
//        if (blended) {
//            initializeBlendedAlphaScale(mainComp);
//            initializeBottomControls(mainComp, colormaps);
//        }

        Composite body = new Composite(mainComp, SWT.NONE);

        body.setLayout(new GridLayout(3, false));
        
//        Label label = new Label(body, SWT.BOLD);
//        label.setText("Brightness: ");
//
//        brightnessScale = new Scale(body, SWT.NONE);
//        brightnessScale.setLayoutData(new GridData(200, SWT.DEFAULT));
//
//        final Label brightnessText = new Label(body, SWT.NONE);
//        brightnessText.setLayoutData(new GridData(50, SWT.DEFAULT));
//
//        brightnessScale.setMinimum(0);
//        brightnessScale.setMaximum(100);
//        brightnessScale.setIncrement(1);
//        brightnessScale.setPageIncrement(5);

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

        if (!blended) {
            initializeAlphaScale(body);
        }

        GridData gd = new GridData();
        gd.horizontalSpan = 2;
        interpolationChk = new Button(body, SWT.CHECK);
        interpolationChk.setText("Interpolate");
        interpolationChk.setLayoutData(gd);
        interpolationChk.setEnabled(false);  // Blended not supported

//        if (imageResource != null
//                && imageResource.getCapabilities().hasCapability(
//                        ImagingCapability.class)) {
//            final ImagingCapability imgCap = imageResource
//                    .getCapability(ImagingCapability.class);
//            brightnessScale
//                    .setSelection((int) (imgCap.getBrightness() * 100.0f));
//
//            brightnessText.setText(brightnessScale.getSelection() + "%");
//
//            brightnessScale.addSelectionListener(new SelectionAdapter() {
//
//                /*
//                 * (non-Javadoc)
//                 * 
//                 * @see
//                 * org.eclipse.swt.events.SelectionListener#widgetSelected(org
//                 * .eclipse.swt.events.SelectionEvent)
//                 */
//                @Override
//                public void widgetSelected(SelectionEvent e) {
//                    imgCap
//                            .setBrightness(brightnessScale.getSelection() / 100.0f);
//                    brightnessText
//                            .setText(brightnessScale.getSelection() + "%");
//                    ((AbstractEditor) VizApp.getCurrentEditor()).refresh();
//                }
//
//            });

            contrastScale.addSelectionListener(new SelectionAdapter() {
                @Override
                public void widgetSelected(SelectionEvent e) {
                	// if this is enabled then there is only one Resource.
                	ImagingCapability imgCap = imageResources.get(0).
                	 						getCapability(ImagingCapability.class);                	 
                	imgCap.setContrast( contrastScale.getSelection() / 100.0f);
                    contrastText.setText( contrastScale.getSelection() + "%" );
                    
                    currEditor.refresh();
                }
            });

// not implemented : 
//            interpolationChk.setSelection(imgCap.isInterpolationState());
//
//            interpolationChk.addSelectionListener(new SelectionAdapter() {
//
//                @Override
//                public void widgetSelected(SelectionEvent e) {
//                    imgCap.setInterpolationState(interpolationChk
//                            .getSelection());
//                    currEditor.refresh();
//                }
//            });
//        } else {
////            brightnessScale.setEnabled(false);
//            contrastScale.setEnabled(false);
//            interpolationChk.setEnabled(false);
//        }
        
        GridData gd1 = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        buttonGD.widthHint = 250;
        Button closeButton = new Button(mainComp, SWT.PUSH);
        closeButton.setText("  Close   ");
        
        closeButton.setAlignment(SWT.CENTER);
        closeButton.setLayoutData(gd1);
        closeButton.addSelectionListener(new SelectionAdapter() {
            @Override
            public void widgetSelected(SelectionEvent e) {
            	NcEditorUtil.removeSelectedPaneChangedListener( currEditor, paneListener );
            
            	paneListener = null;
            	shell.dispose();
            }
        });

    }

    private void initializeBlendedAlphaScale(Composite parent) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 200;
        blendAlphaScale = new Scale(parent, SWT.NONE);
        blendAlphaScale.setLayoutData(gd);

        blendAlphaScale.setMinimum(0);
        blendAlphaScale.setMaximum(BlendableCapability.BLEND_MAX);
        blendAlphaScale.setIncrement(1);
        blendAlphaScale.setPageIncrement(1);

        // Blended currently not supported.
//        if (blended) {
//            final int blendableAmt = imageResource.getCapability(
//                    BlendableCapability.class).getAlphaStep();
//
//            blendAlphaScale.setSelection(blendableAmt);
//
//            blendAlphaScale.addSelectionListener(new SelectionAdapter() {
//
//                /*
//                 * (non-Javadoc)
//                 * 
//                 * @see
//                 * org.eclipse.swt.events.SelectionListener#widgetSelected(org
//                 * .eclipse.swt.events.SelectionEvent)
//                 */
//                @Override
//                public void widgetSelected(SelectionEvent e) {
//                    imageResource.getCapability(BlendableCapability.class)
//                            .setAlphaStep(blendAlphaScale.getSelection());
//
//                    currEditor.refresh();
//                }
//
//            });
//        } else {
//            blendAlphaScale.setEnabled(false);
//        }
    }

    private void initializeBottomControls(Composite parent, String[] colormaps) {
        GridData gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 250;
        bottomColorMapsCombo = new Combo(parent, SWT.SINGLE | SWT.DROP_DOWN
                | SWT.V_SCROLL | SWT.H_SCROLL);
        bottomColorMapsCombo.setLayoutData(gd);
        bottomColorMapsCombo.setItems(colormaps);

        gd = new GridData(SWT.CENTER, SWT.DEFAULT, true, false);
        gd.widthHint = 250;
        bottomColorMapButton = new Button(parent, SWT.PUSH | SWT.CENTER);
        bottomColorMapButton.setLayoutData(gd);

        bottomColorMapButton.setAlignment(SWT.CENTER);

        if (bottomResource != null
                && bottomResource.getCapabilities().hasCapability(
                        ColorMapCapability.class)) {
        	final ColorMapCapability[] bottomCap = new ColorMapCapability[1];
    		bottomCap[0] = bottomResource
                    .getCapability(ColorMapCapability.class);
            IColorMap bcmap = bottomCap[0].getColorMapParameters().getColorMap();
            
            if (bcmap == null) {
                bottomColorMapsCombo.setText("Not Selected");
            } else {
                bottomColorMapsCombo.setText(bottomCap[0].getColorMapParameters()
                        .getColorMap().getName());
            }
            bottomColorMapsCombo.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent anE) {
                    int idx = bottomColorMapsCombo.getSelectionIndex();

                    String colormapfile = bottomColorMapsCombo.getItem(idx);

                    try {
                        IColorMap cxml = ColorMapUtil
                                .loadColorMap( currCmapCategory, colormapfile);
                        ColorMap glColorMap = new ColorMap(colormapfile,
                                (ColorMap) cxml);

                        bottomCap[0].getColorMapParameters().setColorMap(
                                glColorMap);

                        currEditor.refresh();
                    } catch (VizException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }

                }

            });

            String bottomResourceName = (bottomResource).getName();

            // Truncate the name for the button if it is too long.
            if (bottomResourceName.length() > 25) {
                bottomColorMapButton.setToolTipText("Edit "
                        + bottomResourceName);
                bottomResourceName = bottomResourceName.substring(0, 24)
                        + "...";
            } else {
                bottomColorMapButton.setToolTipText(null);
            }

            bottomColorMapButton.setText("Edit " + bottomResourceName);

            bottomColorMapButton.addSelectionListener(new SelectionAdapter() {

                @Override
                public void widgetSelected(SelectionEvent e) {
                    //ColorEditDialog ced = new ColorEditDialog(shell, bottomCap);
                    //ced.open();
            		ColorEditDialog.openDialog(shell, currEditor, null, false, true);

                    currEditor.refresh();
                }

            });
        } else {
            bottomColorMapButton.setText("Bottom image is not displayed.");
            bottomColorMapButton.setEnabled(false);
            bottomColorMapsCombo.setEnabled(false);
        }
    }

    private void initializeAlphaScale(Composite parent) {
        Label alphaLabel = new Label(parent, SWT.BOLD);
        alphaLabel.setText("Alpha: ");

        alphaScale = new Scale(parent, SWT.NONE);
        alphaScale.setLayoutData(new GridData(200, SWT.DEFAULT));

        alphaText = new Label(parent, SWT.NONE);
        alphaText.setLayoutData(new GridData(50, SWT.DEFAULT));

        alphaScale.setMinimum(0);
        alphaScale.setMaximum(100);
        alphaScale.setIncrement(1);
        alphaScale.setPageIncrement(5);

        alphaScale.addSelectionListener(new SelectionAdapter() {
        	@Override
        	public void widgetSelected(SelectionEvent e) {
        		// if this is enabled then there is only one Resource.
        		ImagingCapability imgCap = imageResources.get(0).
        		getCapability(ImagingCapability.class);                	 
        		imgCap.setAlpha( alphaScale.getSelection() / 100.0f);
        		alphaText.setText( alphaScale.getSelection() + "%" );

        		currEditor.refresh();
        	}
        });
    }

    /**
     * Get a list of colormaps in the colormaps directory
     * 
     * @return the colormap names
     */
    private static String[] getColormaps( String cat ) {
    	
        return ColorMapUtil.listColorMaps( cat );
    }
    
//    public void dispose() {
//    	pageForListener.removePartListener( this );
//    }
	
    // Part listener methods to get notified when the active Display changes or when the 
    // selected panes change.
    //    	
    private void updateEditor( AbstractNcEditor ed ) {            	
    	if( ed != currEditor ) {
    		NcEditorUtil.removeSelectedPaneChangedListener( currEditor, paneListener );
    		currEditor = ed;
    		NcEditorUtil.addSelectedPaneChangedListener( currEditor, paneListener );

    		resetDialog();
    	}
    }
    
    public void partActivated(IWorkbenchPart part) {
    	if( !(part instanceof AbstractNcEditor) ) {
    		return;
    	}
//    	System.out.println("partActivated : "+ ((NCMapEditor)part).getDisplayName());
    	updateEditor( (AbstractNcEditor) part );
    }

    public void partBroughtToTop(IWorkbenchPart part) {
//    	System.out.println("partBroughtToTop : "+ ((NCMapEditor)part).getDisplayName());
    }

    public void partClosed(IWorkbenchPart part) {
//    	System.out.println("partClosed : "+ ((NCMapEditor)part).getDisplayName());
    }

    public void partDeactivated(IWorkbenchPart part) {
    }

    public void partOpened(IWorkbenchPart part) {
    	if( !(part instanceof AbstractNcEditor) ) {
    		return;
    	}
//    	System.out.println("partOpened : "+ ((NCMapEditor)part).getDisplayName());
    	//           	updateEditor( (NCMapEditor) part );
    }

//	@Override
//	public void editorChanged(IDisplayPaneContainer container) {
//		// TODO Auto-generated method stub
//		
//	}
}
