package gov.noaa.nws.ncep.viz.tools.imageProperties;

import gov.noaa.nws.ncep.viz.resources.AbstractNatlCntrsResource;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import java.util.ArrayList;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.StatusLineLayoutData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Scale;

import com.raytheon.uf.viz.core.IDisplayPane;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.uf.viz.core.rsc.capabilities.ImagingCapability;

/**
 * 
 * Contribution item added to the status bar which displays the image fading
 * information.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 11/5/2009    183        qzhou      Initial created. 
 * 12/15/2009              G. Hull    display and pane listeners.
 * 10/04/2010   289        Archana    Added FadeHotKeyListener
 * 03/07/2011   R1G2-9     G. Hull    implement IVizEditorChangedListener, 
 *                                    editor no longer passed from Pane Changed Listener
 * 06/19/2012   #569       G. Hull    rm IVizEditorChangedListener. update() gets called
 *                                    from refreshGUIElements which is called from perspective's
 *                                    IVizEditorChangedListener
 * 06/21/2012   #632       G. Hull    Change behaviour for multiple images. Activate if all the 
 *                                    brightness's are the same.
 * 07/12/2012   ####       G. Hull    rm paneChangeListener. A pane change will now call refreshGUIElements
 *                       
 * </pre>
 * 
 * @author Q. Zhou
 * @version 1
 */
public class FadeDisplay extends ContributionItem {

    private FadeHotKeyListener fadeKeyListener = null;

	private Composite comp;

	private Scale scale; // = null;

	private Button btn0;// = null;

	private Button btn50;// = null;   

	private Font font = new Font(Display.getCurrent(), "Monospace", 10,
			SWT.NORMAL);
	
    private ArrayList<AbstractNatlCntrsResource<?, ?>> imageResources = null;

    private NCMapEditor activeDisp = null;

	/**
	 * Constructor
	 */
	public FadeDisplay() {
		super();
		imageResources = new ArrayList<AbstractNatlCntrsResource<?, ?>>();
		if(fadeKeyListener == null){
			fadeKeyListener = new FadeHotKeyListener();
		}
	}

	/**
	 * Populates the scale and buttons on the bottom bar
	 */
	@Override
	public void fill(Composite parent) {
        // Shell fadeShell =
        // PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();

		comp = new Composite(parent, SWT.NONE);
		comp.setSize(200, 55);
		GridLayout gl = new GridLayout(3, false);
		gl.marginTop = 0;
		gl.verticalSpacing = 0;
		comp.setLayout(gl);

        StatusLineLayoutData slLayoutData = new StatusLineLayoutData();
//      slLayoutData.heightHint = 30;
//      slLayoutData.widthHint = 300;
        comp.setLayoutData( slLayoutData );

        /*
		 * Add to the bottom bar
		 */
		btn0 = new Button(comp, SWT.PUSH);
		btn50 = new Button(comp, SWT.PUSH);
		scale = new Scale(comp, SWT.NONE);

		btn0.setLayoutData(new GridData(25, 25));
		btn0.setText("0");
		btn0.setFont(font);

		btn0.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				for( AbstractNatlCntrsResource<?,?> rsc : imageResources ) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
					imgCap.setBrightness(0);
				}
                scale.setEnabled(true);
				scale.setSelection(0);
				activeDisp.refresh();				
			}
		});

		btn50.setLayoutData(new GridData(25, 25));
		btn50.setText("N");
		btn50.setFont(font);
		btn50.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				for( AbstractNatlCntrsResource<?,?> rsc : imageResources ) {
                    ImagingCapability imgCap = rsc
                            .getCapability(ImagingCapability.class);
					imgCap.setBrightness(100 / 100.0f);
				}
                scale.setEnabled(true);
				scale.setSelection(100);
				activeDisp.refresh();				
			}
		});

		scale.setLayoutData(new GridData(160, SWT.DEFAULT));
		scale.setMinimum(0);
		scale.setMaximum(200);
		scale.setIncrement(1);
		scale.setPageIncrement(5);
		scale.setSelection(50);

		scale.addSelectionListener(new SelectionAdapter() {
			public void widgetSelected(SelectionEvent e) {
				
                if (imageResources == null) {
                    return;
                }
                for (AbstractNatlCntrsResource<?, ?> imgRsc : imageResources) {
                    ImagingCapability imgCap = imgRsc
							.getCapability(ImagingCapability.class);
					imgCap.setBrightness(scale.getSelection() / 100.0f);
				}
				activeDisp.refresh();
			}
		});
		scale.setEnabled( false );    		
		btn0.setEnabled( false );
		btn50.setEnabled( false );
		scale.setSelection( 0 );

		update(); 
	}

	/**
	 * The display or selected pane has changed so get the new imageResources 
	 * and update the widgets with  
	 * 
	 * @return
	 */
	private void updateFadeDisplay() { // 
		 
        scale.setEnabled(false);
        btn0.setEnabled(false);
        btn50.setEnabled(false);

        NCMapEditor ed = NmapUiUtils.getActiveNatlCntrsEditor();

        if (ed instanceof NCMapEditor) {
            activeDisp = ed;
        }

		if( activeDisp == null ) {
			return;
		}

    	IDisplayPane[] seldPanes = activeDisp.getSelectedPanes();
    	
    	imageResources.clear();
    	
    	for( IDisplayPane pane : seldPanes ) {
    		ResourceList rscList = pane.getDescriptor().getResourceList();
       
            for( ResourcePair rp : rscList ) {
                if (!rp.getProperties().isSystemResource()
                        && rp.getResource().getCapabilities()
                                .hasCapability(ImagingCapability.class)) {
                    imageResources.add((AbstractNatlCntrsResource<?, ?>) rp
                            .getResource());
                }
            }
        }

        int brightness = -1;

        // the buttons will work with multiple resources but
        // the scale will only work with more than one resource if all the
        // brightness
        // values are the same.
        if (!imageResources.isEmpty()) {
            brightness = (int) (imageResources.get(0)
                    .getCapability(ImagingCapability.class).getBrightness() * 100f);

            // TODO : It is possible that a rsc has no image and so there may
            // not be a conflict. Or there may
            // not be an image for the first frame (how does the brightness get
            // set then?)
            //
            for (AbstractNatlCntrsResource<?, ?> imgRsc : imageResources) {
                ImagingCapability imgCap = imgRsc
                        .getCapability(ImagingCapability.class);
                if (brightness != (int) (imgCap.getBrightness() * 100f)) {
                    brightness = -1;
                    scale.setToolTipText("Fade disabled due to multiple images with different brightnesses.");
                    break;
            	}
            }
        } else {
            scale.setToolTipText("");
    	}

        scale.setEnabled(brightness != -1);
		
		btn0.setEnabled( (imageResources.size() >= 1) );
		btn50.setEnabled( (imageResources.size() >= 1) );

        // load the widget with the current value from the image resource or 0
        // if disabled
    	if( scale.isEnabled() ) {
            scale.setSelection(brightness);
        } else {
    		scale.setSelection( 0 );
    	}
	}
	
	@Override
	public void update(){

		updateFadeDisplay();
	}

     @Override
    public void dispose() {
        super.dispose();
    }

    private class FadeHotKeyListener extends KeyAdapter{
		@Override
		public void keyPressed(KeyEvent e) {
		}

       @Override
		public void keyReleased(KeyEvent e) {
			if(e.keyCode == 'I' || e.keyCode == 'i' ){
                if(scale.getSelection() > 0){
               	 scale.setSelection(0);
                } else  if(scale.getSelection() == 0){
               	 scale.setSelection(100);
                }

			}else if(e.keyCode == 'F' || e.keyCode == 'f' ){
				 scale.setSelection(100);
			}
			
			 scale.notifyListeners(SWT.Selection, new Event());
       }
		
	}
}
