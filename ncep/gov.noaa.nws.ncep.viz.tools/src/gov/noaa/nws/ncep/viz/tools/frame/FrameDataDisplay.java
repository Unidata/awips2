/* FrameDataDisplay
 * 
 * Date Created (16 March 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 
 */
package gov.noaa.nws.ncep.viz.tools.frame;

import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor.IFrameChangedListener;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.jface.action.StatusLineLayoutData;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;

import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeMode;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FrameChangeOperation;



/**
 * Contribution item added to the status bar in the National Centers Perspective
 * to display the frame counter and the time of the displayed frame
 *<p>
 * <pre>
 * SOFTWARE HISTORY
 *    Date       Ticket#	 Engineer	    Description
 * -----------------------------------------------------------
 * 16-Mar-2010   238, 239    Archana       Initial Creation.
 * 08-Apr-2010   239         Archana      Changed the method name
 *                                        from getCurrentTimeFrame(int)
 *                                        to getValidTime(int).
 * 20-May-2010   238, 239    Archana      Increased the spacing for the labels                                                
 * 07/15/11                  C Chen       fixed frame number not updated while looping problem
 *                                              
 * </pre>
 * 
 * @author	Archana
 * @version 1.0
 */


public class FrameDataDisplay extends ContributionItem{

    private static FrameDataDisplay instance = null;
    
	/**Composite to display the frame counter as well as the time for the current frame*/
	private Composite frameDataComposite;
	
	
	private Label frameCounterLabel;
	
	private Label frameTimeLabel;
	
	private Font font;

	private NCMapDescriptor mapDescriptor;
	
	private int currentFrame;
	
	private int totalFrames;
	
	private IFrameChangedListener listener = new IFrameChangedListener() {
		@Override
		public void frameChanged(FrameChangeOperation operation, FrameChangeMode mode  ){
			//System.out.println("updateFrameDataDisplay 1");
			//chin, fixed issue that GUI component can not be changed by other thread (worker thread)			
			//updateFrameDataFromWorkerThread( NmapUiUtils.getActiveNatlCntrsEditor() );
			try{  
				Display.getDefault().asyncExec(new Runnable(){  
					public void run(){  
						updateFrameDataDisplay(NmapUiUtils.getActiveNatlCntrsEditor() ); 
					}  
				});  
				}catch(SWTException e){  
					System.out.println("updateFrameDataFromWorkerThread: can not run asyncExec()");
				}  
		}
	};
	
	public static void updateInstance() {
	    if ( instance != null ) {
	        instance.update();
	    }
	}
	
	public static FrameDataDisplay createInstance() {
	    if ( instance == null ) {
	        instance = new FrameDataDisplay();
	    }
	    return instance;
	}
	
	/**
	 *Default  Constructor
	 */
	private FrameDataDisplay() {
		super();
	}	

	/***
	 * Creates the composite to display the frame counter and the current time of the frame.
	 * @param the parent status bar manager, where the contribution item should be added.	
	 */
    @Override
	public void fill(Composite parent) {

    	font = new Font(parent.getDisplay(), "Monospace", 11, SWT.BOLD);
		frameDataComposite = new Composite(parent, SWT.NONE);
		frameDataComposite.setSize(150,25);
		frameDataComposite.setLayout(new GridLayout(2, false));

		
        StatusLineLayoutData slLayoutData = new StatusLineLayoutData();
//      slLayoutData.heightHint = 30;
//      slLayoutData.widthHint = 300;
      frameDataComposite.setLayoutData( slLayoutData );
//      frameDataComposite.setBackground( Display.getDefault().getSystemColor(
//              SWT.COLOR_GREEN));

		frameCounterLabel = new Label(frameDataComposite, SWT.NONE);
		frameCounterLabel.setFont(font);
		frameCounterLabel.setEnabled(true);
		frameCounterLabel.setText("          ");

		frameTimeLabel = new Label(frameDataComposite, SWT.NONE);
		frameTimeLabel.setFont(font);
		frameTimeLabel.setEnabled(true);
		frameTimeLabel.setText("                ");
		
		setVisible(true); 		

	}

    /***
     * Updates the frame number and the the time of the frame, each time
     * the current frame changes.
     *  
     * @param nmapEditor - the active editor in which the frames are loaded or animated.
     */
	private void updateFrameDataDisplay( final NCMapEditor nmapEditor ) {
		
		if (nmapEditor != null) {
			nmapEditor.removeFrameChangedListener(listener);
			nmapEditor.addFrameChangedListener(listener);
			mapDescriptor = ((NCMapDescriptor) (nmapEditor.getActiveDisplayPane()
					.getRenderableDisplay()
					.getDescriptor()));
			if (mapDescriptor != null) {
				currentFrame = mapDescriptor.getCurrentFrame();
				totalFrames = mapDescriptor.getFrameCount();
				//System.out.println("curFram="+currentFrame+ " totalFrame="+totalFrames);
				if(totalFrames > 0){
					frameCounterLabel.setText((currentFrame+1) + " of "
							+ totalFrames);
				}else{
					frameCounterLabel.setText("               ");
				}
				
				frameTimeLabel.setText(" " + mapDescriptor
						.getValidTime(currentFrame));
			}
		}

	}
	
	@Override
	public void update(){

		//System.out.println("updateFrameDataDisplay 2");
		updateFrameDataDisplay(NmapUiUtils.getActiveNatlCntrsEditor() );
	}

    @Override
    public void dispose() {
        super.dispose();
        instance = null;
    }
	
}






















