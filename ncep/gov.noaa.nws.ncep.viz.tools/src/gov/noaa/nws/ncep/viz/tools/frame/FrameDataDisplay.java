/* FrameDataDisplay
 * 
 * Date Created (16 March 2010)
 * 
 * This code has been developed by the SIB for use in the AWIPS2 system.
 
 */
package gov.noaa.nws.ncep.viz.tools.frame;

import gov.noaa.nws.ncep.viz.ui.display.NCMapDescriptor;
import gov.noaa.nws.ncep.viz.ui.display.NCMapEditor;
import gov.noaa.nws.ncep.viz.ui.display.NmapUiUtils;

import org.eclipse.jface.action.ContributionItem;
import org.eclipse.swt.SWT;
import org.eclipse.swt.SWTException;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.IFrameChangedListener;



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
 * 11/11/11                  G. Hull      implement IVizEditorChangedListener, use raytheon's IFrameChangedListener 
 * 11/11/11                  G. Hull      change to GridLayout, resize and pack Labels on frame change.
 * 11/11/11                  G. Hull      create frameChangelistener in constructor. (ie new instance after a dispose)    
 * 11/22/11      #514        G. Hull      remove editorChangeListener now that this is set by the PerspeciveManager 
 *                                        and this gets updated via refreshGUIElements()
 *                                       
 * </pre>
 * 
 * @author	Archana
 * @version 1.0
 */
public class FrameDataDisplay extends ContributionItem { 

	private Shell shell=null;
	
    private static FrameDataDisplay instance = null;
    
	/**Composite to display the frame counter as well as the time for the current frame*/
	private Composite frameDataComposite;
	
	private Label frameCounterLabel;
	
	private Label frameTimeLabel;
	
	private Font font;

	private NCMapDescriptor mapDescriptor;
	
	private String frameCountString="";
	private String frameTimeString="";
	
	private IFrameChangedListener frameChangelistener = null;
		
	public static FrameDataDisplay createInstance() {
	    if ( instance == null ) {
	        instance = new FrameDataDisplay();
	    }
	    return instance;
	}
	
	private FrameDataDisplay() {
		super();
		
		frameChangelistener = new IFrameChangedListener() {
			@Override
			public
	        void frameChanged( IDescriptor descriptor, DataTime oldTime,
	                		   DataTime newTime ) {

				// if this is the active editor
				//
				if( NmapUiUtils.getActiveNatlCntrsEditor() == descriptor.getRenderableDisplay().getContainer() ) {
					
					// If the widgets are disposed, it means that this listener is no longer valid (the
					// perspective has been deactivated and status bar disposed),So we need to remove this listner.
					// NOTE: it would be nice to remove the listener's when the status bar is disposed but by this
					// time the editors have been removed.
					if( frameCounterLabel == null || frameCounterLabel.isDisposed() ) {
						NmapUiUtils.getActiveNatlCntrsEditor().removeFrameChangedListener( this );
						return;
					}
					
				//chin, fixed issue that GUI component can not be changed by other thread (worker thread)			
					try{  
						Display.getDefault().asyncExec(new Runnable(){  
							public void run(){  
								updateFrameDataDisplay( NmapUiUtils.getActiveNatlCntrsEditor() ); 
							}  
						});  
					}
					catch(SWTException e){  
						System.out.println("updateFrameDataFromWorkerThread: can not run asyncExec()");
					}
				}
			}
		};

	}	

	/***
	 * Creates the composite to display the frame counter and the current time of the frame.
	 * @param the parent status bar manager, where the contribution item should be added.	
	 */
    @Override
	public void fill(Composite parent) {

    	shell = parent.getShell();
    	
    	font = new Font(parent.getDisplay(), "Monospace", 11, SWT.BOLD);
		frameDataComposite = new Composite(parent, SWT.NONE);
		frameDataComposite.setLayout(new GridLayout(2, false));
		
//		frameDataComposite.setBackground( Display.getDefault().getSystemColor(
//              SWT.COLOR_GREEN));
		frameCounterLabel = new Label( frameDataComposite, SWT.NONE );

		frameCounterLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true) );
//		frameCounterLabel.setBackground(Display.getDefault().getSystemColor(
//				SWT.COLOR_BLUE));

		frameCounterLabel.setFont(font);
		frameCounterLabel.setText("");

		frameTimeLabel = new Label(frameDataComposite, SWT.NONE);
		frameTimeLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));
//		frameTimeLabel.setBackground(Display.getDefault().getSystemColor(
//				SWT.COLOR_RED));

		frameTimeLabel.setFont(font);
		frameTimeLabel.setText("");
		
		setVisible(true); 				
	}

    /***
     * Updates the frame number and the the time of the frame, each time
     * the current frame changes.
     *  
     * @param nmapEditor - the active editor in which the frames are loaded or animated.
     */
	private void updateFrameDataDisplay( final NCMapEditor nmapEditor ) {

		frameCountString = "";
		frameTimeString = "";

		if( frameCounterLabel.isDisposed() ) {
			System.out.println("Frame Counter Widget is disposed!");
			return;
		}

		if( nmapEditor != null) {
		
			mapDescriptor = ((NCMapDescriptor) (nmapEditor.getActiveDisplayPane()
											.getRenderableDisplay().getDescriptor()));
			if( mapDescriptor != null ) {
				int currentFrame = mapDescriptor.getCurrentFrame();
				int totalFrames = mapDescriptor.getFrameCount();
				
				if( totalFrames > 0 ) {
					frameCountString = (currentFrame+1) + " of " + totalFrames;
					
					frameTimeString = " " + mapDescriptor.getValidTime( currentFrame );					
				}
			}
		}
		frameCounterLabel.setText( frameCountString );
		frameTimeLabel.setText( frameTimeString );
		shell.layout(true, true);

//		System.out.println("frame counter is "+frameCounterLabel.getText() + " width is "+ frameCounterLabel.getSize().x );
//		System.out.println("frame time is "+frameTimeLabel.getText() + " width is "+ frameTimeLabel.getSize().x );

		shell.layout(true, true);
		frameCounterLabel.pack(true);
		frameTimeLabel.pack(true);		
	}
	
	@Override
	public void update(){
		if(NmapUiUtils.getActiveNatlCntrsEditor()!=null){
			NmapUiUtils.getActiveNatlCntrsEditor().addFrameChangedListener( frameChangelistener );

			updateFrameDataDisplay( NmapUiUtils.getActiveNatlCntrsEditor() );
		}
	}

    @Override
    public void dispose() {
        super.dispose();
                
        // The editors are already gone so we will have to remove the
        // old listeners when we come back to the NCP perspective.
//		AbstractVizPerspectiveManager pMngr = VizPerspectiveListener.getCurrentPerspectiveManager();
//		if( pMngr != null && pMngr.getPerspectiveId().equals( NmapCommon.NatlCntrsPerspectiveID ) ) {			
//			for( AbstractEditor ed : pMngr.getPerspectiveEditors() ) {
//				if( ed instanceof NCMapEditor ) {
//					((NCMapEditor)ed).removeFrameChangedListener( frameChangelistener );
//				}
//			}
//		}

		instance = null;
    }	
}