/*
 * gov.noaa.nws.ncep.ui.pgen.tools.PgenVolcanoCreateTool
 * 
 * October 2010
 *
 * This code has been developed by the SIB for use in the AWIPS2 system.
 */

package gov.noaa.nws.ncep.viz.tools.frame;

import gov.noaa.nws.ncep.viz.ui.display.AbstractNcEditor;
import gov.noaa.nws.ncep.viz.ui.display.NcEditorUtil;
import gov.noaa.nws.ncep.viz.ui.display.NcDisplayMngr;

import java.util.TreeSet;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IDescriptor.FramesInfo;
import com.raytheon.uf.viz.core.drawables.IRenderableDisplay;
import com.raytheon.uf.viz.core.rsc.ResourceList;
import com.raytheon.viz.ui.editor.AbstractEditor;
import com.raytheon.viz.ui.tools.AbstractTool;

/**
 * The class for skipping bad frames action
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date       	Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * 10/10		#309		G. Zhang   	Initial Creation.
 * 02/11/13     #972        G. Hull     AbstractEditor instead of NCMapEditor
 *
 * </pre>
 * 
 * @author	G. Zhang
 */
public class BadFrameAction  extends AbstractTool {
	
	private final static Logger THE_LOG = Logger.getLogger(BadFrameAction.class);
	
	private final static String ERROR_MESSAGE = "The editor or frame is invalid! ";
	
	private final static String WARNING_MESSAGE_NOFRAMES = "There is no frame to be tagged as a bad frame! ";
	
	private final static String WARNING_MESSAGE_LASTFRAME = "The last frame cannot be tagged as a bad frame! ";

	private static int nxtIdx = 0;
	
	private final static int NOT_VALID_INDEX = -1;

	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.viz.ui.tools.AbstractTool#runTool()
	 */
	@Override
	public Object execute(ExecutionEvent arg0) throws ExecutionException {
		
		super.execute(arg0);
		
		AbstractEditor editor = NcDisplayMngr.getActiveNatlCntrsEditor();
		if( editor == null ) 
			return null;
		
		if( editor.getActiveDisplayPane() == null ) 
			return null;			

		IRenderableDisplay disp = editor.getActiveDisplayPane().getRenderableDisplay();
		if( disp == null ) 
			return null;
		
		IDescriptor idtor= disp.getDescriptor();
		if( idtor == null) 
			return null;	
		
		ResourceList rscList = idtor.getResourceList();		
		if( (rscList == null) || (rscList.size() == 0) ) 
			return null;		
		
		//get the Shell for dialogs
		Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
		
		FramesInfo fi = idtor.getFramesInfo();
		
		//no frames
		if(fi.getFrameCount()==0){
			
			MessageDialog.openError(shell, "Warning", WARNING_MESSAGE_NOFRAMES);
			return null;
			
		}	
		
		//the last frame cannot be tagged as a bad frame
		if( isLastFrame( fi ) ){	
			
			MessageDialog.openError(shell, "Warning", WARNING_MESSAGE_LASTFRAME);
			return null;
			
		}
		
		//get user input: OK or Cancel
		Object userResponse = new BadFrameMngDlg(shell).open();
		
		
		//if user choose OK, we proceed to skip the frame		
		if( userResponse instanceof Boolean ){
			if( ((Boolean) userResponse).booleanValue() ){				
				 				
				try{
					
					int index = fi.getFrameIndex();	
					fi.getFrameTimes()[index].setVisible(false);
					
					nxtIdx = getNxtIdx(idtor, index);
					
					fi = new FramesInfo(fi.getFrameTimes(), nxtIdx);
				}catch(Throwable t){
					
					THE_LOG.setLevel(Level.INFO);	
					THE_LOG.info(ERROR_MESSAGE);				
					
					return null;
				}					
				
				editor.refresh();
				NcEditorUtil.refreshGUIElements( editor );
			}
		}else{
			return null;
		}
		
		return null;
	}
	
	/*
	 * get the next visible frame index
	 */
	private int getNxtIdx(IDescriptor idtor, int skipIdx){
		DataTime[] dt = idtor.getFrames();
		
		TreeSet<Integer> set = new TreeSet<Integer>();
		
		for(int i=0; i<dt.length; i++){
			
			if( dt[i].isVisible()  ){				
				set.add(i);
				
				if( i > skipIdx )
					return i;
			}
		}
		
		return set.size() > 0
				? set.first()
				: NOT_VALID_INDEX;
	}
	
	/*
	 * check if this is the last frame
	 */
	private boolean isLastFrame( FramesInfo info ){
		
		if( info.getFrameCount() == 1 )
			return true;
		
		int counter = 0;
		
		for(DataTime dt : info.getFrameTimes()){
			
			if(dt.isVisible()){
				
				counter++;
				
				if(counter > 1) // short cut
					return false;
			}
		}
		
		return true; // if counter is 0 or 1
		
	}

}
