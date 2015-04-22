package gov.noaa.nws.ncep.ui.pgen.tools;

import org.eclipse.swt.SWT;

import com.raytheon.viz.ui.input.InputAdapter;

public class InputHandlerDefaultImpl extends InputAdapter {
    
	protected boolean shiftDown;

	@Override
	public boolean handleKeyDown(int keyCode) {
		if ( keyCode == SWT.SHIFT) {
			shiftDown = true;
		}
	
		return true;
	}

	@Override
	public boolean handleKeyUp(int keyCode) {
		if ( keyCode == SWT.SHIFT) {
			shiftDown = false;
		}
		return true;
	}
	
	/**
	 * Does works needed before the handler is set.
	 */
	public void preprocess(){
		
	}
	
}
