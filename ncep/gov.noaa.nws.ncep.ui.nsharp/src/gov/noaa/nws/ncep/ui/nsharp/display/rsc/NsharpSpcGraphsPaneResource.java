package gov.noaa.nws.ncep.ui.nsharp.display.rsc;
/**
 * 
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    	Engineer    Description
 * -------		------- 	-------- 	-----------
 * 07/10/2012	229			Chin Chen	Initial coding
 *
 * </pre>
 * 
 * @author Chin Chen
 * @version 1.0
 */

import gov.noaa.nws.ncep.ui.nsharp.NsharpConstants;
import gov.noaa.nws.ncep.ui.nsharp.display.NsharpAbstractPaneDescriptor;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

public class NsharpSpcGraphsPaneResource extends NsharpAbstractPaneResource{
	private int spcXOrig;
	private int spcYOrig;
	private int spcWidth;
	private int spcHeight;
	public NsharpSpcGraphsPaneResource(AbstractResourceData resourceData,
			LoadProperties loadProperties, NsharpAbstractPaneDescriptor desc) {
		super(resourceData, loadProperties, desc);
		
	}
	
	@SuppressWarnings("deprecation")
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		super.paintInternal(target, paintProps);
		//defineCharHeight(font10);
		if(rscHandler== null)
			return;
		this.font10.setSmoothing(false);
		this.font10.setScaleFont(false);
		target.drawLine(spcXOrig+ spcWidth/2, spcYOrig, 0.0, spcXOrig+ spcWidth/2, spcYOrig+spcHeight, 0.0, NsharpConstants.color_white, 1);
		target.drawString(this.font10,"FUTURE display", spcXOrig+ spcWidth/2,
				spcYOrig + spcHeight/2, 0.0,
				IGraphicsTarget.TextStyle.NORMAL,
				NsharpConstants.color_green,
				HorizontalAlignment.CENTER,  
				VerticalAlignment.BOTTOM, null);
	}

	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {
		super.initInternal(target);
	}
	@Override
	protected void disposeInternal() {
		
		super.disposeInternal();
	}
	@Override
	public void handleResize() {
		
		super.handleResize();
		IExtent ext = getDescriptor().getRenderableDisplay().getExtent();
		ext.reset();
		spcXOrig = (int) (ext.getMinX());
		spcYOrig = (int) ext.getMinY();
		spcWidth = (int) (ext.getWidth());
		spcHeight = (int) ext.getHeight();
	}
}
