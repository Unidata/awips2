package gov.noaa.nws.ncep.viz.tools.panZoom;


import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.RenderingOrderFactory.ResourceOrder;

/**
 * Draws rectangle on the screen to show zoom box
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 18, 2010            ghull        Copied from ZoomToolResource
 * 
 * </pre>
 * 
 * @author ghull
 * @version 1.0
 */
public class NcZoomToolResource extends
      AbstractVizResource<NcZoomToolResourceData, IDescriptor> {

	/**
	 * @param resourceData
	 * @param loadProperties
	 */
	protected NcZoomToolResource( NcZoomToolResourceData resourceData,
			LoadProperties loadProperties) {
		super(resourceData, loadProperties);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#disposeInternal()
	 */
	@Override
	protected void disposeInternal() {
		recycle();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractVizResource#paintInternal(com.raytheon
	 * .uf.viz.core.IGraphicsTarget,
	 * com.raytheon.uf.viz.core.drawables.PaintProperties)
	 */
	@Override
	protected void paintInternal(IGraphicsTarget target,
			PaintProperties paintProps) throws VizException {
		Rectangle zoomRect = resourceData.getHandler().getZoomRect();
		if (zoomRect != null) {
			target.drawRect(new PixelExtent(zoomRect), new RGB(255, 255, 0),
					1.0f, 1.0);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.raytheon.uf.viz.core.rsc.AbstractVizResource#initInternal(com.raytheon
	 * .uf.viz.core.IGraphicsTarget)
	 */
	@Override
	protected void initInternal(IGraphicsTarget target) throws VizException {

	}

	@Override
	public ResourceOrder getResourceOrder() {
		return ResourceOrder.HIGHEST;
	}

}
