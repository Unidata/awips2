package gov.noaa.nws.ncep.viz.resources.colorBar;

import gov.noaa.nws.ncep.viz.ui.display.IColorBar;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
* 
* <pre>
* SOFTWARE HISTORY
* Date         Ticket#     Engineer    Description
* ------------ ----------  ----------- --------------------------
* 07/11/11                  Greg Hull    Initial Creation.
* 
* </pre>
* 
* @author ghull
* @version 1
*/
public class ColorBarResourceData extends AbstractResourceData {

	private IColorBar colorBar;
	
	// default no-arg required to serialize
	public ColorBarResourceData() {
		colorBar = null;
	}

	public ColorBarResourceData( IColorBar cbar ) {
		colorBar = cbar;
	}
	
	@Override
	public AbstractVizResource<?, ?> construct(LoadProperties loadProperties,
			IDescriptor descriptor) throws VizException {
		return new ColorBarResource( this, loadProperties );
	}

	// ?? use this in place of setColorBar?
	@Override
	public void update(Object updateData) {
		// 
	}

	public void setColorBar( IColorBar cbar ) {
		colorBar = cbar;
	}
	
	public IColorBar getColorbar() {
		return colorBar;
	}
	
	@Override
	public boolean equals(Object obj) {
		if( obj instanceof ColorBarResourceData ) {
			return (colorBar == ((ColorBarResourceData)obj).getColorbar() );
		}
		else return false;
	}
}
