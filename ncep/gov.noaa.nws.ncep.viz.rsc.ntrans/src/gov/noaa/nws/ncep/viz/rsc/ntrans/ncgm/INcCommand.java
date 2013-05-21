/**
 *  Interface INcCommand
 *  
 *  Most classes in this package extend corresponding classes in the
 *  open source "jcgm" package, to implement this interface.
 *  
 *  This gives them the ability to draw themselves in the AWIPS II
 *  (IGraphicsTarget) world.  More precisely, they contribute themselves
 *  to an image being built, whose state is kept in the ImageBuilder parameter.
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 *
 */

public interface INcCommand {

	 public void paint(IGraphicsTarget target, PaintProperties paintProps,
			 IDescriptor descriptor, ImageBuilder ib) throws VizException;
	 
}

