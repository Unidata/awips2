/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import java.io.DataInput;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.CharacterHeight;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder;

/**
 * @author bhebbard
 *
 */
public class NcCharacterHeight extends CharacterHeight implements INcCommand {

	private final Log logger = LogFactory.getLog(this.getClass());  //TODO static better??

	/**
	 * @param ec
	 * @param eid
	 * @param l
	 * @param in
	 * @throws IOException
	 */
	public NcCharacterHeight(int ec, int eid, int l, DataInput in)
			throws IOException {
		super(ec, eid, l, in);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf.viz.core.IGraphicsTarget, com.raytheon.uf.viz.core.drawables.PaintProperties, com.raytheon.uf.viz.core.drawables.IDescriptor, gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
	 */
	@Override
	public void paint(IGraphicsTarget target, PaintProperties paintProps,
			IDescriptor descriptor, ImageBuilder ib) throws VizException {
		//  Only change if different from the current size
		if (ib.currentFont.getFontSize() != this.characterHeight) {  //TODO
			String currentFontNames = ib.currentFont.getFontName();
			Style[] styles = ib.currentFont.getStyle();
			//ib.currentFont.dispose();  //TODO:recycle after paint
			ib.currentFont = target.initializeFont(currentFontNames,
					(float) this.characterHeight, styles);
		}
	}

}
