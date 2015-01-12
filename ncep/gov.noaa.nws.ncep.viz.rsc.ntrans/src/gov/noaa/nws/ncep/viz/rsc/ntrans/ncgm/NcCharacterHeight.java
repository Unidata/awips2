/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.CharacterHeight;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.ImageBuilder;

import java.io.DataInput;
import java.io.IOException;

import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.IFont.Style;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 * 
 */
public class NcCharacterHeight extends CharacterHeight implements INcCommand {

    // private final Log logger = LogFactory.getLog(this.getClass());

    public NcCharacterHeight(int ec, int eid, int l, DataInput in)
            throws IOException {
        super(ec, eid, l, in);
    }

    @Override
    public void contributeToPaintableImage(ImageBuilder ib, IGraphicsTarget target,
            PaintProperties paintProps, IDescriptor descriptor) throws VizException {
        // Only change if different from the current size
        if (ib.currentFont.getFontSize() != this.characterHeight) { // TODO
            String currentFontNames = ib.currentFont.getFontName();
            Style[] styles = ib.currentFont.getStyle();
            // ib.currentFont.dispose(); //TODO:recycle after paint
            ib.currentFont = target.initializeFont(currentFontNames,
                    (float) this.characterHeight, styles);
        }
    }

}
