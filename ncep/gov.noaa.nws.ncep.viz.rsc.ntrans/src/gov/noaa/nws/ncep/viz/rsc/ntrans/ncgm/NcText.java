/**
 * 
 */
package gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm;

import gov.noaa.nws.ncep.viz.rsc.ntrans.jcgm.Text;
import gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.ImageBuilder;

import java.io.DataInput;
import java.io.IOException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * @author bhebbard
 * 
 */
public class NcText extends Text implements INcCommand {

    private final Log logger = LogFactory.getLog(this.getClass()); // TODO
                                                                   // static
                                                                   // better??

    static boolean flipflop = true;

    /**
     * @param ec
     * @param eid
     * @param l
     * @param in
     * @throws IOException
     */
    public NcText(int ec, int eid, int l, DataInput in) throws IOException {
        // To handle little-endian strings, we need to bump an odd length ("l")
        // parameter
        // up one to make it even (ending on two-byte CGM word boundary), so
        // that we
        // get the last character. (Will be flipped into place later.) Note that
        // special case of l=31 indicates "long form" (string length >=31 char,
        // to
        // be specified in following 2-byte integer), so the parent constructor
        // for
        // Command has also been modified to interpret l=32 fed up to it as a
        // signal
        // to handle as l=31, then "bump" the long-form length it reads (from
        // next
        // 2 bytes) up to even value if needed.
        super(ec, eid, (l + 1) / 2 * 2, in);
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * gov.noaa.nws.ncep.viz.rsc.ntrans.ncgm.INcCommand#paint(com.raytheon.uf
     * .viz.core.IGraphicsTarget,
     * com.raytheon.uf.viz.core.drawables.PaintProperties,
     * gov.noaa.nws.ncep.viz.rsc.ntrans.rsc.NtransResource.ImageBuilder)
     */
    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps,
            IDescriptor descriptor, ImageBuilder ib) throws VizException {

        DrawableString ds = new DrawableString(this.string, ib.currentLineColor); // TODO:why?
        double[] newpoint = new double[] { 0.0, 0.0 };
        if (flipflop) // TODO test code
        {
            newpoint = ib.scalePoint(this.position.x, this.position.y);
        } else {
            newpoint = ib.scalePointNoZoom(this.position.x, this.position.y);
        }
        // flipflop = ! flipflop ;
        ds.setCoordinates(newpoint[0], newpoint[1]);

        ds.font = ib.currentFont;
        ds.textStyle = ib.textStyle;
        ds.horizontalAlignment = ib.horizontalAlignment;
        ds.verticallAlignment = ib.verticalAlignment;

        ib.strings.add(ds);

        /*
         * IFont font = target.initializeFont("Monospace", 14, new IFont.Style[]
         * { IFont.Style.BOLD}); PixelCoordinate textLoc = new PixelCoordinate(
         * this.position.x * ib.scaling, this.position.y * ib.scaling, 0 );
         * target.drawString(font, this.string, textLoc.getX(), textLoc.getY(),
         * 0.0, TextStyle.NORMAL, new RGB( 255, 255, 200 ),
         * HorizontalAlignment.CENTER, VerticalAlignment.MIDDLE, 0.0);
         */

    }

    public void flipString() {
        // Flip every even char with its odd sibling (endianess reversal)
        String oldString = this.string;
        char[] oldCharArray = oldString.toCharArray();
        int lengthOfNewArray = oldCharArray.length / 2 * 2; // if odd length,
                                                            // discard last
                                                            // character (null)
        char[] newCharArray = new char[lengthOfNewArray];
        for (int i = 0; i < lengthOfNewArray; i = i + 2) {
            newCharArray[i] = oldCharArray[i + 1];
            newCharArray[i + 1] = oldCharArray[i];
        }
        String newString = new String(newCharArray);
        this.string = newString.trim();
    }

}
