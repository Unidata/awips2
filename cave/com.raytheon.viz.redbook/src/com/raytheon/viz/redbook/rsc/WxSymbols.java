/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.viz.redbook.rsc;

import java.awt.image.BufferedImage;
import java.awt.image.RenderedImage;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.swt.graphics.RGB;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.IRenderedImageCallback;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.point.svg.SVGImageFactory;

/**
 * Create weather symbol images to be drawn on the target.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Aug 11, 2014  3504     mapeters  Replaced deprecated IODataPreparer
 *                                  instances with IRenderedImageCallback.
 * Oct 27, 2015  4798     bsteffen  Extend SVGImageFactory
 * 
 * </pre>
 */
public class WxSymbols extends SVGImageFactory {

    private final Map<Character, IImage> images = new HashMap<>();

    private IGraphicsTarget target;

    private RGB color;

    public WxSymbols() throws VizException {
        super("redbook"
                + IPathManager.SEPARATOR + "RedbookSymbolText.svg");
    }


    public IImage getImage(IGraphicsTarget target, RGB color, char c) {

        if (this.target != target || this.color == null
                || !this.color.equals(color)) {
            this.target = target;
            this.color = new RGB(color.red, color.green, color.blue);

            images.clear();

            /*
             * // this doesn't work... blank images...
             * textNode.setAttribute("stroke", String.format("RGB(%d,%d,%d",
             * color.red, color.green, color.blue));
             */
            /*
             * Attr a =
             * textNode.getAttributeNode("stroke");//AbstractMethodError//have
             * to use setAttribute
             * a.setTextContent(String.format("RGB(%d,%d,%d", color.red,
             * color.green, color.blue));
             */
        }

        IImage image = images.get(c);
        if (image == null) {

            // textText.setTextContent(Character.toString(c));

            /*
             * Because the XML character set is restricted, we cannot define a
             * font to support low ASCII characters in an SVG file. Use the
             * Unicode PUA at U+E000 instead.
             */
            Element textNode = document.getElementById("theText");
            Text textText = (Text) textNode.getFirstChild();
            textText.setNodeValue(Character.toString((char) (c + 0xe000)));

            final BufferedImage bufferedImage = createSingleColorImage(color,
                    12, 12);

            image = target.initializeRaster(new IRenderedImageCallback() {
                @Override
                public RenderedImage getImage() throws VizException {
                    return bufferedImage;
                }
            });
            images.put(c, image);
        }
        return image;
    }
}
