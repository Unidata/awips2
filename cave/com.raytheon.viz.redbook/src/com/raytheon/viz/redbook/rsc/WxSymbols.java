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

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.util.HashMap;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Text;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.data.prep.IODataPreparer;
import com.raytheon.uf.viz.core.drawables.IImage;

public class WxSymbols {
    private IGraphicsTarget target;

    private RGB color;

    private HashMap<Character, IImage> images = new HashMap<Character, IImage>();

    private Document document;

    private Element textNode;

    private Text textText;

    private final GVTBuilder builder;

    private final BridgeContext bridgeContext;

    public WxSymbols() {
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
        try {
            document = f.createDocument(PathManagerFactory.getPathManager()
                    .getStaticFile(
                            "redbook" + File.separator
                                    + "RedbookSymbolText.svg").toURI()
                    .toString());
        } catch (MalformedURLException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        } catch (IOException e1) {
            // TODO Auto-generated catch block
            e1.printStackTrace();
        }
        textNode = document.getElementById("theText");
        textText = (Text) textNode.getFirstChild();

        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    private IndexColorModel tm;

    public IImage getImage(IGraphicsTarget target, RGB color, char c) {

        if (this.target != target || this.color == null
                || !this.color.equals(color)) {
            byte[] red = { 0, (byte) color.red };
            byte[] green = { 0, (byte) color.green };
            byte[] blue = { 0, (byte) color.blue };
            this.target = target;
            this.color = new RGB(color.red, color.green, color.blue);

            tm = new IndexColorModel(8, 2, red, green, blue, 0);
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

            // Is this code going to convert the font every time?
            // should use the bridge context to get the TextNode and change
            // that instead of the origonal DOM and rebuild..

            BufferedImage bufferedImage = new BufferedImage(12, 12,
                    BufferedImage.TYPE_BYTE_INDEXED, tm);
            Graphics2D g2d = bufferedImage.createGraphics();

            // textText.setTextContent(Character.toString(c));

            /*
             * Because the XML character set is restricted, we cannot define a
             * font to support low ASCII characters in an SVG file. Use the
             * Unicode PUA at U+E000 instead.
             */
            textText.setNodeValue(Character.toString((char) (c + 0xe000)));

            GraphicsNode gn = builder.build(this.bridgeContext, this.document);
            gn.paint(g2d);
            /*
             * g2d.setColor(new Color(color.red,color.green,color.blue));
             * g2d.drawRect(0, 0, 13, 13);
             */

            image = target.initializeRaster(new IODataPreparer(bufferedImage,
                    String.format("wxsym%x", (int) c), 0), null);
            images.put(c, image);

            g2d.dispose();
        }
        return image;
    }
}
