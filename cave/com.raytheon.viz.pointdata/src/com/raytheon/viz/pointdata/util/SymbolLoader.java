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
package com.raytheon.viz.pointdata.util;

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
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Loads symbols from svg into IImage
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2009 3099       bsteffen     Initial creation
 * Oct 20, 2010 6853       bgonzale     Migrated common symbol loading code.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SymbolLoader {

    private IGraphicsTarget target;

    private RGB color;

    private HashMap<Character, IImage> images = new HashMap<Character, IImage>();

    private Document document;

    private Element textNode;

    private Text textText;

    private final GVTBuilder builder;

    private final BridgeContext bridgeContext;

    public SymbolLoader() throws VizException {
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
        try {
            document = f.createDocument(PathManagerFactory
                    .getPathManager()
                    .getStaticFile(
                            "plotModels" + File.separator + "WxSymbolText.svg")
                    .toURI().toString());
        } catch (MalformedURLException e) {
            throw new VizException(
                    "Error loading symbol file: WxSymbolText.svg", e);
        } catch (IOException e) {
            throw new VizException(
                    "Error loading symbol file: WxSymbolText.svg", e);
        }
        textNode = document.getElementById("theText");
        textText = (Text) textNode.getFirstChild();

        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        this.bridgeContext = new BridgeContext(userAgentAdapter);
        this.builder = new GVTBuilder();
    }

    public IImage getImage(IGraphicsTarget target, RGB color, char c) {

        if (!target.equals(this.target) || !color.equals(this.color)) {
            images.clear();
            this.target = target;
            this.color = color;
        }

        if (!images.containsKey(c)) {

            byte[] red = { 0, (byte) color.red };
            byte[] green = { 0, (byte) color.green };
            byte[] blue = { 0, (byte) color.blue };
            IndexColorModel colorModel = new IndexColorModel(8, 2, red, green,
                    blue, 0);
            BufferedImage bImage = new BufferedImage(12, 12,
                    BufferedImage.TYPE_BYTE_INDEXED, colorModel);
            Graphics2D g2d = bImage.createGraphics();

            textText.setNodeValue(Integer.toString((int) (c)));

            GraphicsNode gn = builder.build(this.bridgeContext, this.document);
            gn.paint(g2d);

            IImage iImage = target.initializeRaster(new IODataPreparer(bImage,
                    String.format("wxsym%x", (int) c), 0), null);

            images.put(c, iImage);

            g2d.dispose();
        }
        return images.get(c);
    }
}
