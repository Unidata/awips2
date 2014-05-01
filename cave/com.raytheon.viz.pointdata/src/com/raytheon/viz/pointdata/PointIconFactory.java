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
package com.raytheon.viz.pointdata;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.dom.util.SAXDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.eclipse.swt.graphics.RGB;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import com.raytheon.uf.common.localization.IPathManager;
import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.viz.core.exception.VizException;

/**
 * 
 * A class which can create a weather icon from it's numeric value using an svg.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 26, 2010            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class PointIconFactory {

    private RGB color;

    private int size;

    private Document document;

    private GVTBuilder builder;

    private BridgeContext bridgeContext;

    public PointIconFactory(RGB color, int size) throws VizException {
        this.color = color;
        this.size = size;
        String parser = XMLResourceDescriptor.getXMLParserClassName();
        SAXDocumentFactory f = new SAXSVGDocumentFactory(parser);

        try {
            document = f.createDocument(PathManagerFactory
                    .getPathManager()
                    .getStaticFile(
                            "plotModels" + IPathManager.SEPARATOR + "Icon.svg")
                    .toURI().toString());
        } catch (Exception e) {
            throw new VizException("Error loading symbol file: Icon.svg", e);
        }

        UserAgentAdapter userAgentAdapter = new UserAgentAdapter();
        bridgeContext = new BridgeContext(userAgentAdapter);
        builder = new GVTBuilder();
    }

    public BufferedImage getIcon(int i) {
        if (i < 0) {
            i = 0;
        }
        byte[] red = { 0, (byte) color.red };
        byte[] green = { 0, (byte) color.green };
        byte[] blue = { 0, (byte) color.blue };
        IndexColorModel colorModel = new IndexColorModel(8, 2, red, green,
                blue, 0);
        BufferedImage bImage = new BufferedImage(size, size,
                BufferedImage.TYPE_BYTE_INDEXED, colorModel);
        Graphics2D g2d = bImage.createGraphics();
        document.getElementsByTagName("svg").item(0).getAttributes()
                .getNamedItem("viewBox")
                .setNodeValue("0 0 " + size + " " + size);
        Element iconNode = document.getElementById("icon");
        iconNode.getFirstChild().setNodeValue(Integer.toString(i));
        iconNode.getAttributeNode("x").setNodeValue(String.valueOf(size / 2));
        iconNode.getAttributeNode("y").setNodeValue(String.valueOf(size / 2));
        GraphicsNode gn = builder.build(bridgeContext, document);
        gn.paint(g2d);

        g2d.dispose();
        return bImage;
    }

}
