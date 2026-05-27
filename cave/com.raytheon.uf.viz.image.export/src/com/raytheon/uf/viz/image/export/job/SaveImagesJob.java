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
package com.raytheon.uf.viz.image.export.job;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageTypeSpecifier;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.metadata.IIOInvalidTreeException;
import javax.imageio.metadata.IIOMetadata;
import javax.imageio.metadata.IIOMetadataNode;
import javax.imageio.stream.FileImageOutputStream;

import org.eclipse.core.runtime.IProgressMonitor;
import org.w3c.dom.Node;

import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.image.export.options.ImageExportOptions;

/**
 * Save images using javax.imageio.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jul 07, 2015  4607     bsteffen    Extracted from ExportImageHandler
 * 
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1
 */
public class SaveImagesJob extends AbstractSaveImagesJob {

    private ImageWriter writer;

    public SaveImagesJob(ImageExportOptions options,
            LinkedHashMap<DataTime, BufferedImage> dtbiHash) {
        super(options, dtbiHash);
    }

    @Override
    protected boolean initialize() {
        String path = options.getFileLocation().getAbsolutePath();
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        Iterator<ImageWriter> iter = ImageIO.getImageWritersBySuffix(suffix);
        if (!iter.hasNext()) {
            String reason = "Unsupported filetype: \"" + suffix + "\"";
            statusHandler.handle(Priority.PROBLEM, reason);
            return false;
        }
        writer = iter.next();
        return true;
    }

    @Override
    protected void writeImage(File file, BufferedImage image)
            throws IOException {
        try (FileImageOutputStream stream = new FileImageOutputStream(file)) {
            writer.setOutput(stream);
            writer.write(image);
        }
    }

    @Override
    protected void writeAnimation(IProgressMonitor monitor) throws IOException {
        try (FileImageOutputStream stream = new FileImageOutputStream(
                options.getFileLocation())) {
            writer.setOutput(stream);
            writer.prepareWriteSequence(null);
            List<BufferedImage> images = new ArrayList<BufferedImage>();
            images.addAll(dtbiHash.values());
            dtbiHash.clear();
            for (int i = 0; i < images.size(); i++) {
                BufferedImage bi = images.get(i);
                /* Allow GC to collect image after write. */
                images.set(i, null);
                ImageWriteParam iwp = writer.getDefaultWriteParam();
                IIOMetadata metadata = writer.getDefaultImageMetadata(
                        new ImageTypeSpecifier(bi), iwp);
                if (i == 0) {
                    configureAnimation(metadata, options.getFirstFrameDwell(),
                            true);
                } else if (i == images.size() - 1) {
                    configureAnimation(metadata, options.getLastFrameDwell(),
                            false);
                } else {
                    configureAnimation(metadata, options.getFrameDelay(), false);
                }
                IIOImage ii = new IIOImage(bi, null, metadata);
                writer.writeToSequence(ii, null);
                monitor.worked(1);
                if (monitor.isCanceled()) {
                    break;
                }
            }
            writer.endWriteSequence();
        }
    }

    private void configureAnimation(IIOMetadata metadata, int delayMillis,
            boolean first) throws IIOInvalidTreeException {
        int delayCentis = delayMillis / 10;
        String metaFormat = metadata.getNativeMetadataFormatName();

        if (!"javax_imageio_gif_image_1.0".equals(metaFormat)) {
            throw new IllegalArgumentException(
                    "Unfamiliar gif metadata format: " + metaFormat);
        }

        Node root = metadata.getAsTree(metaFormat);

        /* find the GraphicControlExtension node */
        Node child = root.getFirstChild();
        while (child != null) {
            if ("GraphicControlExtension".equals(child.getNodeName())) {
                break;
            }
            child = child.getNextSibling();
        }

        IIOMetadataNode gce = (IIOMetadataNode) child;
        gce.setAttribute("userDelay", "FALSE");
        gce.setAttribute("delayTime", String.valueOf(delayCentis));

        /* only the first node needs the ApplicationExtensions node */
        if (first) {
            IIOMetadataNode aes = new IIOMetadataNode("ApplicationExtensions");
            IIOMetadataNode ae = new IIOMetadataNode("ApplicationExtension");
            ae.setAttribute("applicationID", "NETSCAPE");
            ae.setAttribute("authenticationCode", "2.0");
            /*
             * last two bytes is an unsigned short (little endian) that
             * indicates the the number of times to loop. 0 means loop forever.
             */
            byte[] uo = new byte[] { 0x1, 0x0, 0x0 };
            ae.setUserObject(uo);
            aes.appendChild(ae);
            root.appendChild(aes);
        }

        metadata.setFromTree(metaFormat, root);

    }

}