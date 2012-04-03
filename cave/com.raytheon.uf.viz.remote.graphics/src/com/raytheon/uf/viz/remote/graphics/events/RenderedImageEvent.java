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
package com.raytheon.uf.viz.remote.graphics.events;

import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import javax.media.jai.remote.SerializableRenderedImage;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Event that sends a rendered image object for the object id which should be
 * treated as an image
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 29, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class RenderedImageEvent extends AbstractDispatchingObjectEvent {

    @DynamicSerializeElement
    private byte[] serializedRenderedImage;

    private RenderedImage renderedImage;

    /**
     * @return the renderedImage
     */
    public byte[] getSerializedRenderedImage() {
        return serializedRenderedImage;
    }

    /**
     * @param renderedImage
     *            the renderedImage to set
     */
    public void setSerializedRenderedImage(byte[] renderedImage) {
        this.serializedRenderedImage = renderedImage;
    }

    /**
     * @return the renderedImage
     */
    public RenderedImage getRenderedImage() {
        if (renderedImage == null && serializedRenderedImage != null) {
            // deserialize bytes into rendered image
            try {
                ObjectInputStream oin = new ObjectInputStream(
                        new ByteArrayInputStream(serializedRenderedImage));
                renderedImage = (RenderedImage) oin.readObject();
            } catch (Exception e) {
                throw new RuntimeException(
                        "Error deserializing rendered image: "
                                + e.getLocalizedMessage(), e);
            }
        }
        return renderedImage;
    }

    /**
     * @param renderedImage
     *            the renderedImage to set
     */
    public void setRenderedImage(RenderedImage renderedImage) {
        if (this.renderedImage != renderedImage) {
            if (renderedImage instanceof SerializableRenderedImage == false) {
                renderedImage = new SerializableRenderedImage(renderedImage);
            }
            this.renderedImage = renderedImage;
            // serialize rendered image into bytes
            try {
                ByteArrayOutputStream bytes = new ByteArrayOutputStream();
                ObjectOutputStream oos = new ObjectOutputStream(bytes);
                oos.writeObject(renderedImage);
                serializedRenderedImage = bytes.toByteArray();
            } catch (IOException e) {
                throw new RuntimeException("Error serializing rendered image",
                        e);
            }
        }
    }

}
