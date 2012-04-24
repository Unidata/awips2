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
package com.raytheon.uf.viz.remote.graphics.events.imagery;

import java.awt.image.RenderedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import javax.imageio.ImageIO;

import com.raytheon.uf.common.serialization.IDeserializationContext;
import com.raytheon.uf.common.serialization.ISerializationContext;
import com.raytheon.uf.common.serialization.ISerializationTypeAdapter;
import com.raytheon.uf.common.serialization.SerializationException;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeTypeAdapter;
import com.raytheon.uf.viz.remote.graphics.events.imagery.RenderedImageWrapper.RenderedImageSerializer;

/**
 * Wraps a RenderedImage for dynamic serialization
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Apr 13, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
@DynamicSerializeTypeAdapter(factory = RenderedImageSerializer.class)
public class RenderedImageWrapper {

    public static class RenderedImageSerializer implements
            ISerializationTypeAdapter<RenderedImageWrapper> {
        /*
         * (non-Javadoc)
         * 
         * @see
         * com.raytheon.uf.common.serialization.ISerializationTypeAdapter#serialize
         * (com.raytheon.uf.common.serialization.ISerializationContext,
         * java.lang.Object)
         */
        @Override
        public void serialize(ISerializationContext serializer,
                RenderedImageWrapper object) throws SerializationException {
            RenderedImage image = object.getWrappedImage();
            // serialize rendered image into bytes
            try {
                ByteArrayOutputStream bytes = new ByteArrayOutputStream();
                ImageIO.write(image, "png", bytes);
                serializer.writeBinary(bytes.toByteArray());
            } catch (IOException e) {
                throw new SerializationException(
                        "Error serializing rendered image", e);
            }
        }

        /*
         * (non-Javadoc)
         * 
         * @see com.raytheon.uf.common.serialization.ISerializationTypeAdapter#
         * deserialize
         * (com.raytheon.uf.common.serialization.IDeserializationContext)
         */
        @Override
        public RenderedImageWrapper deserialize(
                IDeserializationContext deserializer)
                throws SerializationException {
            RenderedImageWrapper wrapper = new RenderedImageWrapper();
            byte[] data = deserializer.readBinary();
            // deserialize bytes into rendered image
            try {
                wrapper.setWrappedImage(ImageIO.read(new ByteArrayInputStream(
                        data)));
            } catch (Exception e) {
                throw new SerializationException(
                        "Error deserializing rendered image: "
                                + e.getLocalizedMessage(), e);
            }
            return wrapper;
        }
    }

    @DynamicSerializeElement
    private RenderedImage wrappedImage;

    /**
     * @return the wrappedImage
     */
    public RenderedImage getWrappedImage() {
        return wrappedImage;
    }

    /**
     * @param wrappedImage
     *            the wrappedImage to set
     */
    public void setWrappedImage(RenderedImage wrappedImage) {
        this.wrappedImage = wrappedImage;
    }

}
