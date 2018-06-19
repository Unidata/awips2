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
package com.raytheon.uf.common.dataplugin.pointset;

import java.nio.Buffer;
import java.nio.ByteBuffer;
import java.nio.FloatBuffer;
import java.nio.IntBuffer;
import java.nio.ShortBuffer;

import javax.measure.converter.AddConverter;
import javax.measure.converter.MultiplyConverter;
import javax.measure.converter.RationalConverter;
import javax.measure.converter.UnitConverter;

/**
 * 
 * Store the raw data for pointset. The primary structure is a {@link Buffer}
 * containing numeric data for each point. This also has support for products
 * that use scale and offset values in addition to the raw numeric data(This is
 * for packing the data into bytes or shorts).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------
 * Jan 20, 2016  5208     bsteffen  Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 */
public class PointSetData {

    private Buffer data;

    private Float scale;

    private Float offset;

    public PointSetData() {

    }

    public PointSetData(Buffer data) {
        this.data = data;
    }

    public Buffer getData() {
        return data;
    }

    public void setData(Buffer data) {
        this.data = data;
    }

    public Float getScale() {
        return scale;
    }

    public void setScale(Float scale) {
        this.scale = scale;
    }

    public Float getOffset() {
        return offset;
    }

    public void setOffset(Float offset) {
        this.offset = offset;
    }

    /**
     * Converts the data using the specified converter. If there is a scale or
     * offset present and the converter is a simple add or multiply then it is
     * incorporated into the scale/offset and the data is left untouched, which
     * keeps packed data small. For all other cases the data is converted to
     * floats and the converter is applied to the data.
     * 
     * @param converter
     */
    public void convert(UnitConverter converter) {
        if (data == null || converter == null
                || converter == UnitConverter.IDENTITY) {
            return;
        }
        if (scale != null
                && (converter instanceof MultiplyConverter || converter instanceof RationalConverter)) {
            this.scale = (float) converter.convert(this.scale);
            if (this.offset != null) {
                this.offset = (float) converter.convert(this.offset);
            }
        } else if (offset != null && converter instanceof AddConverter) {
            this.offset = (float) converter.convert(this.offset);
        } else {
            FloatBuffer buffer = null;
            if (this.data instanceof FloatBuffer) {
                buffer = (FloatBuffer) this.data;
            } else if (this.data instanceof ShortBuffer) {
                ShortBuffer sbuffer = (ShortBuffer) this.data;
                buffer = FloatBuffer.allocate(this.data.capacity());
                for (int i = 0; i < buffer.capacity(); i += 1) {
                    buffer.put(i, sbuffer.get(i));
                }
            } else if (this.data instanceof ByteBuffer) {
                ByteBuffer bbuffer = (ByteBuffer) this.data;
                buffer = FloatBuffer.allocate(this.data.capacity());
                for (int i = 0; i < buffer.capacity(); i += 1) {
                    buffer.put(i, bbuffer.get(i));
                }
            } else if (this.data instanceof IntBuffer) {
                IntBuffer ibuffer = (IntBuffer) this.data;
                buffer = FloatBuffer.allocate(this.data.capacity());
                for (int i = 0; i < buffer.capacity(); i += 1) {
                    buffer.put(i, ibuffer.get(i));
                }
            } else {
                throw new IllegalStateException("Unsupported buffer of type: "
                        + this.data.getClass().getSimpleName());
            }
            if (scale != null) {
                for (int i = 0; i < buffer.capacity(); i += 1) {
                    buffer.put(i, buffer.get(i) * scale);
                }
            }
            if (offset != null) {
                for (int i = 0; i < buffer.capacity(); i += 1) {
                    buffer.put(i, buffer.get(i) + offset);
                }
            }
            for (int i = 0; i < buffer.capacity(); i += 1) {
                buffer.put(i, (float) converter.convert(buffer.get(i)));
            }
            this.scale = null;
            this.offset = null;
            this.data = buffer;
        }
    }
}
