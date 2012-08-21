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
package com.raytheon.uf.common.geospatial.interpolation.data;

import java.nio.ShortBuffer;

import org.geotools.coverage.grid.GeneralGridGeometry;

/**
 * 
 * ShortBuffer data wrapper
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class ShortBufferWrapper extends DataWrapper1D {

    protected final ShortBuffer buffer;

    public ShortBufferWrapper(ShortBuffer buffer, GeneralGridGeometry geometry) {
        super(geometry);
        this.buffer = buffer;
    }

    public ShortBufferWrapper(ShortBuffer buffer, int nx, int ny) {
        super(nx, ny);
        this.buffer = buffer;
    }

    public ShortBufferWrapper(int nx, int ny) {
        this(ShortBuffer.allocate(nx * ny), nx, ny);
    }

    public ShortBufferWrapper(GeneralGridGeometry geometry) {
        // assume this is going to be a destination and avoid passing
        // geometry to super to save time on checking for wrapping.
        this(geometry.getGridRange().getSpan(0), geometry.getGridRange()
                .getSpan(1));
    }

    public ShortBuffer getBuffer() {
        return buffer;
    }

    @Override
    protected double getDataValueInternal(int index) {
        return buffer.get(index);
    }

    @Override
    public void setDataValueInternal(double dataValue, int index) {
        buffer.put(index, (short) dataValue);
    }

}