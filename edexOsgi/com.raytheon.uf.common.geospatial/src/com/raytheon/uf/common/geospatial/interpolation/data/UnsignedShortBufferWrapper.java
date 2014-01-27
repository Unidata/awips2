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
 * Unsigned ShortBuffer data wrapper
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jun 20, 2012            bsteffen     Initial creation
 * Nov 19, 2013  2393      bclement    changed to extend ShortBufferWrapper
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class UnsignedShortBufferWrapper extends ShortBufferWrapper {

    /**
     * @param geometry
     */
    public UnsignedShortBufferWrapper(GeneralGridGeometry geometry) {
        super(geometry);
    }

    /**
     * @param nx
     * @param ny
     */
    public UnsignedShortBufferWrapper(int nx, int ny) {
        super(nx, ny);
    }

    /**
     * @param buffer
     * @param geometry
     */
    public UnsignedShortBufferWrapper(ShortBuffer buffer,
            GeneralGridGeometry geometry) {
        super(buffer, geometry);
    }

    /**
     * @param buffer
     * @param nx
     * @param ny
     */
    public UnsignedShortBufferWrapper(ShortBuffer buffer, int nx, int ny) {
        super(buffer, nx, ny);
    }

    @Override
    protected double getDataValueInternal(int index) {
        return buffer.get(index) & 0xFFFF;
    }

}