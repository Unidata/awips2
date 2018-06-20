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
package com.raytheon.uf.common.mpe.fieldgen;

import java.awt.Rectangle;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Used to return requested mpe precipitation data as well as the associated
 * hrap extents.
 * 
 * <pre>
 *
 * SOFTWARE HISTORY
 *
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2017 6407       bkowal      Initial creation
 *
 * </pre>
 *
 * @author bkowal
 */
@DynamicSerialize
public class PrecipDataResponse {

    @DynamicSerializeElement
    private int hrapX;

    @DynamicSerializeElement
    private int hrapY;

    @DynamicSerializeElement
    private int hrapWidth;

    @DynamicSerializeElement
    private int hrapHeight;

    @DynamicSerializeElement
    private short[] data;

    public PrecipDataResponse() {
    }

    public PrecipDataResponse(int hrapX, int hrapY, int hrapWidth,
            int hrapHeight, short[] data) {
        this.hrapX = hrapX;
        this.hrapY = hrapY;
        this.hrapWidth = hrapWidth;
        this.hrapHeight = hrapHeight;
        this.data = data;
    }

    public Rectangle getHrapExtent() {
        return new Rectangle(hrapX, hrapY, hrapWidth, hrapHeight);
    }

    public int getHrapX() {
        return hrapX;
    }

    public void setHrapX(int hrapX) {
        this.hrapX = hrapX;
    }

    public int getHrapY() {
        return hrapY;
    }

    public void setHrapY(int hrapY) {
        this.hrapY = hrapY;
    }

    public int getHrapWidth() {
        return hrapWidth;
    }

    public void setHrapWidth(int hrapWidth) {
        this.hrapWidth = hrapWidth;
    }

    public int getHrapHeight() {
        return hrapHeight;
    }

    public void setHrapHeight(int hrapHeight) {
        this.hrapHeight = hrapHeight;
    }

    public short[] getData() {
        return data;
    }

    public void setData(short[] data) {
        this.data = data;
    }
}