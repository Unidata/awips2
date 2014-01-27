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
package com.raytheon.viz.core.gl.dataformat;

import java.nio.Buffer;

import javax.measure.unit.Unit;

import com.raytheon.uf.common.colormap.image.ColorMapData;

/**
 * GLColorMapData backed by a java Buffer
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Oct 16, 2013       2333 mschenke    Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLBufferColorMapData extends GLColorMapData {

    private Buffer data;

    private int textureType;

    private Unit<?> dataUnit;

    public GLBufferColorMapData(ColorMapData cmData,
            AbstractGLColorMapDataFormat dataFormat) {
        super(dataFormat, cmData.getDataType(), cmData.getDimensions());
        this.textureType = dataFormat.getTextureType();
        this.data = dataFormat.formatForGL(cmData.getBuffer(), this);
        this.dataUnit = cmData.getDataUnit();
    }

    public Buffer getData() {
        return data;
    }

    public void setData(Buffer data) {
        this.data = data;
    }

    public Buffer getCopybackBuffer() {
        return dataFormat.getCopybackBuffer(this);
    }

    public int getCopyBackTextureType() {
        return dataFormat.getCopyBackTextureType();
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.viz.core.gl.dataformat.GLColorMapData#getTextureType()
     */
    @Override
    public int getTextureType() {
        return textureType;
    }

    public void setTextureType(int textureType) {
        this.textureType = textureType;
    }

    public Number getValue(int x, int y) {
        return dataFormat.getValue(x, y, this, data);
    }

    /**
     * @return
     */
    public Unit<?> getDataUnit() {
        return dataUnit;
    }

}
