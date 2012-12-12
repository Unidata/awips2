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

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapDataType;

/**
 * 
 * GL Colormap data, includes a data buffer, data format and bounds
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2011            bsteffen     Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLColorMapData {

    private final AbstractGLColorMapDataFormat dataFormat;

    private Buffer data;

    private int[] dimensions;

    private ColorMapDataType dataType;

    private int textureType;

    public GLColorMapData(ColorMapData cmData,
            AbstractGLColorMapDataFormat dataFormat) {
        this.dataFormat = dataFormat;
        this.dimensions = cmData.getDimensions();
        this.dataType = cmData.getDataType();
        this.textureType = dataFormat.getTextureType();
        this.data = dataFormat.formatForGL(cmData.getBuffer(), this);
    }

    public Buffer getData() {
        return data;
    }

    public void setData(Buffer data) {
        this.data = data;
    }

    public int getTextureFormat() {
        return dataFormat.getTextureFormat();
    }

    public int getTextureInternalFormat() {
        return dataFormat.getTextureInternalFormat();
    }

    public int getTextureType() {
        return textureType;
    }

    public void setTextureType(int textureType) {
        this.textureType = textureType;
    }

    public ColorMapDataType getDataType() {
        return dataType;
    }

    public int getCopyBackTextureType() {
        return dataFormat.getCopyBackTextureType();
    }

    public double getDataFormatMin() {
        return dataFormat.getDataFormatMin();
    }

    public double getDataFormatMax() {
        return dataFormat.getDataFormatMax();
    }

    public Buffer getCopybackBuffer() {
        return dataFormat.getCopybackBuffer(this);
    }

    public Number getValue(int x, int y) {
        return dataFormat.getValue(x, y, this);
    }

    public int getDimensionSize(int index) {
        if (index < 0 || index >= dimensions.length) {
            return 0;
        }
        return dimensions[index];
    }

    public int[] getDimensions() {
        return dimensions;
    }

    public int getBytesPerPixel() {
        return dataFormat.getBytesPerPixel();
    }
}
