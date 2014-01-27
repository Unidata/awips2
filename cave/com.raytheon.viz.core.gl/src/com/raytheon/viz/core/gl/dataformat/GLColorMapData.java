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

import com.raytheon.uf.common.colormap.image.ColorMapData;

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
 * Aug 10, 2011            bsteffen    Initial creation
 * Oct 16, 2013       2333 mschenke    Removed Buffer from this object
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class GLColorMapData {

    protected final AbstractGLColorMapDataFormat dataFormat;

    private final int[] dimensions;

    private final ColorMapData.ColorMapDataType dataType;

    public GLColorMapData(AbstractGLColorMapDataFormat dataFormat,
            ColorMapData.ColorMapDataType dataType, int[] dimensions) {
        this.dataFormat = dataFormat;
        this.dataType = dataType;
        this.dimensions = dimensions;
    }

    public AbstractGLColorMapDataFormat getDataFormat() {
        return dataFormat;
    }

    public int getTextureFormat() {
        return dataFormat.getTextureFormat();
    }

    public int getTextureInternalFormat() {
        return dataFormat.getTextureInternalFormat();
    }

    public int getTextureType() {
        return dataFormat.getTextureType();
    }

    public ColorMapData.ColorMapDataType getDataType() {
        return dataType;
    }

    public double getDataFormatMin() {
        return dataFormat.getDataFormatMin();
    }

    public double getDataFormatMax() {
        return dataFormat.getDataFormatMax();
    }

    public boolean isDataFormatScaled() {
        return dataFormat.isScaled();
    }

    public boolean isDataFormatSigned() {
        return dataFormat.isSignedFormat();
    }

    public int getDimensionSize(int index) {
        if (index < 0 || index >= dimensions.length) {
            return 0;
        }
        return dimensions[index];
    }

    public int getNumDimensions() {
        return dimensions.length;
    }

    public int[] getDimensions() {
        return dimensions;
    }

    public int getBytesPerPixel() {
        return dataFormat.getBytesPerPixel();
    }
}
