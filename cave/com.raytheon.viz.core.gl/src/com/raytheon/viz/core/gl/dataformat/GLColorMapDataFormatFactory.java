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

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;

/**
 * Factory class for getting GLColorMapDataFormat objects given the ColorMapData
 * object
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Nov 21, 2011            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */

public class GLColorMapDataFormatFactory {

    public static AbstractGLColorMapDataFormat getGLColorMapDataFormat(
            ColorMapData colorMapData) {
        AbstractGLColorMapDataFormat dataFormat = null;
        switch (colorMapData.getDataType()) {
        case BYTE: {
            dataFormat = new GLByteDataFormat();
            break;
        }
        case SIGNED_BYTE: {
            dataFormat = new GLSignedByteDataFormat();
            break;
        }
        case UNSIGNED_SHORT: {
            dataFormat = new GLUnsignedShortDataFormat();
            break;
        }
        case SHORT: {
            dataFormat = new GLShortDataFormat();
            break;
        }
        case INT: {
            dataFormat = new GLIntDataFormat();
            break;
        }
        case FLOAT: {
            dataFormat = new GLFloatDataFormat();
        }
        }
        return dataFormat;
    }

}
