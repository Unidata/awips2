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

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback.ColorMapData;

/**
 * Interface for providing an AbstractGLColorMapDataFormat object given
 * ColorMapData. If a GL project wants to use a custom format for an image, they
 * can wrap the {@link IColorMapDataRetrievalCallback} in an
 * {@link IColorMapDataRetrievalCallback} that also implements this interface
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

public interface IGLColorMapDataFormatProvider {

    /**
     * Default IGLColorMapDataFormatProvider, uses GLColorMapDataFormatFactory
     * to get format
     */
    public static final IGLColorMapDataFormatProvider defaultCallback = new IGLColorMapDataFormatProvider() {
        @Override
        public AbstractGLColorMapDataFormat getGLColorMapDataFormat(
                ColorMapData colorMapData) {
            return GLColorMapDataFormatFactory
                    .getGLColorMapDataFormat(colorMapData);
        }
    };

    public AbstractGLColorMapDataFormat getGLColorMapDataFormat(
            ColorMapData colorMapData);

}
