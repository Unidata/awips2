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
package com.raytheon.uf.viz.core.data.resp;

import java.awt.Rectangle;
import java.nio.Buffer;

import com.raytheon.uf.viz.core.data.IColorMapDataRetrievalCallback;
import com.raytheon.uf.viz.core.data.IColormappedDataPreparer;

/**
 * 
 * DEPRECATED: Do not use {@link IColormappedDataPreparer}, use
 * {@link IColorMapDataRetrievalCallback} instead
 * 
 * return type for {@link IColormappedDataPreparer}
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jul 27, 2009            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 */
@Deprecated
public class NumericImageData extends ImageData {

    private Buffer data;

    private int totalSize;

    private Rectangle datasetBounds;

    public NumericImageData(Buffer data, Rectangle datasetBounds, int totalSize) {
        this.data = data;
        this.totalSize = totalSize;
        this.datasetBounds = datasetBounds;
    }

    public Buffer getData() {
        return this.data;
    }

    public int getTotalSize() {
        return this.totalSize;
    }

    public void setData(Buffer data) {
        this.data = data;
    }

    public void setTotalSize(int totalSize) {
        this.totalSize = totalSize;
    }

    public Rectangle getDatasetBounds() {
        return datasetBounds;
    }

    public void setDatasetBounds(Rectangle datasetBounds) {
        this.datasetBounds = datasetBounds;
    }

}
