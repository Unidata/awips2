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
package com.raytheon.uf.common.dataplugin.radar.level3.generic;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 19, 2009            askripsk     Initial creation
 * 
 * </pre>
 * 
 * @author askripsk
 * @version 1.0
 */

public class RadialComponent extends GenericDataComponent {

    private short[] data;

    private int numRadials;

    private int numBins;

    protected float[] angleData;

    @Override
    @SuppressWarnings("unused")
    public void parseData(DataInputStream in) throws IOException {
        // Read the description
        String description = GenericUtil.readInString(in);
        float bin_size = in.readFloat();
        float first_range = in.readFloat();
        int numof_comp_params = in.readInt();
        for (int i = 0; i < numof_comp_params; i++) {
            // In my sample data there are no comp parameters
            // TODO read comp params
        }
        numRadials = in.readInt();
        numRadials = in.readInt();
        angleData = new float[numRadials];
        data = null;
        for (int i = 0; i < numRadials; i++) {
            angleData[i] = in.readFloat();
            float elevation = in.readFloat();
            float width = in.readFloat();
            numBins = in.readInt();
            String attrs = GenericUtil.readInString(in);
            if (data == null) {
                data = new short[numRadials * numBins];
            }
            numBins = in.readInt();
            for (int j = 0; j < numBins; j++) {
                data[i * numBins + j] = (short) in.readInt();
            }
        }
    }

    /**
     * @return the data
     */
    public short[] getData() {
        return data;
    }

    /**
     * @param data
     *            the data to set
     */
    public void setData(short[] data) {
        this.data = data;
    }

    /**
     * @return the numof_radials
     */
    public int getNumRadials() {
        return numRadials;
    }

    /**
     * @return the n_bins
     */
    public int getNumBins() {
        return numBins;
    }

    /**
     * @return the angleData
     */
    public float[] getAngleData() {
        return angleData;
    }

}
