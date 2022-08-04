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
package com.raytheon.uf.common.mpe.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;

import com.raytheon.uf.common.mpe.constants.DPAConstants;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;

/**
 * Read the DAA file.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * ?            ?          ?           Initial creation
 * Sep 16, 2016 5631       bkowal      Cleanup. Eliminated duplicated
 *                                     DPA Constants.
 * 
 * </pre>
 * 
 */

public class DAAFile {

    private static final IUFStatusHandler handler = UFStatus
            .getHandler(DAAFile.class);

    private static final double radarZero = Math.pow(10.0, -98.0 / 10.0);

    /**
     * Raw radar field.
     */
    private double[][] stage1i;

    /**
     * Unbiased radar field (= raw radar * bias)
     */
    private double[][] stage1u;

    /**
     * Zero data for mask.
     */
    private double[][] zeroData;

    /** The DAA file. */
    private File file;

    /** The Bias Value */
    private double biasValue;

    /**
     * Constructor.
     * 
     * @param fileName
     *            filename as String
     */
    public DAAFile(String fileName) {
        this(new File(fileName));
    }

    /**
     * Constructor.
     * 
     * @param file
     *            Filename as file
     */
    public DAAFile(File file) {
        this.file = file;
    }

    /**
     * Load the data.
     * 
     * @throws Exception
     * 
     * @throws IOException
     */
    public void load() throws DAAException {
        if (file == null) {
            throw new DAAException("No DAA radar file has been specified.");
        }

        FileInputStream fis;
        try {
            fis = new FileInputStream(file);
            FileChannel fc = fis.getChannel();
            ByteBuffer byteBuf = fc.map(MapMode.READ_ONLY, 0, fis.available());

            readData(byteBuf);

            fis.close();
        } catch (FileNotFoundException e) {
            throw new DAAException("Unable to find DAA radar file: "
                    + file.getAbsolutePath() + ".", e);
        } catch (IOException e) {
            throw new DAAException("Failed to read DAA radar file: "
                    + file.getAbsolutePath() + ".", e);
        }
    }

    /**
     * Read the data out of the byte buffer.
     * 
     * stage1i array contains raw radar field stage1u array contains unbiased
     * radar field (= raw radar * bias) both arrays are read in as mm and
     * multiplied by 100 to make them look like RMOSAIC
     * 
     * @param byteBuf
     * @throws IOException
     */
    private void readData(ByteBuffer byteBuf) throws IOException {
        stage1i = new double[131][131];
        stage1u = new double[131][131];
        zeroData = new double[131][131];

        byteBuf.order(ByteOrder.LITTLE_ENDIAN);
        byteBuf.rewind();

        for (int i = DPAConstants.NUM_DPA_ROWS - 1; i >= 0; i--) {
            for (int j = 0; j < DPAConstants.NUM_DPA_COLS; j++) {
                float f = byteBuf.getFloat();
                if (f > -99.) {
                    if (f == -98) {
                        stage1i[i][j] = radarZero * 100;
                        stage1u[i][j] = radarZero * 100 * biasValue;
                    } else {
                        stage1i[i][j] = Math.pow(10, f / 10) * 100;
                        stage1u[i][j] = Math.pow(10, f / 10) * 100 * biasValue;
                    }
                    zeroData[i][j] = radarZero * 100;
                } else {
                    // Set to missing for colorbar
                    stage1i[i][j] = -9999;
                    stage1u[i][j] = -9999;
                    zeroData[i][j] = -9999;
                }
            }
        }
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }

    /**
     * Get the raw radar field.
     * 
     * @return
     */
    public double[][] getStage1i() {
        handler.debug("Getting Stage1i Data...");
        return stage1i;
    }

    /**
     * Get the unbiased radar field (= raw radar * bias)
     * 
     * @return
     */
    public double[][] getStage1u() {
        handler.debug("Getting Stage1u Data...");
        return stage1u;
    }

    /**
     * Get an array of zero data to use as a mask.
     * 
     * @return
     */
    public double[][] getZeroData() {
        return zeroData;
    }

    /**
     * Set the bias value.
     * 
     * @param biasValue
     *            The bias value to set
     */
    public void setBiasValue(double biasValue) {
        handler.debug("Bias Value set to " + biasValue);
        this.biasValue = biasValue;
    }
}