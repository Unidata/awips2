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
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

/**
 * Radar Coverage File. Misbin files.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 10, 2009 2675       mpduff     Initial creation
 * 
 * </pre>
 * 
 * @author mpduff
 * @version 1.0
 */

public class RadarCoverageFile {
    private static final int NUM_DPA_COLS = 131;

    private static final int NUM_DPA_ROWS = 131;

    /**
     * Radar Coverage field.
     */
    private double[][] radCov;

    /** The Misbin file. */
    private File file;

    /** Date format */
    private static SimpleDateFormat sdf;
    static {
        sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        sdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    /**
     * Constructor.
     * 
     * @param fileName
     *            filename as String
     */
    public RadarCoverageFile(String fileName) {
        this(new File(fileName));
    }

    /**
     * Constructor.
     * 
     * @param file
     *            Filename as file
     */
    public RadarCoverageFile(File file) {
        this.file = file;
    }

    /**
     * Load the data.
     * 
     * @throws IOException
     */
    public void load() throws IOException {
        FileInputStream fis = new FileInputStream(file);
        FileChannel fc = fis.getChannel();
        ByteBuffer byteBuf = fc.map(MapMode.READ_ONLY, 0, fis.available());

        readData(byteBuf);

        fis.close();
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
        radCov = new double[131][131];

        byteBuf.order(ByteOrder.BIG_ENDIAN);
        byteBuf.rewind();

        for (int i = 0; i < NUM_DPA_ROWS; i++) {
            for (int j = 0; j < NUM_DPA_COLS; j++) {
                short f = byteBuf.getShort();
                radCov[i][j] = f;
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
     * Get the radar coverage field.
     * 
     * @return
     */
    public double[][] getRadCov() {
        return radCov;
    }

}
