package com.raytheon.uf.common.mpe.util;


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.channels.FileChannel.MapMode;

/**
 * Read the DAA file.
 * 
 */

public class DAAFile {
    private static final int NUM_DPA_COLS = 131;

    private static final int NUM_DPA_ROWS = 131;

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
    public void load() throws Exception {
        FileInputStream fis;
        try {
            fis = new FileInputStream(file);
            FileChannel fc = fis.getChannel();
            ByteBuffer byteBuf = fc.map(MapMode.READ_ONLY, 0, fis.available());

            readData(byteBuf);

            fis.close();
        } catch (FileNotFoundException e) {
            System.out.println("DAA file not found " + e);
            // throw e;
        } catch (IOException e) {
            System.out.println("IO Error attempting to read DAA file " + e);
            // throw e;
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

        for (int i = NUM_DPA_ROWS - 1; i >= 0; i--) {
            for (int j = 0; j < NUM_DPA_COLS; j++) {
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
        System.out.println("Getting Stage1i Data...");
        return stage1i;
    }

    /**
     * Get the unbiased radar field (= raw radar * bias)
     * 
     * @return
     */
    public double[][] getStage1u() {
        System.out.println("Getting Stage1u Data...");
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
        System.out.println("Bias Value set to " + biasValue);
        this.biasValue = biasValue;
    }

}
