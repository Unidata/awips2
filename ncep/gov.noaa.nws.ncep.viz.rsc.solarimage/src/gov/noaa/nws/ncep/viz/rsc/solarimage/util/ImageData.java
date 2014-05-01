package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Represents the image data of a SolarImageRecord object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer          Description
 * ------------ ---------- --------------    --------------------------
 * 02/07/2013   958        sgurung, qzhou    Initial creation.
 * 11/12/2013   958        qzhou, sgurung    removed Reverse order of image, removed imageArrays, hdu, header.
 * 
 * </pre>
 * 
 * @author sgurung, qzhou
 * @version 1.0
 */
public class ImageData {

    private int nx;

    private int ny;

    private float[] imageValues;

    /**
     * @param record
     */
    public ImageData(int nx, int ny, double bitpix, double bscale,
            double bzero, Object imgData) throws VizException {

        try {
            // BasicHDU hdu = SolarImageUtil.getImageHDU(record);

            if (imgData != null) {
                this.nx = nx;
                this.ny = ny;
                // this.header = hdu.getHeader();
                // Object imgData = hdu.getKernel();

                float[][] array = null;
                if (imgData instanceof float[][]) {
                    array = (float[][]) imgData;
                } else if (imgData instanceof int[][]) {
                    array = new float[ny][nx];
                    int[][] tmp = (int[][]) imgData;
                    for (int i = 0; i < ny; i++) {
                        for (int j = 0; j < nx; j++) {
                            array[i][j] = (float) (bscale * tmp[i][j] + bzero);
                        }
                    }

                } else if (imgData instanceof short[][]) {
                    array = new float[ny][nx];
                    short[][] tmp = (short[][]) imgData;
                    for (int i = 0; i < ny; i++) {
                        for (int j = 0; j < nx; j++) {
                            array[i][j] = (float) (bscale * tmp[i][j] + bzero);
                        }
                    }
                } else if (imgData instanceof byte[][]) {
                    array = new float[ny][nx];
                    byte[][] tmp = (byte[][]) imgData;
                    for (int i = 0; i < ny; i++) {
                        for (int j = 0; j < nx; j++) {
                            array[i][j] = (float) (bscale * tmp[i][j] + bzero);
                        }
                    }
                } else {
                    String msg = "SolarImageData: Unrecognized imgDatatype: "
                            + imgData.getClass().getCanonicalName();
                    throw new VizException(msg);
                }

                int n = 0;
                float[] imgDataRec = new float[nx * ny];
                // for (int i = ny - 1; i >= 0; i--) { // Reverse order of rows
                for (int i = 0; i < ny; i++) {
                    for (int j = 0; j < nx; j++) {
                        imgDataRec[n++] = array[i][j];
                    }
                }

                this.imageValues = imgDataRec;
                // this.imageArrays = array;
            }
        } catch (Exception e) {
            throw new VizException(
                    "Error getting SolarImageData from SolarImageRecord.");
        }

    }

    public ImageData() {

    }

    /**
     * @return the nx
     */
    public int getNx() {
        return nx;
    }

    /**
     * @param nx
     *            the nx to set
     */
    public void setNx(int nx) {
        this.nx = nx;
    }

    /**
     * @return the ny
     */
    public int getNy() {
        return ny;
    }

    /**
     * @param ny
     *            the ny to set
     */
    public void setNy(int ny) {
        this.ny = ny;
    }

    /**
     * @return the imageValues
     */
    public float[] getImageValues() {
        return imageValues;
    }

    /**
     * @param imageValues
     *            the imageValues to set
     */
    public void setImageValues(float[] imageValues) {
        this.imageValues = imageValues;
    }

}
