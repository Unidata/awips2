package gov.noaa.nws.ncep.viz.rsc.solarimage.wcs;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import nom.tam.fits.Header;
import nom.tam.fits.TruncatedFileException;
import nom.tam.util.BufferedDataInputStream;

/**
 * Provides methods for fits wcs transformation.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------  --------------------------
 *                             
 *  02/14/2013   #958       q.zhou      Convert original fits to wcs format.  
 *  									Convert pixel to real world coordinates
 * 11/17/2013    958        qzhou       Added fixes for no flip image: modified pc, pcInv, crota and crpix.
 * 01/07/2014    1046       qzhou       Fixed imagefunction rotation problem. Added constructor for cylindrical.
 *                                      Fixed imageToWorld and worldToImage reverse problem.
 *                                      Added WorldToImageSamp method for sampling
 * </pre>
 * 
 * @author q.zhou
 * @version 2
 */
public class WCSConverter {

    private int dim = 0;

    private double[] naxis;

    private double[] crpix;

    private double[] cdelt;

    private double[] crval;

    private double[] crota;

    private double[][] pc;

    private double[][] cd;

    private double[][] pcInv, cdInv;

    private boolean containsPC = false;

    private boolean containsCD = false;

    private int cylindrical;

    private boolean equalNavigation = false;

    /**
     * @param header
     */

    public WCSConverter(Header header, int cylindrical) {
        this.cylindrical = cylindrical;

        if (header.getStringValue("TELESCOP").startsWith("NSO-GONG")
                || header.getStringValue("TELESCOP").startsWith("SDO/AIA"))
            this.equalNavigation = true;

        init(header);
    }

    public WCSConverter(byte[] data) throws TruncatedFileException, IOException {
        ByteArrayInputStream bis = new ByteArrayInputStream(data);
        BufferedDataInputStream stream = new BufferedDataInputStream(bis);
        init(new Header(stream));
    }

    /*
     * Initialize and Convert original fits to wcs
     */
    private void init(Header header) {
        dim = header.getIntValue("NAXIS");
        naxis = new double[dim];
        crpix = new double[dim];
        cdelt = new double[dim];
        crval = new double[dim];
        crota = new double[dim];
        pc = new double[dim][dim];
        cd = new double[dim][dim];
        pcInv = new double[dim][dim];
        cdInv = new double[dim][dim];
        containsPC = header.containsKey("PC1_1");
        containsCD = header.containsKey("CD1_1");

        for (int n = 0; n < dim; n++) {
            String ij = Integer.toString(n + 1);
            crpix[n] = header.getDoubleValue("CRPIX" + ij, 0.0); // if null, 0.0
            cdelt[n] = header.getDoubleValue("CDELT" + ij, 1.0);
            crval[n] = header.getDoubleValue("CRVAL" + ij, 0.0);
            crota[n] = header.getDoubleValue("CROTA" + ij, 0.0);
            naxis[n] = header.getDoubleValue("NAXIS" + ij, 0.0);
        }

        for (int m = 0; m < dim; m++) {
            String i = Integer.toString(m + 1);
            for (int n = 0; n < dim; n++) {
                String j = Integer.toString(n + 1);
                pc[m][n] = header.getDoubleValue("PC" + i + "_" + j, 0.0);
                cd[m][n] = header.getDoubleValue("CD" + i + "_" + j, 0.0);
                pcInv[m][n] = pc[m][n];
                cdInv[m][n] = cd[m][n];
            }
        }
        // change pc for no flip method up
        pc[0][1] = -pc[0][1];
        pc[1][0] = -pc[1][0];

        // Convert original to pc. Do not convert original fits to cd.
        if (!header.containsKey("PC1_1") && !header.containsKey("CD1_1")) {
            double rota = 0.0;

            if (crota[1] != 0.0) // CROTA2
                rota = crota[1];
            else if (crota[0] != 0.0)
                rota = crota[0];
            // change pc for no flip method
            rota = -rota;

            if (dim == 2) {
                for (int i = 0; i < dim; i++) {
                    for (int j = 0; j < dim; j++) {
                        if (i == j) {
                            pc[i][j] = Math.cos(Math.toRadians(rota));
                        } else if (i < j) {
                            pc[i][j] = (-Math.sin(Math.toRadians(rota)))
                                    * cdelt[1] / cdelt[0];
                        } else if (i > j) {
                            pc[i][j] = (Math.sin(Math.toRadians(rota)))
                                    * cdelt[0] / cdelt[1];
                        }
                    }
                }
            }
        }

        // inverse
        if (dim == 2) {
            if (cylindrical == 0) {
                for (int i = 0; i < dim; i++) {
                    for (int j = 0; j < dim; j++) {
                        if (i == j) {
                            pcInv[i][j] = pc[i][j];
                            cdInv[i][j] = cd[i][j];
                        } else {
                            pcInv[i][j] = -pc[i][j];
                            cdInv[i][j] = -cd[i][j];
                        }
                    }
                }
            } else {
                pcInv = pc;
                cdInv = cd;
            }
        }
    }

    public double[] imageToWorld(double[] image) {

        /*
         * formula:
         * 
         * double x= crval[0] +cdelt[0] *(pc[0][0]*cs[0] + pc[0][1]*cs[1]);
         * double y= crval[1] +cdelt[1] *(pc[1][0]*cs[0] + pc[1][1]*cs[1]);
         */

        double[] cs = new double[dim];
        double[] tmp = new double[dim];

        /*
         * Changed for no flip reading data. Was: for (int j = 0; j < dim; j++)
         * cs[j] = image[j] - crpix[j];
         */
        cs[0] = image[0] - crpix[0];
        cs[1] = image[1] - (naxis[1] - crpix[1]);

        for (int i = 0; i < dim; i++) {
            tmp[i] = 0.0;
            for (int j = 0; j < dim; j++) {
                if (dim == 2 && containsPC)
                    tmp[i] += pc[i][j] * cs[j] * cdelt[i];
                else if (dim == 2 && containsCD)
                    tmp[i] += cd[i][j] * cs[j];
                else
                    tmp[i] += pc[i][j] * cs[j] * cdelt[i];
            }
        }

        for (int i = 0; i < dim; i++) {
            cs[i] = tmp[i];
            cs[i] += crval[i];
        }

        return cs;
    }

    public double[] WorldToImage(double[] cs) {
        double[] image = new double[dim];
        double[] tmp = new double[dim];

        for (int i = 0; i < dim; i++) {
            image[i] = (cs[i] - crval[i]);
        }

        for (int i = 0; i < dim; i++) {
            tmp[i] = 0.0;
            for (int j = 0; j < dim; j++) {
                if (dim == 2 && containsPC)
                    tmp[i] += pcInv[i][j] * image[j] / cdelt[i];
                else if (dim == 2 && containsCD)
                    tmp[i] += cdInv[i][j] * image[j];
                else
                    tmp[i] += pcInv[i][j] * image[j] / cdelt[i];
            }
        }

        if (cylindrical != 0) {

            for (int j = 0; j < dim; j++) {
                image[j] = tmp[j] + crpix[j];
            }
        } else {

            image[0] = tmp[0] + crpix[0];
            image[1] = tmp[1] + (naxis[1] - crpix[1]);
        }

        return image;
    }

    public double[] WorldToImageSamp(double[] cs) {
        double[] image = new double[dim];
        double[] tmp = new double[dim];

        for (int i = 0; i < dim; i++) {
            image[i] = (cs[i] - crval[i]);
        }

        for (int i = 0; i < dim; i++) {
            tmp[i] = 0.0;
            for (int j = 0; j < dim; j++) {
                if (dim == 2 && containsPC)
                    tmp[i] += pc[i][j] * image[j] / cdelt[i];
                else if (dim == 2 && containsCD)
                    tmp[i] += cd[i][j] * image[j];
                else
                    tmp[i] += pc[i][j] * image[j] / cdelt[i];
            }
        }

        for (int j = 0; j < dim; j++) {
            image[j] = tmp[j] + crpix[j];
        }

        return image;
    }

    public boolean hasEqualNavigation() {
        return equalNavigation;
    }
}
