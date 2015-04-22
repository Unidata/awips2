package gov.noaa.nws.ncep.viz.rsc.solarimage.wcs;

/**
 * Provides methods for Cylindrical transformation.
 * 
 * <pre>
 * 
 *  SOFTWARE HISTORY
 * 
 *  Date         Ticket#    Engineer    Description
 *  ------------ ---------- ----------  --------------------------
 *                             
 *  04/04/2013   #958       q.zhou      Initial creation
 * 
 * 
 * </pre>
 * 
 * @author q.zhou
 * @version 2
 */
public class CylindricalConverter {
    // Convert pixel to real world coordinates

    /**
     * @param
     */
    public CylindricalConverter() {

    }

    public double[] imageToWorld(double[] image) {
        // Convert pixel 0-360 to real world coordinates
        double[] cs = new double[2];

        if (cs[0] >= 0 && cs[0] <= 360)
            cs[0] = image[0] - 180;
        else
            System.out.println("x is out of 0-360");

        if (cs[1] >= 0 && cs[1] <= 180)
            cs[1] = image[1] - 90;
        else
            System.out.println("y is out of 0-180");

        return cs;
    }

    public double[] WorldToImage(double[] cs) {
        double[] image = new double[2];

        if (cs[0] >= -180 && cs[0] <= 180)
            image[0] = cs[0] + 180;

        if (cs[1] >= -90 && cs[1] <= 90)
            image[1] = cs[1] + 90;

        return image;
    }

}
