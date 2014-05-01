package gov.noaa.nws.ncep.viz.rsc.solarimage.wcs;

import gov.noaa.nws.ncep.viz.rsc.solarimage.util.HeaderData;

/**
 * Provides utility methods for Coordinate conversions.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer        Description
 * ------------ ---------- --------------- --------------------------
 * 02/21/2013   958        qzhou, sgurung   Initial creation.
 * 
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */

public class CSConversions {
    private int dim = 0;

    private double rsun;

    private double dsun;

    private double hgln;

    private double hglt;

    private double crln;

    private double B0;

    private double L0;

    private boolean isStereo = false;

    public CSConversions(HeaderData headerData) {

        dim = headerData.getDim();

        rsun = headerData.getRsun();
        dsun = headerData.getDsun();

        hglt = headerData.getHglt();
        crln = headerData.getCrln();
        hgln = headerData.getHgln();

        B0 = headerData.getB0(); // -headerData.getL0B0()[1]. tested aia euvi
        L0 = headerData.getL0();

        isStereo = headerData.isStereo();

    }

    public double[] heliocentricToHeliographic(double[] cs, boolean isCarrington) {

        /*
         * Equations used: r2= x2 + y 2 + z2 , Θ = sin−1 ((y cos B0 + z sin B0
         * )/r), Φ = Φ0 + arg(z cos B0 − y sin B0 , x), Where arg( x, y) = tan−1
         * (y/ x) .
         */
        double[] helio = new double[dim];
        double r = this.rsun;

        double z = (double) Math.sqrt(r * r - cs[0] * cs[0] - cs[1] * cs[1]);
        double b0 = this.hglt;
        double a0 = this.hgln;
        double L0 = 0.0;

        if (!isStereo) {
            b0 = this.B0;
        }

        if (isCarrington) {
            L0 = this.L0;
        }

        double temp = (double) ((cs[1] * Math.cos(Math.toRadians(b0)) + z
                * Math.sin(Math.toRadians(b0))) / r); // lat
        helio[1] = (double) Math.asin(temp);
        helio[1] = Math.toDegrees(helio[1]);

        temp = (double) ((cs[0]) / (z * Math.cos(Math.toRadians(b0)) - cs[1]
                * Math.sin(Math.toRadians(b0))));
        helio[0] = (double) (Math.toRadians(a0) + Math.atan(temp));
        helio[0] = Math.toDegrees(helio[0]) + L0;

        if (helio[0] < -180)
            helio[0] = helio[0] + 360;
        if (helio[0] > 180)
            helio[0] = helio[0] - 360;
        if (helio[0] < 0)
            helio[0] = helio[0] + 360;

        return helio;
    }

    public double[] heliographicToHeliocentric(double[] cs, boolean isCarrington) {

        /*
         * Equations used: centric[0] = r cos Θ sin(Φ − Φ0 ), centric[1] = r[sin
         * Θ cos B0 − cos Θ cos(Φ − Φ0 ) sin B0 ], z = r[sin Θ sin B0 + cos Θ
         * cos(Φ − Φ0 ) cos B0 ],
         */
        double[] centric = new double[dim];
        double r = this.rsun;
        double b0 = this.hglt;
        double a0 = this.hgln;
        double L0 = 0.0;

        if (!isStereo) {
            b0 = this.B0; // -headerData.getL0B0()[1]. tested aia euvi
        }

        if (isCarrington) {
            L0 = this.L0;
            cs[0] = cs[0] - L0; // change to stony
        }

        centric[0] = r * Math.cos(Math.toRadians(cs[1]))
                * Math.sin(Math.toRadians(cs[0] - a0));
        centric[1] = r
                * (Math.sin(Math.toRadians(cs[1]))
                        * Math.cos(Math.toRadians(b0)) - Math.cos(Math
                        .toRadians(cs[1]))
                        * Math.cos(Math.toRadians(cs[0] - a0))
                        * Math.sin(Math.toRadians(b0)));

        return centric;
    }

    public double[] helioprojectiveToHeliocentric(double[] cs) {
        /*
         * Equations used: z = D − d cos θy cos θ x , x = D ( PI/180) θ x,
         * (approximate) y = D ( PI/180) θ y.
         */
        double[] centric = new double[dim];
        double dsun = this.dsun;
        double rsun = this.rsun;

        double q = dsun * Math.cos(Math.toRadians(cs[1]))
                * Math.cos(Math.toRadians(cs[0]));
        double d = q * q - dsun * dsun + rsun * rsun;
        // if (d<0)
        // d = q - Math.sqrt(d);

        centric[0] = (double) (d * Math.cos(Math.toRadians(cs[1])) * Math
                .sin(Math.toRadians(cs[0])));// /3600
        centric[1] = (double) (d * Math.sin(Math.toRadians(cs[1])));

        return centric;
    }

    public double[] heliocentricToHelioprojective(double[] cs) {

        /*
         * Equations used: d= x2 + y2 + (D − z)2 , θ x = arg(D − z, x), θy =
         * sin−1 (y/d).
         */
        double[] projective = new double[dim];
        double d = this.dsun;
        double d0 = (double) Math.sqrt(d * d - cs[0] * cs[0] - cs[1] * cs[1]);
        projective[0] = Math.atan(cs[0] / d0);
        projective[1] = (double) Math.asin(cs[1] / d);
        projective[0] = Math.toDegrees(projective[0]);
        projective[1] = Math.toDegrees(projective[1]);

        return projective;
    }

    public double[] heliographicToCylindrical(double[] cs, boolean isCarrington) {

        /*
         * Equations used: x = (n-n0) *cos Φ, y = Φ;
         */
        double[] cylindrical = new double[dim];

        if (isCarrington) {

            cs[0] = cs[0]; // - L0;
        }

        // System.out.println(" heliographicToHeliocentric() ***  L0 = " +L0
        // +" crln = "+ crln+" b0 = "+ b0 + " B0 = " + headerData.getL0B0()[1]);
        cylindrical[0] = cs[0]; // *cos Φ;r* Math.cos(Math.toRadians( cs[1])) *
                                // Math.sin(Math.toRadians( cs[0]-a0));
        cylindrical[1] = cs[1]; // r* ( Math.sin(Math.toRadians( cs[1]))
                                // *Math.cos(Math.toRadians(b0)) -
                                // Math.cos(Math.toRadians(
                                // cs[1]))*Math.cos(Math.toRadians(cs[0]-a0))*Math.sin(Math.toRadians(b0)
                                // ));

        return cylindrical;
    }

    public double[] heliocentricToCylindrical(double[] cs) {

        /*
         * Equations used: x = (n-n0) *cos Φ, y = Φ;
         */
        double[] cylindrical = new double[dim];
        double r = 0.0;
        double theta = 0.0;

        r = Math.sqrt(cs[0] * cs[0] + cs[1] * cs[1]);
        theta = Math.atan2(cs[1], cs[0]) * 180.0 / Math.PI;

        cylindrical[0] = r; // *cos Φ;r* Math.cos(Math.toRadians( cs[1])) *
                            // Math.sin(Math.toRadians( cs[0]-a0));
        cylindrical[1] = theta; // r* ( Math.sin(Math.toRadians( cs[1]))
                                // *Math.cos(Math.toRadians(b0)) -
                                // Math.cos(Math.toRadians(
                                // cs[1]))*Math.cos(Math.toRadians(cs[0]-a0))*Math.sin(Math.toRadians(b0)
                                // ));

        return cylindrical;
    }

    public int getDim() {
        return dim;
    }

    public void setDim(int dim) {
        this.dim = dim;
    }

    public double getRsun() {
        return rsun;
    }

    public void setRsun(double rsun) {
        this.rsun = rsun;
    }

    public double getDsun() {
        return dsun;
    }

    public void setDsun(double dsun) {
        this.dsun = dsun;
    }

    public double getHgln() {
        return hgln;
    }

    public void setHgln(double hgln) {
        this.hgln = hgln;
    }

    public double getHglt() {
        return hglt;
    }

    public void setHglt(double hglt) {
        this.hglt = hglt;
    }

    public double getCrln() {
        return crln;
    }

    public void setCrln(double crln) {
        this.crln = crln;
    }

    public double getB0() {
        return B0;
    }

    public void setB0(double b0) {
        B0 = b0;
    }

    public double getL0() {
        return L0;
    }

    public void setL0(double l0) {
        L0 = l0;
    }

    public boolean isStereo() {
        return isStereo;
    }

    public void setStereo(boolean isStereo) {
        this.isStereo = isStereo;
    }
}
