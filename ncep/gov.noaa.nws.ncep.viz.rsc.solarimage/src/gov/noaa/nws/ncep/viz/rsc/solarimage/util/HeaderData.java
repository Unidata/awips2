package gov.noaa.nws.ncep.viz.rsc.solarimage.util;

import nom.tam.fits.BasicHDU;
import nom.tam.fits.Header;

import com.raytheon.uf.viz.core.exception.VizException;

/**
 * Represents the image header data of a SolarImageRecord object.
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 02/22/2013   958        qzhou       Initial creation.
 * 04/02/2013   958        qzhou       Added isSdoHmi, isSdoHmi, isGoes, isNso, isSoho.
 * </pre>
 * 
 * @author qzhou
 * @version 1.0
 */
public class HeaderData {
    private Header header;

    private int nx;

    private int ny;

    private int dim;

    private double hgln;

    private double hglt;

    private double crln;

    private double crlt;

    // private double hcit, hcix, haex, haey, heex, heey, heqx, heqy,;
    private double dsun; // distance between the observer and sun image, ~D0

    private double carRot; //

    private double solarB0;

    private double solarL0;

    private double rsun; // fits rsun

    private double bitpix;

    private double bscale;

    private double bzero;

    private String telescope;

    /**
     * @param record
     */
    public HeaderData(BasicHDU hdu) throws VizException {

        try {
            // BasicHDU hdu = SolarImageUtil.getImageHDU(record);

            if (hdu != null) {
                int[] axes = hdu.getAxes();

                if (axes != null && axes.length != 2) {
                    // Expecting 2 dimensional image at this time
                    throw new VizException(
                            "The record does not contain a 2 dimensional image.");
                }

                this.header = hdu.getHeader();
                this.nx = axes[1];
                this.ny = axes[0];
                this.dim = header.getIntValue("NAXIS");
                this.hgln = header.getDoubleValue("HGLN_OBS", 0.0);
                this.hglt = header.getDoubleValue("HGLT_OBS", 0.0);
                this.crln = header.getDoubleValue("CRLN_OBS", 0.0);
                this.crlt = header.getDoubleValue("CRLT_OBS", 0.0);
                this.dsun = header.getDoubleValue("DSUN_OBS", 1392000000);
                this.carRot = header.getDoubleValue("CAR_ROT", 0.0);
                this.solarB0 = header.getDoubleValue("SOLAR-B0", 0.0);
                this.solarL0 = header.getDoubleValue("SOLAR-L0", 0.0);
                this.bitpix = hdu.getBitPix();
                this.bscale = hdu.getBScale();
                this.bzero = hdu.getBZero();
                this.telescope = header.getStringValue("TELESCOP");
            }

        } catch (Exception e) {
            throw new VizException(
                    "Error getting SolarImageData from SolarImageRecord.");
        }

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
     * @return the dim
     */
    public int getDim() {
        return dim;
    }

    /**
     * @param dim
     *            the dim to set
     */
    public void setDim(int dim) {
        this.dim = dim;
    }

    /**
     * @return the hgln
     */
    public double getHgln() {
        return hgln;
    }

    /**
     * @param hgln
     *            the hgln to set
     */
    public void setHgln(double hgln) {
        this.hgln = hgln;
    }

    /**
     * @return the hgln
     */
    public double getHglt() {
        return this.hglt;
    }

    /**
     * @param hglt
     *            the hglt to set
     */
    public void setHglt(double hglt) {
        this.hglt = hglt;
    }

    /**
     * @return the crln
     */
    public double getCrln() {
        return this.crln;
    }

    /**
     * @param crln
     *            the crln to set
     */
    public void setCrln(double crln) {
        this.crln = crln;
    }

    /**
     * @return the crlt
     */
    public double getCrlt() {
        return this.crlt;
    }

    /**
     * @param crlt
     *            the crlt to set
     */
    public void setCrlt(double crlt) {
        this.crlt = crlt;
    }

    /**
     * @return the dsun
     */
    public double getDsun() {
        return dsun;
    }

    /**
     * @param dsun
     *            the dsun to set
     */
    public void setDsun(double dsun) {
        this.dsun = dsun;
    }

    /**
     * @return the header
     */
    public double getSolarB0() {
        return solarB0;
    }

    /**
     * @return the header
     */
    public double getSolarL0() {
        return solarL0;
    }

    /**
     * @return the rsun
     */
    public double getRsun() {
        rsun = 0.0;
        if (telescope.startsWith("SDO"))
            rsun = header.getDoubleValue("RSUN_OBS");
        else if (telescope.startsWith("STEREO"))
            rsun = header.getDoubleValue("RSUN");
        else if (telescope.startsWith("SOHO")
                && header.getStringValue("INSTRUME").startsWith("EIT"))
            rsun = header.getDoubleValue("SOLAR_R")
                    * header.getDoubleValue("CDELT1");
        else if (telescope.startsWith("NSO"))
            rsun = header.getDoubleValue("RADIUS",
                    header.getDoubleValue("SOLAR-R"));
        else if (telescope.startsWith("GOES"))
            rsun = calculateRsun();

        return rsun;
    }

    /**
     * @param rsun
     *            the rsun to set??
     */
    public void setRsun(double rsun) {
        this.rsun = rsun;
    }

    /**
     * @return the rsun
     */
    public double getCarRot() {
        return carRot;
    }

    /**
     * @param rsun
     *            the rsun to set??
     */
    public void setCarRot(double carRot) {
        this.carRot = carRot;
    }

    /**
     * @return the rsun
     */
    public double getJulianDate() {
        String date = ""; // '2012/03/07T17:34:57.160'
        int y = 0, m = 0, d = 0, hh = 0, mm = 0, ss = 0;
        double jd = 0.0;
        date = header.getStringValue("DATE_OBS");
        if (date == null || date.equalsIgnoreCase(""))
            date = header.getStringValue("DATE-OBS");

        if (date != null && date.length() <= 10) // LASCO dateObs=Date +Time
            date = date + "T" + header.getStringValue("TIME-OBS");

        y = Integer.valueOf(date.substring(0, 4));
        m = Integer.valueOf(date.substring(5, 7));
        d = Integer.valueOf(date.substring(8, 10));
        hh = Integer.valueOf(date.substring(11, 13));
        mm = Integer.valueOf(date.substring(14, 16));
        ss = Integer.valueOf(date.substring(17, 19));
        double msec = hh * 3600 * 1000 + mm * 60 * 1000 + ss * 1000;

        jd = 367 * y - 7 * (y + (m + 9) / 12) / 4 - 3
                * ((y + (m - 9) / 7) / 100 + 1) / 4 + 275 * m / 9 + d + 1721029
                - 0.5 + msec / 86400000;
        // System.out.println("****jd  " +jd +" "+m +" "+d +" "+hh
        // +" "+mm+" "+ss);

        return jd;
    }

    public double calculateRsun() {
        double jd = 0.0, t = 0.0;
        jd = getJulianDate();

        // Julian Centuries from 1900.0:
        t = (jd - 2415020) / 36525;

        // Carrington Rotation Number:
        double carr = (1. / 27.2753) * (jd - 2398167.0) + 1.0;
        // OLD = 349.03 - (360.* carr / 27.2753),
        // Fit#1 = f + X/g + a*SIN(2*π*X/e) + b*SIN(4*π*X/e) + h*SIN(6*π*X/e)
        // + c*COS(2*π*X/e) + d*COS(4*π*X/e) + i*COS(6*π*X/e)

        // Geometric Mean Longitude (deg):
        double mnl = 279.69668 + 36000.76892 * t + 0.0003025 * t * t;
        mnl = mnl % 360;

        // Mean anomaly (deg):
        double mna = 358.47583 + 35999.04975 * t - 0.000150 * t * t - 0.0000033
                * t * t * t;
        mna = mna % 360;

        // Eccentricity of orbit:
        double e = 0.01675104 - 0.0000418 * t - 0.000000126 * t * t;

        // Sun's equation of center (deg):
        double c = (1.919460 - 0.004789 * t - 0.000014 * t * t)
                * Math.sin(Math.toRadians(mna)) + (0.020094 - 0.000100 * t)
                * Math.sin(Math.toRadians(2 * mna)) + 0.000293
                * Math.sin(Math.toRadians(3 * mna));

        // Sun's true geometric longitude (deg)
        // (Refered to the mean equinox of date. Question: Should the higher
        // accuracy terms from which app_long is derived be added to true_long?)
        // true_long = (mnl + c) mod 360d0

        // Sun's true anomaly (deg):
        double ta = (mna + c) % 360;

        // Sun's radius vector (AU). There are a set of higher accuracy
        // terms not included here. The values calculated here agree with the
        // example in the book:
        double dist = 1.0000002 * (1.0 - e * e)
                / (1.0 + e * Math.cos(Math.toRadians(ta)));

        // Semidiameter (arc sec):
        double sd = 959.63 / dist;
        // System.out.println("****sd  " +sd );

        return sd;
    }

    public double[] getL0B0() {
        double L0 = 0.0;
        double jd = getJulianDate();

        double radian = 180 / Math.PI;
        double theta = (jd - 2398220) * 360 / 25.38;
        double inc = 7.25 / radian;
        double k = (73.6667 + 1.3958333 * (jd - 2396758) / 36525) / radian;

        double t = (jd - 2451545) / 36525;
        L0 = 280.46645 + 36000.76983 * t + 0.0003032 * t * t;

        double M = 357.52910 + 35999.05030 * t - 0.0001559 * t * t - 0.00000048
                * t * t * t;
        double Mr = M / radian;
        double C = (1.914600 - 0.004817 * t - 0.000014 * t * t) * Math.sin(Mr)
                + (0.019993 - 0.000101 * t) * Math.sin(2 * Mr) + 0.000290
                * Math.sin(3 * Mr);
        double sunL = L0 + C;

        double omega = 125.04 - 1934.136 * t;
        double lngtd = sunL - 0.00569 - 0.00478 * Math.sin(omega / radian);
        double lngtdr = lngtd / radian;
        double diffk = (lngtdr - k);

        double etay = -Math.sin(diffk) * Math.cos(inc);
        double etax = -Math.cos(diffk);
        double eta = (Math.atan2(etay, etax)) * radian;

        double B0r = Math.asin(Math.sin(diffk) * Math.sin(inc)); // central
                                                                 // latitude
        double B0 = B0r * radian;

        L0 = eta - theta;
        L0 = truncate(L0);

        double[] L0B0 = new double[2];
        L0B0[0] = L0;
        L0B0[1] = B0;
        return L0B0;

    }

    public double getL0() {
        double L0 = 0;
        if (!isStereo()) {
            L0 = getL0B0()[0];
        } else {
            L0 = crln - hgln;
        }
        return L0;
    }

    public double getB0() {
        double B0 = 0;

        B0 = getL0B0()[1];

        return B0;
    }

    public double truncate(double angle) {
        double n = Math.floor(angle / 360);
        double tangle = angle - n * 360;
        return tangle;
    }

    public boolean isStereo() {

        if (header.getStringValue("TELESCOP").startsWith("STEREO"))
            return true;

        return false;
    }

    public boolean isSdoAia() {

        if (header.getStringValue("TELESCOP").startsWith("SDO/AIA"))
            return true;

        return false;
    }

    public boolean isSdoHmi() {

        if (header.getStringValue("TELESCOP").startsWith("SDO/HMI"))
            return true;

        return false;
    }

    public boolean isGoes() {

        if (header.getStringValue("TELESCOP").startsWith("SXI"))
            return true;

        return false;
    }

    public boolean isNso() {

        if (header.getStringValue("TELESCOP").startsWith("NSO"))
            return true;

        return false;
    }

    public boolean isSoho() {

        if (header.getStringValue("TELESCOP").startsWith("SOHO"))
            return true;

        return false;
    }

    /**
     * @return the header
     */
    public Header getHeader() {
        return header;
    }

    /**
     * @param header
     *            the header to set
     */
    public void setHeader(Header header) {
        this.header = header;
    }

    public double getBitpix() {
        return bitpix;
    }

    public void setBitpix(double bitpix) {
        this.bitpix = bitpix;
    }

    public double getBscale() {
        return bscale;
    }

    public void setBscale(double bscale) {
        this.bscale = bscale;
    }

    public double getBzero() {
        return bzero;
    }

    public void setBzero(double bzero) {
        this.bzero = bzero;
    }

}
