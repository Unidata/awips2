package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * 
 * Performs Cubic spline interpolation given a set of control points.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 14, 2016   12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class CubicFunction extends LineFunction {

    /* The matrix of the Cubic Function to convert an x to a y. */
    private double[] mm;

    /**
     * Creates a cubic spline from a given set of control points.
     * 
     * The spline is guaranteed to pass through each control point exactly.
     * 
     * This function uses the Fritsch-Carlson method for computing the spline
     * parameters. http://en.wikipedia.org/wiki/Monotone_cubic_interpolation
     * 
     * @param sourceX
     *            -The X component of the control points, strictly increasing,
     *            no same values.
     * @param sourceY
     *            -The Y component of the control points.
     * @return
     * 
     */
    public CubicFunction(double[] xx, double[] yy) {

        super(xx, yy);

        if (xx != null && yy != null && xx.length == yy.length && xx.length > 1) {

            final int n = xx.length;
            double[] d = new double[n - 1];
            double[] m = new double[n];

            /* Compute slopes of secant lines between successive points. */
            for (int i = 0; i < n - 1; i++) {
                double h = xx[i + 1] - xx[i];
                if (h == 0f) {
                    /** For readability only: x[i + 1] and x[i] are never equal */
                }
                d[i] = (yy[i + 1] - yy[i]) / h;
            }

            /* Initialize the tangents as the average of the secants. */
            m[0] = d[0];
            for (int i = 1; i < n - 1; i++) {
                m[i] = (d[i - 1] + d[i]) * 0.5f;
            }
            m[n - 1] = d[n - 2];

            /* Update the tangents */
            for (int i = 0; i < n - 1; i++) {
                if (d[i] == 0f) { // successive Y values are equal
                    m[i] = 0f;
                    m[i + 1] = 0f;
                } else {
                    double a = m[i] / d[i];
                    double b = m[i + 1] / d[i];

                    double h = Math.hypot(a, b);
                    if (h > 9f) {
                        double t = 3f / h;
                        m[i] = t * a * d[i];
                        m[i + 1] = t * b * d[i];
                    }
                }
            }

            this.mm = m;
        }
    }

    /**
     * Interpolates the value of Y = f(X) for a given X. Clamps X to the domain
     * of the spline.
     * 
     * @param x
     *            -The X value.
     * @param increasing
     *            - If the function is increasing only
     * @return The interpolated Y = f(X) value.
     */
    @Override
    public double at(double x, boolean increasing) {

        /** Handle the boundary cases. */
        final int n = ctrlX.length;
        if (Double.isNaN(x) || ctrlX == null || ctrlY == null) {
            return x;
        }
        if (ctrlX.length == 1) {
            return ctrlY[0];
        }
        if (x <= ctrlX[0]) {
            return ctrlY[0];
        }
        if (x >= ctrlX[n - 1]) {
            return ctrlY[n - 1];
        }

        /*
         * Finds the index 'i' of the last point with smaller X. We know this
         * will be within the spline due to the boundary tests.
         */

        int i = 0;
        while (x >= ctrlX[i + 1]) {
            i += 1;
            if (x == ctrlX[i]) {
                return ctrlY[i];
            }
        }

        /* Performs cubic Hermite spline interpolation. */
        double h = ctrlX[i + 1] - ctrlX[i];
        double t = (x - ctrlX[i]) / h;

        double yOut = (ctrlY[i] * (1 + 2 * t) + h * mm[i] * t) * (1 - t)
                * (1 - t)
                + (ctrlY[i + 1] * (3 - 2 * t) + h * mm[i + 1] * (t - 1)) * t
                * t;

        /* Correction and restriction */
        int k = 0;
        for (k = 1; k < ctrlX.length; k++) {
            if (x <= ctrlX[k]) {
                break;
            }
        }
        if (increasing && yOut > ctrlY[k]) {
            yOut = ctrlY[k];
        }
        return yOut;
    }

    // TODO: Keep the case test code for development and DR fixing
    // public static void main(String[] args) {
    // double[] x = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0 };
    // double[] y = new double[] { .25, 0.0, .25, .5, 0.0 };
    // CubicFunction f = new CubicFunction(x, y);
    // double xin = 1.5;
    // double yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 2.0;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 2.5;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 3.0;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 3.5;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 4.0;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 4.5;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // xin = 5.0;
    // yout = f.at(xin, false);
    // System.out.println("x=" + xin + ", y=" + yout);
    // }

}