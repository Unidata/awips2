package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * 
 * Line interpolation function, is the top-level class.
 * 
 * X-value control points are increasing only and the length of ctrlX and ctrlY
 * are equal. Requires that x values are monotonically increasing.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 23, 2015  12301     jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class LineFunction {
    /**
     * The x values of line control points, monotonically increasing is required
     */
    protected double[] ctrlX;

    /** The y values of line control points, same length as ctrlX */
    protected double[] ctrlY;

    /**
     * Creates a new Spline.
     * 
     * @param xx
     *            -The x values of line control points
     * @param yy
     *            -The y values of line control points
     */
    public LineFunction(double[] xx, double[] yy) {
        setValues(xx, yy);
    }

    /**
     * Sets values for this line.
     * 
     * @param xx
     *            -The x values of line control points
     * @param yy
     *            -The y values of line control points
     */
    public void setValues(double[] xx, double[] yy) {
        this.ctrlX = xx;
        this.ctrlY = yy;

    }

    /**
     * Returns an interpolated value.
     * 
     * @param x
     *            - The x value
     * @param increaseOnly
     *            - if the function increases only
     * @return the interpolated value
     */
    public double at(double x, boolean increaseOnly) {
        if (ctrlX.length == 0) {
            return Double.NaN;
        }

        if (ctrlX.length == 1) {
            if (ctrlX[0] == x) {
                return ctrlY[0];
            } else {
                return Double.NaN;
            }
        }

        int k = 0;
        for (k = 1; k < this.ctrlX.length; k++) {
            if (x <= this.ctrlX[k]) {
                break;
            }
        }
        if (x < ctrlX[0]) {
            k = 1;
        } else if (x > ctrlX[ctrlX.length - 1]) {
            k = ctrlX.length - 1;
        }

        /** Need not care about increasing only correction since no error. */
        return ctrlY[k - 1] + (x - ctrlX[k - 1]) * (ctrlY[k] - ctrlY[k - 1])
                / (ctrlX[k] - ctrlX[k - 1]);

    }

    // TODO: Keep the case test code for development and DR fixing
    // public static void main(String[] args) {
    // double[] x = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0 };
    // double[] y = new double[] { .25, 0.0, .25, .5, 0.0 };
    // LineFunction f = new LineFunction(x, y);
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
