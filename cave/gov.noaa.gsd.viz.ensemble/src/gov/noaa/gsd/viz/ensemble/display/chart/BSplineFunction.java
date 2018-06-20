package gov.noaa.gsd.viz.ensemble.display.chart;

import java.util.Arrays;

/**
 * 
 * Interpolates given values by B-Splines.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2015  12301        jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */

public class BSplineFunction extends LineFunction {
    /** Function coefficients to calculate the y values for a specified x */
    private double[] a;

    private double[] b;

    private double[] c;

    private double[] d;

    /**
     * Creates a new Spline.
     * 
     * @param xx
     *            - The source x values
     * @param yy
     *            - The source y values
     */
    public BSplineFunction(double[] xx, double[] yy) {
        super(xx, yy);
        setValues(xx, yy);
        if (xx.length > 1) {
            calculateCoefficients();
        }
    }

    /**
     * Returns an interpolated value.
     * 
     * @param x
     *            - The value
     * @return The interpolated value
     */
    @Override
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

        int index = Arrays.binarySearch(ctrlX, x);
        if (index > 0) {
            return ctrlY[index];
        }

        index = -(index + 1) - 1;

        /* linear interpolation or extrapolation */
        if (index < 0) {
            return ctrlY[0];
        }

        double yOut = a[index] + b[index] * (x - ctrlX[index]) + c[index]
                * Math.pow(x - ctrlX[index], 2) + d[index]
                * Math.pow(x - ctrlX[index], 3);

        /* Correction and restriction */

        int k = 0;
        for (k = 1; k < this.ctrlX.length; k++) {
            if (x <= this.ctrlX[k]) {
                break;
            }
        }

        if (this.ctrlX.length <= k) {
            return this.ctrlY[this.ctrlX.length - 1];
        }
        if (this.ctrlX[k] - x < x - this.ctrlX[k - 1] // xin closer to this.x[k]
                && this.ctrlY[k] > this.ctrlY[k - 1] && yOut > this.ctrlY[k]) {
            yOut = this.ctrlY[k];
        } else if (yOut > this.ctrlY[k - 1]
                && this.ctrlY[k] <= this.ctrlY[k - 1]) { // closer to this.x[k
                                                         // -1]
            yOut = this.ctrlY[k - 1];
        } else if (yOut > this.ctrlY[k - 1] && yOut > this.ctrlY[k]) {
            if (this.ctrlX[k] - x < x - this.ctrlX[k - 1]) {
                yOut = this.ctrlY[k];
            } else {
                yOut = this.ctrlY[k - 1];
            }

        }

        /* the increase only case, support cdf function */
        // this.x[k] - xin < xin - this.x[k -1]

        if (this.ctrlX[k] - x < x - this.ctrlX[k - 1] && increaseOnly
                && yOut < this.ctrlY[k - 1]) {
            yOut = this.ctrlY[k];
        }

        if (yOut < 0.0) {
            return 0.0;
        }

        return yOut;

    }

    /**
     * Used to check the correctness of this spline
     * 
     */
    public boolean checkValues() {
        if (ctrlX.length < 2) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Calculates the Spline coefficients.
     */
    private void calculateCoefficients() {
        int N = ctrlY.length;
        a = new double[N];
        b = new double[N];
        c = new double[N];
        d = new double[N];

        if (N == 2) {
            a[0] = ctrlY[0];
            b[0] = ctrlY[1] - ctrlY[0];
            return;
        }

        double[] h = new double[N - 1];
        for (int i = 0; i < N - 1; i++) {
            a[i] = ctrlY[i];
            h[i] = ctrlX[i + 1] - ctrlX[i];
            /* h[i] is used for division later, avoid a NaN */
            if (h[i] == 0.0) {
                h[i] = 0.01;
            }
        }
        a[N - 1] = ctrlY[N - 1];

        double[][] A = new double[N - 2][N - 2];
        double[] y = new double[N - 2];
        for (int i = 0; i < N - 2; i++) {
            y[i] = 3 * ((ctrlY[i + 2] - ctrlY[i + 1]) / h[i + 1] - (ctrlY[i + 1] - ctrlY[i])
                    / h[i]);

            A[i][i] = 2 * (h[i] + h[i + 1]);

            if (i > 0) {
                A[i][i - 1] = h[i];
            }

            if (i < N - 3) {
                A[i][i + 1] = h[i + 1];
            }
        }
        solve(A, y);

        for (int i = 0; i < N - 2; i++) {
            c[i + 1] = y[i];
            b[i] = (a[i + 1] - a[i]) / h[i] - (2 * c[i] + c[i + 1]) / 3 * h[i];
            d[i] = (c[i + 1] - c[i]) / (3 * h[i]);
        }
        b[N - 2] = (a[N - 1] - a[N - 2]) / h[N - 2] - (2 * c[N - 2] + c[N - 1])
                / 3 * h[N - 2];
        d[N - 2] = (c[N - 1] - c[N - 2]) / (3 * h[N - 2]);
    }

    /**
     * Solves Ax=b and stores the solution in b.
     */
    public void solve(double[][] A, double[] b) {
        int n = b.length;
        for (int i = 1; i < n; i++) {
            A[i][i - 1] = A[i][i - 1] / A[i - 1][i - 1];
            A[i][i] = A[i][i] - A[i - 1][i] * A[i][i - 1];
            b[i] = b[i] - A[i][i - 1] * b[i - 1];
        }

        b[n - 1] = b[n - 1] / A[n - 1][n - 1];
        for (int i = b.length - 2; i >= 0; i--) {
            b[i] = (b[i] - A[i][i + 1] * b[i + 1]) / A[i][i];
        }
    }

    // TODO: Keep the case test code for development and DR fixing
    // public static void main(String[] args) {
    // double[] x = new double[] { 1.0, 2.0, 3.0, 4.0, 5.0 };
    // double[] y = new double[] { .25, 0.0, .25, .5, 0.0 };
    // BSplineFunction f = new BSplineFunction(x, y);
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
