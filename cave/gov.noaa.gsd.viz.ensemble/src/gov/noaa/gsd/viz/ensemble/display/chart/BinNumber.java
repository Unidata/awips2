package gov.noaa.gsd.viz.ensemble.display.chart;

/**
 * Generates a 'number of bins' value based on the configuration and the source
 * data.
 * 
 * <pre>
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 21, 2015  12301       jing     Initial creation
 * 
 * </pre>
 * 
 * @author jing
 * @version 1.0
 */
public class BinNumber {
    /* The default number of bins */
    private final int DEFAULT_BIN_NUMBER = 7;

    /* Current number of bins generated with configuration and source data */
    private int currentBinNum;

    /**
     * Constructor.
     */
    public BinNumber() {
        this.currentBinNum = DEFAULT_BIN_NUMBER;
    }

    /**
     * Constructor with a bin number.
     * 
     * @param - The bin number.
     */
    public BinNumber(int currentBinNum) {
        this.currentBinNum = currentBinNum;
    }

    /**
     * Constructor with the configuration and source data size.
     * 
     * @param config
     *            -The configuration
     * @param dataSize
     *            -The data Size
     */
    public BinNumber(ChartConfig config, int dataSize) {
        this.currentBinNum = calculateBinNumWithConfig(config, dataSize);
    }

    /**
     * Gets the bin number.
     * 
     * @return -the bin number
     */
    public int getCurrentBinNum() {
        return currentBinNum;
    }

    /**
     * Sets the bin number.
     * 
     * @param -The bin number
     */
    public void setCurrentBinNum(int currentBinNum) {
        this.currentBinNum = currentBinNum;
    }

    /**
     * Sets the bin number with the configuration and source data size.
     * 
     * @param config
     *            -The configuration
     * @param dataSize
     *            -The data size
     */
    public void setCurrentBinNum(ChartConfig config, int dataSize) {
        this.currentBinNum = calculateBinNumWithConfig(config, dataSize);
    }

    /**
     * Sets the bin number with the configuration and source data. Must use this
     * method when configuration is ChartConfig.BinChoicer.BIN_ALPS.
     * 
     * @param config
     *            -The configuration
     * @param data
     *            -The data
     */
    public void setCurrentBinNum(ChartConfig config, float[] data) {
        this.currentBinNum = calculateBinNumWithConfig(config, data);
    }

    /**
     * Gets the bin number with the configuration and source data size.
     * 
     * @param config
     *            - The configuration
     * @param data
     *            -The data size
     * @return
     */
    public int calculateBinNumWithConfig(ChartConfig config, int dataSize) {

        ChartConfig.BinChooser binMethod = config.getBinChoicer();

        switch (binMethod) {
        case BIN_11:
            currentBinNum = 11;
            break;
        case BIN_9:
            currentBinNum = 9;
            break;
        case BIN_7:
            currentBinNum = 7;
            break;
        case BIN_5:
            currentBinNum = 5;
            break;
        case RISE_RULE:
            currentBinNum = riseRule(dataSize);
            break;
        case SQUARE_ROOT:
            currentBinNum = squareRoot(dataSize);
            break;
        case STURGES_FORMULA:
            currentBinNum = sturgesFormula(dataSize);
            break;
        default:

            break;
        }

        return currentBinNum;

    }

    /**
     * Gets the bin number with the configuration and source data. Must use this
     * method when is configured as the ChartConfig.BinChoicer.BIN_ALPS.
     * 
     * @param config
     *            - The configuration
     * @param data
     *            -The data
     * @return The bin number
     */
    public int calculateBinNumWithConfig(ChartConfig config, float[] data) {

        ChartConfig.BinChooser binMethod = config.getBinChoicer();

        switch (binMethod) {
        case BIN_11:
            currentBinNum = 11;
            break;
        case BIN_9:
            currentBinNum = 9;
            break;
        case BIN_7:
            currentBinNum = 7;
            break;
        case BIN_5:
            currentBinNum = 5;
            break;
        case RISE_RULE:
            currentBinNum = riseRule(data.length);
            break;
        case SQUARE_ROOT:
            currentBinNum = squareRoot(data.length);
            break;
        case STURGES_FORMULA:
            currentBinNum = sturgesFormula(data.length);
            break;
        case BIN_ALPS:
            currentBinNum = alphaBinNum(data);
            break;
        default:

            break;
        }

        return currentBinNum;

    }

    /**
     * Generates a bin number with Rise Rule method.
     * 
     * @param dataSize
     *            -The data size
     * @return The bin number
     */
    private int riseRule(int dataSize) {
        int binNum = currentBinNum;

        if (dataSize != 0) {
            binNum = (int) (.5 + 2 * Math.pow((double) dataSize, 1 / 3.0));
        }
        return binNum;
    }

    /**
     * Generates a bin number with Square Root method.
     * 
     * @param dataSize
     *            -The data size
     * @return The bin number
     */
    private int squareRoot(int dataSize) {
        int binNum = currentBinNum;

        if (dataSize != 0) {
            binNum = 1 + (int) (.5 + Math.pow((double) dataSize, 0.5));
        }
        return binNum;
    }

    /**
     * Generates a bin number with Sturges Formula method.
     * 
     * @param dataSize
     *            -The data size
     * @return The bin number
     */
    private int sturgesFormula(int dataSize) {
        int binNum = currentBinNum;

        if (dataSize != 0) {
            binNum = 1 + (int) (.5 + Math.log((double) dataSize)
                    / Math.log(2.0));
        }
        return binNum;
    }

    /**
     * An approximate size for the bins on the ordinate of a histogram is input
     * in delta. This is changed to a rounded value for plotting, and delLbl is
     * how often to label the ordinatea. The ALPS bin number method is from
     * theText histogram of ALPS, created by James Ramer.
     * 
     * @param delta
     * @param delLbl
     */
    private void computeBinDelta(float delta, float delLbl) {
        float ee = (float) Math.pow(10, (int) Math.log10(delta));
        delta /= ee;
        while (delta > 10) {
            delta /= 10;
            ee *= 10;
        }
        while (delta < 1) {
            delta *= 10;
            ee /= 10;
        }
        if (delta > 7) {
            delta = 10 * ee;
            delLbl = 50 * ee;
        } else if (delta < 1.414) {
            delta = ee;
            delLbl = 5 * ee;
        } else if (delta > 3.333) {
            delta = 5 * ee;
            delLbl = 20 * ee;
        } else {
            delta = 2 * ee;
            delLbl = 10 * ee;
        }
    }

    /**
     * Generates a bin number with the ALPS method using doubles.
     * 
     * @param values
     *            -The data values
     * @return The bin number
     */
    public int alphaBinNum(double[] values) {
        float[] vd = new float[values.length];
        for (int i = 0; i < values.length; i++) {
            vd[i] = (float) values[i];
        }
        return alphaBinNum(vd);
    }

    /**
     * Generates a bin number with the ALPS method using floats.
     * 
     * @param values
     *            -The data values
     * @return The bin number
     */
    public int alphaBinNum(float[] values) {

        /* Check if histogram sampling is possible. */
        if (values == null || values.length == 0) {

            return currentBinNum;
        }

        float minBin = 10;
        float minLbl = 10;

        /* Show a depiction of the weights being used. */
        int i, j, n, n2;

        /*
         * Histogram case, the most complex, start by gathering the values. If
         * none are toggled on, we will base this on all inputs which exist.
         */

        float minVal, maxVal;
        minVal = maxVal = 0;
        n = values.length;
        n2 = 2 * n;

        for (i = j = 0; i < n2; i++) {
            j = i % n;
            if (i == j) {
                n2 = n;
            }

            float oneval = values[j];

            if (oneval == Float.NaN) {
                continue;
            }

            if (minVal == maxVal && maxVal == 0) {
                minVal = maxVal = oneval;
            } else if (oneval < minVal) {
                minVal = oneval;
            } else if (oneval > maxVal) {
                maxVal = oneval;
            }
        }

        /*
         * Compute the best size to use for the value bins (dv). This responds
         * to density by using smaller increments along the x-axis when density
         * is larger.
         */

        if (minVal >= maxVal) {

            return currentBinNum;
        }
        float dv = minBin;

        dv = (float) .5;
        float delLbl = minLbl * dv / minBin;
        if (dv == 0.5) {
            delLbl = 1;
        } else if (dv == 0 || (maxVal - minVal) > dv * 25) {
            float minDv = (maxVal > -minVal ? maxVal : -minVal) / 5000;
            if (dv < minDv) {
                dv = minDv;
            }
            computeBinDelta(dv, delLbl);
        } else if (dv != minBin) {
            computeBinDelta(dv, delLbl);
        }

        int nbMax = 20;
        if ((maxVal - minVal) / dv > nbMax) {
            float dvSeed, dvPrev;
            for (dvSeed = dvPrev = dv, i = 0; i < 100; i++) {
                dvSeed *= 1.4;
                dv = dvSeed;
                computeBinDelta(dv, delLbl);
                if (dv == dvPrev) {
                    continue;
                }
                if ((maxVal - minVal) / dv < nbMax) {
                    break;
                }
                dvPrev = dvSeed = dv;
            }
        }

        /*
         * We want edge of the bins to be at half the bin size, containing at
         * least two even values of del2.
         */

        float del2 = delLbl * 2;
        float edge = dv * (((int) (minVal / dv)) - (float) 0.5);
        while (minVal - edge > dv) {
            edge += dv;
        }
        while (edge > minVal) {
            edge -= dv;
        }
        minVal = edge;
        edge = dv * (((int) (maxVal / dv)) + (float) 0.5);
        while (edge - maxVal > dv) {
            edge -= dv;
        }
        while (edge < maxVal) {
            edge += dv;
        }
        maxVal = edge;
        int nbins = (int) (0.5 + (maxVal - minVal) / dv);
        float mean2 = (minVal + maxVal) / 2;
        mean2 = mean2 > 0 ? del2 * (int) (0.5 + mean2 / del2) : -del2
                * (int) (0.5 - mean2 / del2);
        while (minVal > mean2 - del2 && maxVal < mean2 + del2) {
            nbins += 2;
            minVal -= dv;
            maxVal += dv;
        }

        return nbins;
    }

    // TODO: Keep the case test code for development and DR fixing
    // public static void main(String[] args) {
    // BinNumber binNumber = new BinNumber();
    // for (int i = 0; i < 100; i++) {
    // System.out.println(" i= " + i + " RISE_RULE="
    // + binNumber.riseRule(i) + "SQUARE_ROOT="
    // + binNumber.squareRoot(i) + " STURGES_FORMULA="
    // + binNumber.sturgesFormula(i));
    // }
    // double[] v1 = { 0.2 };
    // double[] v2 = { .0, .2 };
    // double[] v3 = { .0, .2, .0 };
    // double[] v6 = { .0, .2, .0, .5, .01, .04 };
    // double[] v12 = { .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01, .04 };
    // double[] v24 = { .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01, .04,
    // .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01, .04 };
    // double[] v48 = { .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01, .04,
    // .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01, .04, .0, .2, .0,
    // .5, .01, .04, .0, .2, .0, .5, .01, .04, .0, .2, .0, .5, .01,
    // .04, .0, .2, .0, .5, .01, .04 };
    // System.out.println(" v1 bins=" + binNumber.alphaBinNum(v1));
    // System.out.println(" v2 bins=" + binNumber.alphaBinNum(v2));
    // System.out.println(" v3 bins=" + binNumber.alphaBinNum(v3));
    // System.out.println(" v6 bins=" + binNumber.alphaBinNum(v6));
    // System.out.println(" v12 bins=" + binNumber.alphaBinNum(v12));
    // System.out.println(" v24 bins=" + binNumber.alphaBinNum(v24));
    // System.out.println(" v48 bins=" + binNumber.alphaBinNum(v48));
    // }

}
