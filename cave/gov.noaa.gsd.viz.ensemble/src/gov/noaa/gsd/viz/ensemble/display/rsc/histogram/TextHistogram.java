package gov.noaa.gsd.viz.ensemble.display.rsc.histogram;

import java.util.ArrayList;
import java.util.List;

import com.raytheon.uf.viz.core.rsc.AbstractVizResource;

/**
 * Generate the text of the histogram. This algorithm is from ALPS(C++) by James
 * Ramer. First step: implement single color; Second step:multi-color.
 * 
 * 
 * @author jing
 * @version 1.0
 * 
 *          <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July, 2014     5056       jing       Initial creation
 * 
 * </pre>
 */

public class TextHistogram {

    private final int BLOCK_SIZE = 400;

    private float _minBin = 10;

    private float _minLbl = 10;

    private boolean isColorHist;

    private String unit = "";

    public TextHistogram(boolean isColorDiagram) {

        isColorHist = false;
    }

    public String makeHistogram(List<Float> data) {
        return null;
    }

    // -------------------------------------------------------------
    // char * defaultMultiColor()
    //
    // For a multi color sample string for which all characters will have the
    // color of the sampling overlay, put a space after all non-whitespace
    // characters. Returns pointer to terminating null.
    // -- implementation
    // ---------------------------------------------------------
    // TODO
    private char[] defaultMultiColor(char[] wrkStr) {
        int newSize = wrkStr.length;
        for (char ch : wrkStr) {
            if (ch > ' ') {
                newSize++;
            }
        }

        char[] newStr = new char[newSize];

        int j = 0;
        for (int i = 0; i < wrkStr.length; i++) {
            newStr[j] = wrkStr[i];
            j++;
            if (wrkStr[i] > ' ') {
                newStr[j] = ' ';
                j++;
            }
        }

        return newStr;
    }

    // -------------------------------------------------------------
    // void computeBinDelta(float & delta, float & delLbl)
    //
    // An approximate size for the bins on the ordinate of a histogram is
    // input in delta. This is changed to a rounded value for ploting, and
    // delLbl is how often to label the ordinate.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    // private void computeBinDelta(float & delta, float & delLbl)
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

    // -----------------------------------------------------------------
    // TextString FunctionOverlayDepict::interogate()
    // Adds one or both color bars to image data, as required.
    //
    // New multi-color sampling overlays need to have an instance added for
    // their
    // function name in DepictSeq::samplingCategory().
    // ---------------------------------------------------------------------------
    public List<String> interrogate(List<AbstractVizResource<?, ?>> rscs,
            List<Float> values, String unit) {
        List<String> histStr = new ArrayList<String>();
        // Most common case; computational overlay, sample with _implDepict

        this.unit = unit;

        // Check if histogram sampling is possible.
        if (rscs.size() == 0) {
            histStr.add("NO DATA");
            return histStr;
        }

        // Show a depiction of the weights being used.
        int i, j, n, n2;

        // Histogram case, the most complex, start by gathering the values.
        // If none are toggled on, we will base this on all inputs which exist.

        float minVal, maxVal;
        minVal = maxVal = 0;
        n = rscs.size();
        n2 = 2 * n;
        if (values.size() == 0) {
            histStr.add("NO VALUES");
            return histStr;
        }
        for (i = j = 0; i < n2; i++) {
            j = i % n;
            if (i == j) {
                n2 = n;
            }

            float oneval = values.get(j);

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

        // Compute the best size to use for the value bins (dv).
        // This responds to density by using smaller increments along
        // the x-axis when density is larger.

        char[] wrkstr = new char[BLOCK_SIZE];
        if (minVal >= maxVal) {
            wrkstr = (values.size() + " values of " + maxVal + "\n")
                    .toCharArray();
            histStr.add(wrkstr.toString());
            return histStr;

        }
        float dv = _minBin;

        dv = (float) .5;
        float delLbl = _minLbl * dv / _minBin;
        if (dv == 0.5) {
            delLbl = 1;
        } else if (dv == 0 || (maxVal - minVal) > dv * 25) {
            float minDv = (maxVal > -minVal ? maxVal : -minVal) / 5000;
            if (dv < minDv) {
                dv = minDv;
            }
            computeBinDelta(dv, delLbl);
        } else if (dv != _minBin) {
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

        // We want edge of the bins to be at half the bin size, containing at
        // least
        // two even values of del2.
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

        // Determine the proper y-axis for the counts, we want about 10 steps in
        // the vertical axis, and the max value rounded to the nearest 5 counts.
        Integer[] counts = new Integer[nbins];
        for (int k = 0; k < nbins; k++) {
            counts[k] = 0;
        }

        nbins--;
        int maxCount = 0;
        for (i = 0; i < values.size(); i++) {
            j = (int) ((values.get(i) - minVal) / dv);
            if (j < 0) {
                j = 0;
            }
            if (j > nbins) {
                j = nbins;
            }
            counts[j]++;
            if (counts[j] > maxCount) {
                maxCount = counts[j];
            }

        }
        maxCount = ((4 + maxCount) / 5) * 5;
        i = values.size() / 2;
        // while (maxCount<i) maxCount += 5;
        while (maxCount < i) {
            maxCount += 2;
        }
        int dCount = 1;
        int eCount = 1;
        while (maxCount > dCount * eCount * 10) {
            if (dCount == 5) {
                eCount *= 10;
                dCount = 1;
            } else {
                dCount = dCount == 1 ? 2 : 5;
            }
        }
        dCount *= eCount;
        // TextString histStr;

        eCount = (maxCount / dCount) * dCount;
        if (eCount == maxCount) {
            eCount -= dCount;
        }

        eCount++;

        // First line, the top
        String tmpStr = String.format("%3d", maxCount);
        tmpStr += "| ";
        histStr.add(tmpStr);

        while (eCount > 0) {
            int k = 0;
            for (char ch : "    | ".toCharArray()) {
                wrkstr[k] = ch;
                k++;
            }
            int cp = 6;
            for (j = 0; j <= nbins; j++, cp++) {
                if (counts[j] == 0 || counts[j] < eCount) {
                    wrkstr[cp] = ' ';
                } else {
                    wrkstr[cp] = 'x';
                }
            }

            wrkstr[cp++] = '\n';
            wrkstr[cp] = '\0';

            histStr.add(String.copyValueOf(wrkstr, 0, cp - 1));

            eCount -= dCount;

        }

        // X-axis line

        int k = 0;
        for (char ch : "    --".toCharArray()) {
            wrkstr[k] = ch;
            k++;
        }
        edge = minVal + dv / 2;

        int cp = 6;
        for (j = 0; j <= nbins; j++, cp++, edge += dv) {
            float d = edge / del2;
            if (d < 0) {
                d = -d;
            }
            d -= (int) d;
            if (d < 0.01 || d > 0.99) {
                wrkstr[cp] = '|';
            } else if (d > 0.49 && d < 0.51) {
                wrkstr[cp] = ':';
            } else {
                wrkstr[cp] = '-';
            }
        }
        wrkstr[cp++] = '\n';
        wrkstr[cp] = '\0';

        // if (idLists)
        // defaultMultiColor(wrkstr);

        histStr.add(String.copyValueOf(wrkstr, 0, cp - 1));
        // Mark X-axis
        for (k = 0; k < BLOCK_SIZE; k++) {
            wrkstr[k] = ' ';
        }

        String scr = "";
        for (k = 0; k < BLOCK_SIZE; k++) {
            scr += ' ';
        }

        cp = 0;
        dCount = 0;
        edge = minVal + dv / 2;
        edge = edge > 0 ? del2 * (int) (0.5 + edge / del2) : -del2
                * (int) (0.5 - edge / del2);
        while (edge < minVal) {
            edge += del2;
        }

        while (edge < maxVal) {

            scr = String.format("%.4f", edge);
            scr = String.copyValueOf(scr.toCharArray(), 0, 4);
            i = scr.length();
            j = (int) ((edge - minVal) / dv) + 5 - i / 2;

            for (k = 0; k < i; k++) {
                wrkstr[k + j] = scr.charAt(k);
            }
            cp = i + j;
            dCount++;
            edge += del2;
        }
        edge -= delLbl;
        if (dCount >= 2) {
            // TODO
            ;
        } else if (edge > maxVal) {
            edge -= del2;

            scr = String.format("%.1f", edge);

            scr.length();
            j = (int) ((edge - minVal) / dv) + 5 - i / 2;
            for (k = 0; k < i; k++) {
                wrkstr[k + j] = scr.charAt(k);
            }

        } else {

            scr = String.format("%.1f", edge);
            scr.length();
            j = (int) ((edge - minVal) / dv) + 5 - i / 2;
            for (k = 0; k < i; k++) {
                wrkstr[k + j] = scr.charAt(k);
            }

            cp = i + j;
        }
        wrkstr[cp++] = '\n';
        wrkstr[cp] = '\0';

        histStr.add(String.copyValueOf(wrkstr, 0, cp - 1));

        return histStr;

    }

}
