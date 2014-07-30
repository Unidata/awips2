/**
 * This software was developed and / or modified by Raytheon Company,
 * pursuant to Contract DG133W-05-CQ-1067 with the US Government.
 * 
 * U.S. EXPORT CONTROLLED TECHNICAL DATA
 * This software product contains export-restricted data whose
 * export/transfer/disclosure is restricted by U.S. law. Dissemination
 * to non-U.S. persons whether in the United States or abroad requires
 * an export license or other authorization.
 * 
 * Contractor Name:        Raytheon Company
 * Contractor Address:     6825 Pine Street, Suite 340
 *                         Mail Stop B8
 *                         Omaha, NE 68106
 *                         402.291.0100
 * 
 * See the AWIPS II Master Rights File ("Master Rights File.pdf") for
 * further licensing information.
 **/
package com.raytheon.uf.viz.bufrsigwx.rsc;

import java.awt.geom.Rectangle2D;
import java.util.ArrayList;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.LineStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * A polygon resource which can handle SigWx cloud data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 25, 2009 3099       bsteffen     Initial creation
 * Sep 28, 2009 3099       bsteffen     Updated to conform with common SigWxResource
 * Jul 29, 2014 3465       mapeters     Updated deprecated drawStrings() calls.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SigWxCloudsResource extends SigWxPolygonResource {

    private static final String[] distrTypes = { "CLEAR", "FEW", "SCT", "BRKN",
            "OVC", "", "SCT/BKN", "BKN/OVC", "ISOL", "ISOL/EMBD", "OCNL",
            "OCNL/EMBD", "FRQNT", "DENSE", "LYRS" };

    private static final String[] cloudTypes = { "CI", "CC", "CS", "AC", "AS",
            "NS", "SC", "ST", "CU", "CB" };

    private static final String[] icingTypes = { "NONE", "LIGHT", "LIGHT",
            "LIGHT", "MODERATE", "MODERATE", "MODERATE", "SEVERE", "SEVERE",
            "SEVERE", "TRACE", "TRACE", "TRACE" };

    private static final String[] icingTypesSym = { "\u0000", "\u007d",
            "\u007d", "\u007d", "\u007e", "\u007e", "\u007e", "\u007f",
            "\u007f", "\u007f", "\u007d", "\u007d", "\u007d" };

    private static final String P_NUM_OF_POINTS_2 = "numOfVertices";

    private static final String P_CLOUD_DIST = "cloudDistribution";

    private static final String P_CLOUD_TYPE = "cloudType";

    private static final String P_CLOUD_TOP = "cloudTop";

    private static final String P_CLOUD_BASE = "cloudBase";

    private static final String P_ICING_TYPE = "icingType";

    private static final String P_ICING_TOP = "icingTop";

    private static final String P_ICING_BASE = "icingBase";

    private static final String P_TURB_TYPE = "turbType";

    private static final String P_TURB_TOP = "turbTop";

    private static final String P_TURB_BASE = "turbBase";

    private static final String[] PARAMETERS = { P_LATITUDE, P_LONGITUDE,
            P_NUM_OF_POINTS, P_NUM_OF_POINTS_2, P_CLOUD_DIST, P_CLOUD_TYPE,
            P_CLOUD_TOP, P_CLOUD_BASE, P_ICING_TYPE, P_ICING_TOP, P_ICING_BASE,
            P_TURB_TYPE, P_TURB_TOP, P_TURB_BASE };

    private static final int PADDING = 3;

    public SigWxCloudsResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    protected int getNumPoints(PointDataView pdv) {
        int length;
        if (pdv.getDimensions(P_NUM_OF_POINTS) != -1) {
            length = pdv.getInt(P_NUM_OF_POINTS);
        } else {
            length = pdv.getInt(P_NUM_OF_POINTS_2);
        }
        return length;
    }

    protected void drawText(IGraphicsTarget target, PaintProperties paintProps,
            double[] textLoc, double[] scale, RGB color, PointDataView pdv)
            throws VizException {
        double[] symDim = getSymbolDimensions(pdv);
        double[] textDim = getTextDimensions(target, pdv);
        double[] boxDim = getTextBoxDimensions(target, pdv);
        double y = textLoc[1] + (boxDim[1] / 2 - symDim[1] / 2) * scale[1];
        for (String symbol : getSymbolLines(pdv)) {
            // Center the line in its column
            double lineLength = symbol.length() * 12;
            double x = textLoc[0] + (PADDING + symDim[0] / 2 - lineLength / 2)
                    * scale[0];
            for (int i = 0; i < symbol.length(); i++) {
                IImage image = symbolLoader.getImage(target, color, symbol
                        .charAt(i));
                Coordinate ul = new Coordinate(x, y);
                Coordinate ur = new Coordinate(x + 12 * scale[0], y);
                Coordinate lr = new Coordinate(ur.x, y + 12 * scale[1]);
                Coordinate ll = new Coordinate(x, lr.y);
                PixelCoverage extent = new PixelCoverage(ul, ur, lr, ll);
                target.drawRaster(image, extent, paintProps);
                x += 12 * scale[0];
            }
            y += 24 * scale[1];
        }
        y = textLoc[1] + (PADDING) * scale[1];
        double x;
        if (symDim[0] == 0) {
            x = textLoc[0] + (symDim[0] + textDim[0] / 2 + PADDING * 1)
                    * scale[0];
        } else {
            x = textLoc[0] + (symDim[0] + textDim[0] / 2 + PADDING * 3)
                    * scale[0];
        }
        DrawableString string = new DrawableString(getTextLines(pdv), color);
        string.font = font;
        string.setCoordinates(x, y);
        string.horizontalAlignment = HorizontalAlignment.CENTER;
        string.verticallAlignment = VerticalAlignment.TOP;
        target.drawStrings(string);
    }

    private String[] getTextLines(PointDataView pdv) {
        ArrayList<String> lines = new ArrayList<String>();
        int icingType = -9999;
        int turbType = -9999;
        if (pdv.getDimensions(P_ICING_TYPE) != -1) {
            icingType = pdv.getInt(P_ICING_TYPE);
        }
        if (icingType != -9999) {
            float icingTop = pdv.getFloat(P_ICING_TOP);
            float icingBase = pdv.getFloat(P_ICING_BASE);
            lines.add(formatNumber(metersToFl(icingTop)));
            lines.add(formatNumber(metersToFl(icingBase)));
        }
        if (pdv.getDimensions(P_TURB_TYPE) != -1) {
            turbType = pdv.getInt(P_TURB_TYPE);
        }
        if (turbType != -9999) {
            float turbTop = pdv.getFloat(P_TURB_TOP);
            float turbBase = pdv.getFloat(P_TURB_BASE);
            lines.add(formatNumber(metersToFl(turbTop)));
            lines.add(formatNumber(metersToFl(turbBase)));
        }
        if (icingType == -9999 && turbType == -9999) {
            int cloudDist = pdv.getInt(P_CLOUD_DIST);
            int cloudType = pdv.getInt(P_CLOUD_TYPE);
            float cloudTop = pdv.getFloat(P_CLOUD_TOP);
            float cloudBase = pdv.getFloat(P_CLOUD_BASE);
            if (cloudDist > 1) {
                for (String line : distrTypes[cloudDist].split("/")) {
                    lines.add(line);
                }
            }
            if (cloudType > 1) {
                lines.add(cloudTypes[cloudType]);
            }
            lines.add(formatNumber(metersToFl(cloudTop)));
            lines.add(formatNumber(metersToFl(cloudBase)));
        }
        return lines.toArray(new String[lines.size()]);
    }

    private String[] getSymbolLines(PointDataView pdv) {
        ArrayList<String> lines = new ArrayList<String>();
        int icingType = -9999;
        int turbType = -9999;
        if (pdv.getDimensions(P_ICING_TYPE) != -1) {
            icingType = pdv.getInt(P_ICING_TYPE);
        }
        if (icingType != -9999) {
            lines.add(icingTypesSym[icingType]);
        }
        if (pdv.getDimensions(P_TURB_TYPE) != -1) {
            turbType = pdv.getInt(P_TURB_TYPE);
        }
        if (turbType != -9999) {
            lines.add(turbTypesSym[turbType]);
        }
        return lines.toArray(new String[lines.size()]);

    }

    protected double[] getTextBoxDimensions(IGraphicsTarget target,
            PointDataView pdv) {
        // Calculate the height and Width of the text box
        double[] symDim = getSymbolDimensions(pdv);
        double[] textDim = getTextDimensions(target, pdv);
        double[] boxDim = new double[2];
        if (symDim[0] == 0) {
            boxDim[0] = textDim[0] + symDim[0] + PADDING * 1;
        } else {
            boxDim[0] = textDim[0] + symDim[0] + PADDING * 4;
        }
        boxDim[1] = Math.max(textDim[1], symDim[1]) + PADDING * 2;
        return boxDim;
    }

    private double[] getTextDimensions(IGraphicsTarget target, PointDataView pdv) {
        // Calculate the height and Width of the text box
        double textWidth = 0;
        double textHeight = 0;
        for (String line : getTextLines(pdv)) {
            Rectangle2D rect = target.getStringBounds(font, line);
            if (rect.getWidth() > textWidth) {
                textWidth = rect.getWidth();
            }
            textHeight += rect.getHeight() + 1;
        }
        return new double[] { textWidth, textHeight };
    }

    private double[] getSymbolDimensions(PointDataView pdv) {
        double symWidth = 0;
        double symHeight = -12;
        for (String line : getSymbolLines(pdv)) {
            if (line.length() * 12 > symWidth) {
                symWidth = line.length() * 12;
            }
            symHeight += 24;
        }
        return new double[] { symWidth, symHeight };
    }

    protected String getInspectString(PointDataView pdv) {
        StringBuilder text = new StringBuilder();
        int icingType = -9999;
        int turbType = -9999;
        if (pdv.getDimensions(P_ICING_TYPE) != -1
                && pdv.getDimensions(P_TURB_TYPE) != -1) {
            icingType = pdv.getInt(P_ICING_TYPE);
            turbType = pdv.getInt(P_TURB_TYPE);
        }
        if (turbType != -9999) {
            float turbTop = pdv.getFloat(P_TURB_TOP);
            float turbBase = pdv.getFloat(P_TURB_BASE);
            text.append(" Turb: ");
            text.append(turbTypes[turbType]);
            text.append(' ');
            text.append(formatNumber(metersToFl(turbTop)));
            text.append('/');
            text.append(formatNumber(metersToFl(turbBase)));
        }
        if (icingType != -9999) {
            float icingTop = pdv.getFloat(P_ICING_TOP);
            float icingBase = pdv.getFloat(P_ICING_BASE);
            text.append(" Icing: ");
            text.append(icingTypes[icingType]);
            text.append(' ');
            text.append(formatNumber(metersToFl(icingTop)));
            text.append('/');
            text.append(formatNumber(metersToFl(icingBase)));
        }
        if (turbType == -9999 && icingType == -9999) {
            int cloudDist = pdv.getInt(P_CLOUD_DIST);
            int cloudType = pdv.getInt(P_CLOUD_TYPE);
            float cloudTop = pdv.getFloat(P_CLOUD_TOP);
            float cloudBase = pdv.getFloat(P_CLOUD_BASE);
            if (cloudDist > 1) {
                text.append(distrTypes[cloudDist]);
                text.append(' ');
            }
            if (cloudType > 1) {
                text.append(cloudTypes[cloudType]);
                text.append(' ');
            }
            text.append(formatNumber(metersToFl(cloudTop)));
            text.append('/');
            text.append(formatNumber(metersToFl(cloudBase)));

        }

        return text.toString();
    }

    @Override
    protected LineStyle getLineStyle() {
        return LineStyle.SOLID;
    }

    @Override
    protected String[] getParameters() {
        return PARAMETERS;
    }

}
