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
 * A polygon resource which can handle SigWx CAT data.
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
public class SigWxCatResource extends SigWxPolygonResource {

    private static final String P_TURB_TYPE = "degreeOfTurb";

    private static final String P_CAT_TOP = "catTop";

    private static final String P_CAT_BASE = "catBase";

    public static final String[] PARAMETERS = { P_LATITUDE, P_LONGITUDE,
            P_NUM_OF_POINTS, P_TURB_TYPE, P_CAT_TOP, P_CAT_BASE };

    public static final int PADDING = 3;

    public SigWxCatResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected int getNumPoints(PointDataView pdv) {
        return pdv.getInt(P_NUM_OF_POINTS);
    }

    protected void drawText(IGraphicsTarget target, PaintProperties paintProps,
            double[] textLoc, double[] scale, RGB color, PointDataView pdv)
            throws VizException {
        double[] symDim = getSymbolDimensions(pdv);
        double[] boxDim = getTextBoxDimensions(target, pdv);
        // Horizontally Center the symbols
        double x = textLoc[0] + (boxDim[0] / 2 - symDim[0] / 2) * scale[0];
        double y = textLoc[1] + PADDING * scale[1];

        String symbol = getSymbolLine(pdv);
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
        y = textLoc[1] + (symDim[1] + 3 * PADDING) * scale[1];
        x = textLoc[0] + (boxDim[0] / 2) * scale[0];

        DrawableString string = new DrawableString(getTextLines(pdv), color);
        string.font = font;
        string.setCoordinates(x, y);
        string.horizontalAlignment = HorizontalAlignment.CENTER;
        string.verticallAlignment = VerticalAlignment.TOP;
        target.drawStrings(string);
    }

    private String[] getTextLines(PointDataView pdv) {
        float catTop = pdv.getFloat(P_CAT_TOP);
        float catBase = pdv.getFloat(P_CAT_BASE);
        String catTopString = formatNumber(metersToFl(catTop));
        String catBaseString = formatNumber(metersToFl(catBase));
        return new String[] { catTopString, catBaseString };
    }

    private String getSymbolLine(PointDataView pdv) {
        int turbType = pdv.getInt(P_TURB_TYPE);
        return turbTypesSym[turbType];

    }

    protected double[] getTextBoxDimensions(IGraphicsTarget target,
            PointDataView pdv) {
        // Calculate the height and Width of the text box
        double[] symDim = getSymbolDimensions(pdv);
        double[] textDim = getTextDimensions(target, pdv);
        double[] boxDim = new double[2];
        // Padding on Left and Right
        boxDim[0] = Math.max(textDim[0], symDim[0]) + PADDING * 2;
        // Padding on Left and right and two in between
        boxDim[1] = textDim[1] + symDim[1] + PADDING * 4;
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
        String line = getSymbolLine(pdv);
        double symWidth = line.length() * 12;
        double symHeight = 12;
        return new double[] { symWidth, symHeight };
    }

    protected String getInspectString(PointDataView pdv) {
        StringBuilder text = new StringBuilder();
        int turbType = pdv.getInt(P_TURB_TYPE);
        float catTop = pdv.getFloat(P_CAT_TOP);
        float catBase = pdv.getFloat(P_CAT_BASE);
        text.append(turbTypes[turbType]);
        text.append(' ');
        text.append(formatNumber(metersToFl(catTop)));
        text.append('/');
        text.append(formatNumber(metersToFl(catBase)));
        return text.toString();
    }

    @Override
    protected LineStyle getLineStyle() {
        return LineStyle.DASHED;
    }

    @Override
    protected String[] getParameters() {
        return PARAMETERS;
    }

}
