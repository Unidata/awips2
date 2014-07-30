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

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.pointdata.PointDataView;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelCoverage;
import com.raytheon.uf.viz.core.drawables.IImage;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;
import com.raytheon.viz.pointdata.util.SymbolLoader;
import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Resource for SigWx VTS Data.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 28, 2009 3099       bsteffen    Initial creation
 * Sep 28, 2009 3099       bsteffen    Updated to conform with common SigWxResource
 * Jul 29, 2014 3465       mapeters    Updated deprecated drawString() calls.
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
public class SigWxVtsResource extends SigWxResource {

    protected static final String P_LATITUDE = "latitude";

    protected static final String P_LONGITUDE = "longitude";

    protected static final String P_FEATURE_NAME = "featureName";

    protected static final String P_MET_FEATURE = "metFeature";

    protected static final String P_ATTRIB_SIG = "attribSig";

    protected static final String[] PARAMETERS = { P_LATITUDE, P_LONGITUDE,
            P_FEATURE_NAME, P_MET_FEATURE, P_ATTRIB_SIG };

    protected static final char VOLCANO_SYMBOL = '\u00b1';

    protected static final char STORM_SYMBOL = '\u0078';

    private SymbolLoader symbolLoader;

    protected SigWxVtsResource(SigWxResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
    }

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        super.initInternal(target);
        this.symbolLoader = new SymbolLoader();
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps, PointDataView pdv) throws VizException {

        RGB color = getCapability(ColorableCapability.class).getColor();

        String featureName = pdv.getString(P_FEATURE_NAME);
        String text = "";
        Number lat = pdv.getNumber(P_LATITUDE);
        Number lon = pdv.getNumber(P_LONGITUDE);
        int metFeature = pdv.getInt(P_MET_FEATURE);
        int attribSig = pdv.getInt(P_ATTRIB_SIG);
        double[] loc = descriptor.worldToPixel(new double[] {
                lon.doubleValue(), lat.doubleValue() });
        double scale[] = getScale(paintProps);
        IImage image = null;
        if (metFeature == 17) {
            image = symbolLoader.getImage(target, color, VOLCANO_SYMBOL);
        } else if (attribSig == 1) {
            image = symbolLoader.getImage(target, color, STORM_SYMBOL);
        }
        if (image != null) {
            Coordinate ul = new Coordinate(loc[0], loc[1] - 6 * scale[1]);
            Coordinate ur = new Coordinate(loc[0] + 12 * scale[0], ul.y);
            Coordinate lr = new Coordinate(ur.x, loc[1] + 6 * scale[1]);
            Coordinate ll = new Coordinate(loc[0], lr.y);
            PixelCoverage extent = new PixelCoverage(ul, ur, lr, ll);
            target.drawRaster(image, extent, paintProps);
            loc[0] += 12 * scale[0];
            text = "  " + featureName;
        } else {
            text = "???  " + featureName;
        }
        DrawableString string = new DrawableString(text, color);
        string.font = font;
        string.setCoordinates(loc[0], loc[1]);
        string.verticallAlignment = VerticalAlignment.MIDDLE;
        target.drawStrings(string);
    }

    @Override
    public String getName() {
        return "SIGWX VTS";
    }

    protected String[] getParameters() {
        return PARAMETERS;
    }

}