/**
 * This code has unlimited rights, and is provided "as is" by the National Centers 
 * for Environmental Prediction, without warranty of any kind, either expressed or implied, 
 * including but not limited to the implied warranties of merchantability and/or fitness 
 * for a particular purpose.
 * 
 * This code has been developed by the NCEP-SIB for use in the AWIPS2 system.
 * 
 **/
package gov.noaa.nws.ncep.viz.rtkp.rsc;

import gov.noaa.nws.ncep.viz.resources.INatlCntrsResource;
import gov.noaa.nws.ncep.viz.rtkp.util.RTKpUtil;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.SimpleDateFormat;
import java.util.TimeZone;

import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.RGBColors;
import com.raytheon.uf.viz.core.drawables.IFont;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer     Description
 * ------------ ----------  -----------  --------------------------
 * 08/26/14     #4078       Shova Gurung Initial Creation.
 * 
 * 
 * </pre>
 * 
 * @author sgurung
 * @version 1
 */
public class GeoMagWorldActivityTitleDateResource
        extends
        AbstractVizResource<GeoMagWorldActivityTitleDateResourceData, IMapDescriptor>
        implements INatlCntrsResource, PropertyChangeListener {

    private SimpleDateFormat topLabelSdf = new SimpleDateFormat(
            "dd-MMM-yy HH:mm:ss 'UTC'");

    private IFont font = null;

    private IGraphicsTarget currTarget = null;

    private PaintProperties currPaintProps = null;

    public GeoMagWorldActivityTitleDateResource(
            GeoMagWorldActivityTitleDateResourceData resourceData,
            LoadProperties loadProperties) {
        super(resourceData, loadProperties);
        rscData = resourceData;
    }

    GeoMagWorldActivityTitleDateResourceData rscData;

    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        topLabelSdf.setTimeZone(TimeZone.getTimeZone("GMT"));
    }

    @Override
    protected void paintInternal(IGraphicsTarget target,
            PaintProperties paintProps) throws VizException {
        currTarget = target;
        currPaintProps = paintProps;

        if (font == null) {
            font = target.initializeFont(rscData.getFontName(),
                    1.5f * rscData.getFontSize(), null);
            if (font == null) {
                font = target.initializeFont(target.getDefaultFont()
                        .getFontName(), 1.5f * rscData.getFontSize(), null);
            }
            font.setScaleFont(false);
            font.setSmoothing(false);
        }

        currTarget.clearClippingPlane();

        IExtent pixExtents = paintProps.getView().getExtent();
        double pixExtentsMinX = pixExtents.getMinX();
        double pixExtentsMaxX = pixExtents.getMaxX();
        double pixExtentsMinY = pixExtents.getMinY();
        double pixExtentsMaxY = pixExtents.getMaxY();
        double pixExtentsWidth = pixExtents.getWidth();
        double pixExtentsHeight = pixExtents.getHeight();

        double minX = 0, maxX = 0;
        double minY = 0, maxY = 0;

        double xScaleFactor = pixExtentsWidth
                / (double) paintProps.getCanvasBounds().width;
        double yScaleFactor = pixExtentsHeight
                / (double) paintProps.getCanvasBounds().height;

        minX = pixExtentsMinX + pixExtentsWidth * 0.005;
        minY = pixExtentsMinY + pixExtentsHeight * 0.05;

        double yMargin = 2 * yScaleFactor;

        double textX = minX;
        double textY = minY - yMargin * 3;

        currTarget.drawString(font, rscData.getTitle(), textX, textY, 0,
                IGraphicsTarget.TextStyle.NORMAL,
                RGBColors.getRGBColor("white"), HorizontalAlignment.LEFT, 0.0);

        maxX = pixExtentsMaxX - pixExtentsWidth * 0.005;
        minY = pixExtentsMinY + pixExtentsHeight * 0.05;

        textX = maxX;
        textY = minY - yMargin * 3;

        currTarget.drawString(font,
                topLabelSdf.format(RTKpUtil.getCurrentTime()), textX, textY, 0,
                IGraphicsTarget.TextStyle.NORMAL,
                RGBColors.getRGBColor("white"), HorizontalAlignment.RIGHT, 0.0);

        currTarget.setupClippingPlane(currPaintProps.getClippingPane());
    }

    @Override
    protected void disposeInternal() {
        if (font != null) {
            font.dispose();
            font = null;
        }
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {

    }

    @Override
    public void resourceAttrsModified() {

    }

}
