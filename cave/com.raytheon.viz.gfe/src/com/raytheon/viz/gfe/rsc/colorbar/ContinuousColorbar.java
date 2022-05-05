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
package com.raytheon.viz.gfe.rsc.colorbar;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.common.colormap.prefs.ColorMapParameters;
import com.raytheon.uf.common.dataplugin.gfe.db.objects.GridParmInfo.GridType;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.viz.core.DrawableColorMap;
import com.raytheon.uf.viz.core.DrawableLine;
import com.raytheon.uf.viz.core.DrawableString;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.IGraphicsTarget.HorizontalAlignment;
import com.raytheon.uf.viz.core.IGraphicsTarget.TextStyle;
import com.raytheon.uf.viz.core.IGraphicsTarget.VerticalAlignment;
import com.raytheon.uf.viz.core.PixelExtent;
import com.raytheon.uf.viz.core.drawables.PaintProperties;
import com.raytheon.uf.viz.core.drawables.ResourcePair;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorMapCapability;
import com.raytheon.viz.gfe.GFEPreference;
import com.raytheon.viz.gfe.core.DataManager;
import com.raytheon.viz.gfe.core.ISpatialDisplayManager;
import com.raytheon.viz.gfe.core.parm.Parm;
import com.raytheon.viz.gfe.core.wxvalue.ScalarWxValue;
import com.raytheon.viz.gfe.core.wxvalue.VectorWxValue;
import com.raytheon.viz.gfe.core.wxvalue.WxValue;
import com.raytheon.viz.gfe.rsc.GFEResource;

/**
 * Implements a colorbar for continuous (scalar and vector) elements
 *
 * <pre>
 * SOFTWARE HISTORY
 *
 * Date          Ticket#  Engineer  Description
 * ------------- -------- --------- --------------------------------------------
 * Mar 26, 2008           chammack  Initial Creation.
 * Apr 13, 2009  2092     njensen   Support for custom labels
 * Aug 20, 2012  1083     randerso  Fixed user defined labels
 * Feb 14, 2013  1616     bsteffen  Add option for interpolation of colormap
 *                                  parameters, disable colormap interpolation
 *                                  by default.
 * Feb 11, 2014  2788     randerso  Fixed infinite loop in
 *                                  computeIntervalAndPrecision when pmax < pmin
 * Aug 14, 2014  3523     mapeters  Updated deprecated {@link
 *                                  DrawableString#textStyle} assignments.
 * Nov 04, 2014  3557     randerso  Fixed NPE when parm is removed from display
 * Jan 24, 2018  7153     randerso  Changes to allow new GFE config file to be
 *                                  selected when perspective is re-opened.
 * Jun 07, 2018  7310     mapeters  Support colorMapMin > colorMapMax
 *
 * </pre>
 *
 * @author chammack
 */
public class ContinuousColorbar implements IColorBarDisplay {

    private DataManager dManager;

    private Parm parm;

    private GFEColorbarResource colorbarResource;

    private RGB seColorBarTickColor = new RGB(255, 255, 255);

    private RGB seColorBarTextColor = new RGB(255, 255, 255);

    private RGB seColorBarFgPickupColor = new RGB(255, 255, 255);

    private RGB seColorBarFgWxPickupColor = new RGB(255, 255, 255);

    private RGB seColorBarBgPickupColor;

    private RGB seColorBarBgWxPickupColor;

    private transient IUFStatusHandler log;

    /**
     * @param dataManager
     * @param parm
     * @param colorbarResource
     */
    public ContinuousColorbar(DataManager dataManager, Parm parm,
            GFEColorbarResource colorbarResource) {
        dManager = dataManager;
        this.parm = parm;
        this.colorbarResource = colorbarResource;
        this.log = UFStatus.getHandler(ContinuousColorbar.class);

    }

    @Override
    public void dispose() {

    }

    @Override
    public void paint(IGraphicsTarget target, PaintProperties paintProps)
            throws VizException {

        ISpatialDisplayManager spatialDisplayManager = dManager
                .getSpatialDisplayManager();

        ResourcePair rscPair = spatialDisplayManager.getResourcePair(parm);
        if (rscPair == null) {
            // spatial display manager deactivated since we got the color map?
            log.debug("Cannot obtain resource pair for "
                    + parm.getParmID().getCompositeName()
                    + ", exiting paint()");
            return;
        }

        GFEResource rsc = (GFEResource) rscPair.getResource();
        ColorMapParameters cmap = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters();

        if (cmap == null) {
            log.debug("Cannot obtain color map, exiting paint()");
            return;
        }

        PixelExtent pe = colorbarResource.getExtent();
        float center = (float) ((pe.getMinY() + pe.getMaxY()) / 2.0);

        DrawableColorMap dcm = new DrawableColorMap(cmap);
        dcm.alpha = 1.0f;
        dcm.extent = pe;
        target.drawColorRamp(dcm);

        float leftLimit = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getColorMapMin();
        float rightLimit = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getColorMapMax();

        float[] labels = GFEPreference.getFloatArray(
                parm.getParmID().getParmName() + "_ColorBarLabels");

        double labelLoc = 0.0f;
        double llx = pe.getMinX();
        double xExtent = pe.getWidth();

        double ratio = (paintProps.getView().getExtent().getWidth()
                / paintProps.getCanvasBounds().width);

        DrawableString dstring = new DrawableString("", seColorBarTextColor);
        dstring.font = colorbarResource.getColorbarScaleFont();
        dstring.horizontalAlignment = HorizontalAlignment.CENTER;
        dstring.verticallAlignment = VerticalAlignment.MIDDLE;

        DrawableLine dline = new DrawableLine();
        dline.basics.color = seColorBarTickColor;
        dline.width = 1.0f;

        if ((labels == null) || (labels.length < 1)) {
            float[] val = computeIntervalAndPrecision(leftLimit, rightLimit,
                    paintProps.getCanvasBounds().width, target);

            float tmpValue = (int) leftLimit;
            float interval = val[0];
            int precision = (int) val[1];

            boolean ascendingValues = leftLimit < rightLimit;
            float limitDiff = Math.abs(rightLimit - leftLimit);
            for (int i = 0; i * interval <= limitDiff; i++) {
                // check to see whether this colorTable item needs to be
                // rendered
                float labelValue;
                if (ascendingValues) {
                    labelValue = leftLimit + (i * interval);
                } else {
                    labelValue = leftLimit - (i * interval);
                }

                // Check to see if value is same as previous unless float....
                if ((tmpValue != (int) labelValue) || (precision > 0)) {
                    String labelText = GFEColorbarResource
                            .formatString(labelValue, precision);

                    labelLoc = getLabelLoc(llx, xExtent, labelValue, leftLimit,
                            rightLimit);

                    if (GFEColorbarResource.isLabelWithin(pe.getMinX(),
                            pe.getMaxX(), labelLoc, 0)) {
                        dline.setCoordinates(labelLoc, pe.getMinY(), 0.0);
                        dline.addPoint(labelLoc, pe.getMaxY(), 0.0);
                        target.drawLine(dline);

                        dstring.setCoordinates(labelLoc, center);
                        dstring.setText(labelText, seColorBarTextColor);
                        target.drawStrings(dstring);
                        tmpValue = (int) labelValue;
                    }
                }
            }
        } else {
            int precision = parm.getGridInfo().getPrecision();
            for (float labelValue : labels) {
                labelLoc = getLabelLoc(llx, xExtent, labelValue, leftLimit,
                        rightLimit);
                if (GFEColorbarResource.isLabelWithin(pe.getMinX(),
                        pe.getMaxX(), labelLoc, 0)) {
                    dline.setCoordinates(labelLoc, pe.getMinY(), 0.0);
                    dline.addPoint(labelLoc, pe.getMaxY(), 0.0);
                    target.drawLine(dline);

                    String s = GFEColorbarResource.formatString(labelValue,
                            precision);
                    dstring.setCoordinates(labelLoc, center);
                    dstring.setText(s, seColorBarTextColor);
                    target.drawStrings(dstring);
                }
            }
        }

        // Draw the pickup value
        WxValue wxv = parm.getParmState().getPickUpValue();
        if ((wxv != null) && ((wxv instanceof ScalarWxValue)
                || (wxv instanceof VectorWxValue))) {
            float floatValue = ((ScalarWxValue) wxv).getValue();
            if (isValueWithinLimits(floatValue, leftLimit, rightLimit)) {
                labelLoc = getLabelLoc(llx, xExtent, floatValue, leftLimit,
                        rightLimit);

                String s = wxv.toString();
                dstring.font = colorbarResource.getPickupValueFont();
                if (GridType.WEATHER == parm.getGridInfo().getGridType()) {
                    dstring.setText(s, seColorBarFgWxPickupColor);
                    dstring.addTextStyle(TextStyle.DROP_SHADOW,
                            seColorBarBgWxPickupColor);
                } else {
                    dstring.setText(s, seColorBarFgPickupColor);
                    dstring.addTextStyle(TextStyle.DROP_SHADOW,
                            seColorBarBgPickupColor);
                }

                double halfWidth = (target.getStringsBounds(dstring).getWidth()
                        * ratio) / 2;

                if ((labelLoc - halfWidth) < pe.getMinX()) {
                    labelLoc = pe.getMinX() + halfWidth;
                } else if ((labelLoc + halfWidth) > pe.getMaxX()) {
                    labelLoc = pe.getMaxX() - halfWidth;
                }
                dstring.setCoordinates(labelLoc,
                        (pe.getMinY() + pe.getMaxY()) / 2.0);
                target.drawStrings(dstring);
            }
        }

    }

    private static boolean isValueWithinLimits(float value, float leftLimit,
            float rightLimit) {
        if (leftLimit < rightLimit) {
            return value >= leftLimit && value <= rightLimit;
        } else {
            return value <= leftLimit && value >= rightLimit;
        }
    }

    private static double getLabelLoc(double llx, double xExtent,
            float labelValue, float leftLimit, float rightLimit) {
        // Works regardless of whether left/right is larger
        float valueOffsetFromLeft = labelValue - leftLimit;
        float valueRange = rightLimit - leftLimit;
        return llx + (valueOffsetFromLeft / valueRange * xExtent);
    }

    private float[] computeIntervalAndPrecision(float leftLimit,
            float rightLimit, int xExtent, IGraphicsTarget target) {

        // multiplier for the possible intervals
        float[] multStep = { 1, 2, 5 };

        // get the difference of the parm min and max values
        float parmExtent = Math.abs(rightLimit - leftLimit);

        // set the return values to "safe" values
        float finterval = 1;
        float precision = 0;
        float labelLength = 100;

        // integer to step down multipliers by factor of 10
        double dec = Math.log10(parmExtent);
        int decadePower = (int) dec + 1;
        double decade = Math.pow(10.0, decadePower);

        // initial values are good if decade < parmExtent
        // loop is infinite if parmExtent is NaN or 0, so avoid it
        if (Float.isNaN(parmExtent) || (parmExtent <= 0.0)
                || (decade < parmExtent)) {
            return new float[] { finterval, precision, labelLength };
        }

        boolean done = false;
        // which of the possible intervals is the closest?
        double interval;
        long noInterval;
        int cPrecision, cLabelLength;

        DrawableString ds = new DrawableString("", null);
        ds.font = colorbarResource.getColorbarScaleFont();
        while (!done) {
            for (int i = 2; i >= 0; i--) {
                // calculate interval
                interval = decade * multStep[i];

                // calculate typical text label
                cPrecision = precisionBasedOnInterval((float) interval);

                String s = GFEColorbarResource.formatString(rightLimit,
                        cPrecision);
                cLabelLength = (int) (target.getStringsBounds(ds, s)
                        .getWidth());
                long noLabels = xExtent / (cLabelLength + 20);
                noInterval = (long) (parmExtent / interval);
                if (noInterval > noLabels) {
                    done = true;
                    break;

                }

                // save the last one in case the exit triggers this time
                finterval = (float) interval;
                precision = cPrecision;
                labelLength = cLabelLength;
            }
            if (done) {
                break;
            } else {
                // try again, after increasing multipliers
                decadePower--;
                decade = Math.pow(10.0, decadePower);
            }
        }

        return new float[] { finterval, precision, labelLength };

    }

    // -- private
    // ----------------------------------------------------------------
    // CBContVisual::precisionBasedOnInterval()
    // Returns the precision for the labels based on the label interval.
    // -- implementation
    // ---------------------------------------------------------
    // ---------------------------------------------------------------------------
    private int precisionBasedOnInterval(float interval) {
        if (interval >= 0.9) {
            return 0;
        } else if (interval >= 0.09) {
            return 1;
        } else if (interval >= 0.009) {
            return 2;
        } else if (interval >= 0.0009) {
            return 3;
        } else if (interval >= 0.00009) {
            return 4;
        } else {
            return 5;
        }
    }

    /**
     * @param seColorBarTickColor
     *            the seColorBarTickColor to set
     */
    public void setSeColorBarTickColor(RGB seColorBarTickColor) {
        this.seColorBarTickColor = seColorBarTickColor;
    }

    /**
     * @param seColorBarTextColor
     *            the seColorBarTextColor to set
     */
    public void setSeColorBarTextColor(RGB seColorBarTextColor) {
        this.seColorBarTextColor = seColorBarTextColor;
    }

    /**
     * @param seColorBarFgPickupColor
     *            the seColorBarFgPickupColor to set
     */
    public void setSeColorBarFgPickupColor(RGB seColorBarFgPickupColor) {
        this.seColorBarFgPickupColor = seColorBarFgPickupColor;
    }

    /**
     * @param seColorBarFgWxPickupColor
     *            the seColorBarFgWxPickupColor to set
     */
    public void setSeColorBarFgWxPickupColor(RGB seColorBarFgWxPickupColor) {
        this.seColorBarFgWxPickupColor = seColorBarFgWxPickupColor;
    }

    /**
     * @param seColorBarBgPickupColor
     *            the seColorBarBgPickupColor to set
     */
    public void setSeColorBarBgPickupColor(RGB seColorBarBgPickupColor) {
        this.seColorBarBgPickupColor = seColorBarBgPickupColor;
    }

    /**
     * @param seColorBarBgWxPickupColor
     *            the seColorBarBgWxPickupColor to set
     */
    public void setSeColorBarBgWxPickupColor(RGB seColorBarBgWxPickupColor) {
        this.seColorBarBgWxPickupColor = seColorBarBgWxPickupColor;
    }

    @Override
    public WxValue getValueAt(double[] coord, int mouseButton) {
        PixelExtent lastExtent = colorbarResource.getExtent();
        float fractionX = (float) ((coord[0] - lastExtent.getMinX())
                / (lastExtent.getMaxX() - lastExtent.getMinX()));
        GFEResource rsc = (GFEResource) dManager.getSpatialDisplayManager()
                .getResourcePair(parm).getResource();
        float min = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getColorMapMin();
        float max = rsc.getCapability(ColorMapCapability.class)
                .getColorMapParameters().getColorMapMax();

        switch (parm.getGridInfo().getGridType()) {
        case SCALAR:
            return new ScalarWxValue(getValueByFraction(min, max, fractionX),
                    parm);
        case VECTOR:
            WxValue previous = parm.getParmState().getPickUpValue();
            float mag = 0.0f;
            float dir = 0.0f;
            if ((previous != null) && (previous instanceof VectorWxValue)) {
                mag = ((VectorWxValue) previous).getMag();
                dir = ((VectorWxValue) previous).getDir();
            }
            if (mouseButton == 1) {
                mag = getValueByFraction(min, max, fractionX);
            } else if (mouseButton == 2) {
                dir = 360 * fractionX;
            }
            return new VectorWxValue(mag, dir, parm);
        default:
            throw new IllegalArgumentException(
                    "getValueAt does not support type: "
                            + parm.getGridInfo().getGridType());
        }
    }

    private static float getValueByFraction(float leftLimit, float rightLimit,
            float fraction) {
        // Works regardless of whether left/right is larger
        return leftLimit + ((rightLimit - leftLimit) * fraction);
    }
}
