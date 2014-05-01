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
package com.raytheon.viz.core.contours.util;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * 
 * Configuration options to control how vectors are rendered when using
 * {@link VectorGraphicsRenderable}. These config options are applied
 * immediately when you call
 * {@link VectorGraphicsRenderable#paintBarb(Coordinate, double, double)},
 * {@link VectorGraphicsRenderable#paintArrow(Coordinate, double, double)}, or
 * {@link VectorGraphicsRenderable#paintDualArrow(Coordinate, double, double)}.
 * This means that multiple vectors can be rendered with different styles using
 * the same renderable. For example the following snippet will render two barbs,
 * one with clockwise barbs and one with counterclockwise barbs using the same
 * renderable.
 * 
 * <pre>
 * {
 *     &#064;code
 *     VectorGraphicsRenderable renderable = new VectorGraphicsRenderable(
 *             descriptor, target);
 *     renderable.getConfig().setBarbRotationClockwise();
 *     renderable.paintBarb(location1, magnitude1, direction1);
 *     renderable.getConfig().setBarbRotationCounterClockwise();
 *     renderable.paintBarb(location2, magnitude2, direction2);
 *     renderable.paint(target);
 * }
 * </pre>
 * 
 * This class contains configuration for wind barbs, arrows, and calm circles.
 * The barb options apply only when paintBarb is used. The arrow options apply
 * to both paintArrow and paintDualArrow. The calm circles options apply to both
 * paintBarb and paintArrow.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Sep 23, 2013  2363     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 * @See {@link VectorGraphicsRenderable}
 */
public class VectorGraphicsConfig {

    /**
     * Interface for providing complex arrow scaling algorithms. For most simple
     * parameters linear scaling works well enough, however some data is easier
     * to understand by using complex logarithmic scaling.
     */
    public static interface IArrowScaler {

        /**
         * This method should calculate the length of an arrow(in pixels) for a
         * single vector.
         * 
         * @param magnitude
         *            the magnitude of a single vector.
         * @return the length of the arrow(in pixels).
         */
        public double scale(double magnitude);
    }

    /**
     * Simple linear scaling for arrows that multiplies the magnitude by a
     * constant scaleFactor.
     */
    public static class LinearArrowScaler implements IArrowScaler {

        protected final double scaleFactor;

        public LinearArrowScaler(double scaleFactor) {
            this.scaleFactor = scaleFactor;
        }

        @Override
        public double scale(double magnitude) {
            return magnitude * scaleFactor;
        }

    }

    protected double baseSize;

    protected double sizeScaler;

    protected double offsetRatio;

    protected double minimumMagnitude;

    protected double barbRotationRadians;

    protected double barbLengthRatio;

    protected double barbSpacingRatio;

    protected boolean barbFillFiftyTriangle;

    protected double calmCircleMaximumMagnitude;

    protected double calmCircleSizeRatio;

    protected double arrowHeadSizeRatio;

    protected double arrowHeadStaffRatio;

    protected IArrowScaler arrowScaler;

    /**
     * Creates a new config with <em>reasonable</em> defalt values
     */
    public VectorGraphicsConfig() {
        setBaseSize(32.0);
        setOffsetRatio(0.0);
        setMinimumMagnitude(2.5);
        setBarbRotationDegrees(75);
        setBarbLengthRatio(0.3);
        setBarbSpacingRatio(0.105);
        setBarbFillFiftyTriangle(false);
        setCalmCircleMaximumMagnitude(2.5);
        setCalmCircleSizeRatio(0.075);
        setArrowHeadSizeRatio(0.1875);
        setLinearArrowScaleFactor(1.0);
    }

    /**
     * Construct a new instance with all fields copied from the other object.
     * 
     * @param other
     *            an existing config to copy.
     */
    public VectorGraphicsConfig(VectorGraphicsConfig other) {
        this.baseSize = other.baseSize;
        this.sizeScaler = other.sizeScaler;
        this.offsetRatio = other.offsetRatio;
        this.minimumMagnitude = other.minimumMagnitude;
        this.barbRotationRadians = other.barbRotationRadians;
        this.barbLengthRatio = other.barbLengthRatio;
        this.barbSpacingRatio = other.barbSpacingRatio;
        this.barbFillFiftyTriangle = other.barbFillFiftyTriangle;
        this.calmCircleMaximumMagnitude = other.calmCircleMaximumMagnitude;
        this.calmCircleSizeRatio = other.calmCircleSizeRatio;
        this.arrowHeadSizeRatio = other.arrowHeadSizeRatio;
        this.arrowHeadStaffRatio = other.arrowHeadStaffRatio;
        this.arrowScaler = other.arrowScaler;
    }

    /**
     * Set the base size. Base size combined with size scaler is used for barb
     * length and as the base measurement for all ratios when rendering wind
     * barbs or arrows.
     * 
     * @param size
     *            the base size of the barb in descriptor grid pixels
     * @see #setSizeScaler(double)
     * @see #getScaledSize()
     */
    public void setBaseSize(double size) {
        this.baseSize = size;
    }

    /**
     * Set the linear size scaler. All rendered lines are linearly scaled by
     * this constant. This is used for implementing magnification and/or scaling
     * the descriptor grid into screen pixels. Scale factor should by a positive
     * value. A value of 1.0 is a no-op and all lines will be the default size.
     * Values less than 1 will shrink the lines and values greater than 1 cause
     * them to grow. Although the same renderings could be achieved by
     * manipulating base size directly, using the size scaler provides a
     * convenient way to have a fixed base size that needs to be frequently
     * modified by a changing scale factor.
     * 
     * @param sizeScaler
     *            a factor to scale baseSize.
     * @see #setBaseSize(double)
     * @see #getScaledSize()
     */
    public void setSizeScaler(double sizeScaler) {
        this.sizeScaler = sizeScaler;
    }

    /**
     * Set a ratio describing the distance from the plot location that a barb or
     * arrow will be rendered relative to scaled size. This value will be
     * multiplied by the scaled size do determine the actual offset distance.
     * For most applications a value between 0 and 1 is reasonable. A negative
     * value will cause the barb to overlap the plot location rather than
     * extending from the plot location. Using excessively large values will
     * cause the barb to be very far away from the plot location and is not
     * advised. This offset can be used to leave space for other symbols that
     * are being rendered at plot location.
     * 
     * @param offsetRatio
     *            distance from plot location to start rendering.
     * @see #getScaledSize()
     */
    public void setOffsetRatio(double offsetRatio) {
        this.offsetRatio = offsetRatio;
    }

    /**
     * Set a minimum magnitude for rendering. Any calls to
     * {@link VectorGraphicsRenderable} that paint a vector with magnitude less
     * than this value will not be rendered. A calm circle will still be
     * rendered if the value is less than the calm circle maximum magnitude.
     * 
     * @param minimumMagnitude
     *            the minimum magnitude that will be rendered.
     * @see #setCalmCircleMaximumMagnitude(double)
     * @see #alwaysIncludeVector()
     */
    public void setMinimumMagnitude(double minimumMagnitude) {
        this.minimumMagnitude = minimumMagnitude;
    }

    /**
     * Shortcut method to set this configuration to always draw vectors
     * regardless of the magnitude. This is equivalent to
     * <code>setMinimumMagnitude(0.0)</code>
     */
    public void alwaysIncludeVector() {
        setMinimumMagnitude(0.0);
    }

    /**
     * Set the rotation in radians of the barbs from the shaft.
     * 
     * @param barbRotationRadians
     *            rotation in radians.
     */
    public void setBarbRotationRadians(double barbRotationRadians) {
        this.barbRotationRadians = barbRotationRadians;
    }

    /**
     * Set the rotation in degrees of the barbs from the shaft. Internally it
     * will be converted to radians. This is equivalent to
     * <code>setBarbRotationRadians(Math.toRadians(barbRotationDegrees))</code>
     * 
     * @param barbRotationDegrees
     *            rotation in degrees.
     * @see #setBarbRotationRadians(double)
     */
    public void setBarbRotationDegrees(double barbRotationDegrees) {
        setBarbRotationRadians(Math.toRadians(barbRotationDegrees));
    }

    /**
     * Shortcut method to set the barb rotation angle so that barbs are
     * clockwise. Since any positive angle will be clockwise this simply sets
     * the rotation to the absolute value of its current value. The same result
     * can be achieved by using #setBarbRotationRadians(double) directly.
     * 
     * @see #setBarbRotationRadians(double)
     */
    public void setBarbRotationClockwise() {
        setBarbRotationRadians(Math.abs(barbRotationRadians));
    }

    /**
     * Shortcut method to set the barb rotation angle so that barbs are counter
     * clockwise. Since any negative angle will be counter clockwise this simply
     * sets the rotation to the negative absolute value of its current value.
     * The same result can be achieved by using #setBarbRotationRadians(double)
     * directly.
     * 
     * @see #setBarbRotationRadians(double)
     */
    public void setBarbRotationCounterClockwise() {
        setBarbRotationRadians(-1 * Math.abs(barbRotationRadians));
    }

    /**
     * Set a ratio describing the length of the barb relative to scaled size.
     * This should be a value between 0 and 1 and will be multiplied by the
     * scaled size do determine the actual barb length. The half barb will
     * always be half of this distance. Values too close to 0 will be difficult
     * to see and values too close to 1 will make it hard to distinguish the
     * barb from the shaft.
     * 
     * @param barbLengthRatio
     *            length of the barbs
     * @see #getScaledSize()
     */
    public void setBarbLengthRatio(double barbLengthRatio) {
        if (barbLengthRatio <= 0 || barbLengthRatio >= 1) {
            throw new IllegalArgumentException(
                    "Barb length Ratio must be between 0 and 1");
        }
        this.barbLengthRatio = barbLengthRatio;
    }

    /**
     * Set a ratio describing the distance between barbs along the shaft
     * relative to the scaled size. This should be a value between 0 and 1 and
     * will be multiplied by the scaled size do determine the actual barb
     * spacing. In practice this value should be much smaller than 1 so that
     * high wind speeds will fit.
     * 
     * @param barbSpacingRatio
     *            distance between barbs along the shaft
     * @see #getScaledSize()
     */
    public void setBarbSpacingRatio(double barbSpacingRatio) {
        if (barbSpacingRatio <= 0 || barbSpacingRatio >= 1) {
            throw new IllegalArgumentException(
                    "Barb Spacing Ratio must be between 0 and 1");
        }
        this.barbSpacingRatio = barbSpacingRatio;
    }

    /**
     * Boolean value to indicate if winds over fifty should use a filled
     * triangle or just an outline.
     * 
     * @param barbFillFiftyTriangle
     *            true to render a filled triangle, false to render an outline
     *            only.
     * @see #setBarbFillFiftyTriangle();
     * @see #setBarbOutlineFiftyTriangle();
     */
    public void setBarbFillFiftyTriangle(boolean barbFillFiftyTriangle) {
        this.barbFillFiftyTriangle = barbFillFiftyTriangle;
    }

    /**
     * Set the maximum magnitude for which to draw a calm circle. If this value
     * is the same as minimum magnitude then for each paintArrow or paintBarb
     * method either a vector or calm circle will be drawn. If this is larger
     * than minimum magnitude then some or all vectors will contain both an
     * arrow/barb and a calm circle and if it is smaller than minimum magnitude
     * than some vectors might not be drawn at all.
     * 
     * @param calmCircleMaximumMagnitude
     *            the maximum vector magnitude for which calm circles should be
     *            rendered.
     * @see #setMinimumMagnitude(double)
     * @see #disableCalmCircle()
     * @see #alwaysIncludeCalmCircle()
     */
    public void setCalmCircleMaximumMagnitude(double calmCircleMaximumMagnitude) {
        this.calmCircleMaximumMagnitude = calmCircleMaximumMagnitude;
    }

    /**
     * Shortcut method to disable drawing calm circles. This is equivalent to
     * <code>setCalmCircleMaximumMagnitude(0.0)</code>
     * 
     * @see #setCalmCircleMaximumMagnitude(double)
     */
    public void disableCalmCircle() {
        setCalmCircleMaximumMagnitude(0.0);
    }

    /**
     * Shortcut method to alwaysthe vector is painted the size scaler is used to
     * change the size of all elements of the vector by draw calm circles
     * regardless of the magnitude. This is equivalent to
     * <code>setCalmCircleMaximumMagnitude(Double.POSITIVE_INFINITY)</code>
     * 
     * @see #setCalmCircleMaximumMagnitude(double)
     */
    public void alwaysIncludeCalmCircle() {
        setCalmCircleMaximumMagnitude(Double.POSITIVE_INFINITY);
    }

    /**
     * Set a ratio describing the size of the calm circle relative to scaled
     * size. This should be positive number and will be multiplied by the scaled
     * size do determine the actual calm circle size. Usually the calm circle is
     * significantly smaller than the wind barb so the value should be
     * significantly less than 1.
     * 
     * @param calmCircleSizeRatio
     *            size of the calm circle.
     * @see #getScaledSize()
     */
    public void setCalmCircleSizeRatio(double calmCircleSizeRatio) {
        if (calmCircleSizeRatio <= 0) {
            throw new IllegalArgumentException(
                    "Calm circle size ratio must be a positive value");
        }
        this.calmCircleSizeRatio = calmCircleSizeRatio;
    }

    /**
     * Set a ratio describing the size of an arrow head relative to scaled size.
     * This should be a value between 0 and 1 and will be multiplied by the
     * scaled size do determine the actual arrow head size. This can be used for
     * constant size arrow heads, if the arrow head should change size when
     * magnitude changes #setArrowHeadStaffRatio(double) should be used instead.
     * 
     * @param arrowHeadSizeRatio
     *            size of the arrow head.
     * @see #getScaledSize()
     * @see #setArrowHeadStaffRatio(double)
     */
    public void setArrowHeadSizeRatio(double arrowHeadSizeRatio) {
        if (arrowHeadSizeRatio <= 0 || arrowHeadSizeRatio >= 1) {
            throw new IllegalArgumentException(
                    "Arrow Head Size Ratio must be between 0 and 1");
        }
        this.arrowHeadSizeRatio = arrowHeadSizeRatio;
        this.arrowHeadStaffRatio = 0.0;
    }

    /**
     * Set a ratio describing the size of an arrow head relative to the scaled
     * length of the arrow staff. This should be a value between 0 and 1 and
     * will be multiplied by the scaled staff size do determine the actual arrow
     * head size. This can be used to render arrow heads with a size dependent
     * on the magnitude, if the arrow head should be constant size then
     * #setArrowHeadSizeRatio(double) should be used instead.
     * 
     * @param arrowHeadSizeRatio
     *            size of the arrow head.
     * @see #getScaledSize()
     * @see #setArrowHeadSizeRatio(double)
     */
    public void setArrowHeadStaffRatio(double arrowHeadStaffRatio) {
        if (arrowHeadSizeRatio <= 0 || arrowHeadSizeRatio >= 1) {
            throw new IllegalArgumentException(
                    "Arrow Head Staff Ratio must be between 0 and 1");
        }
        this.arrowHeadStaffRatio = arrowHeadStaffRatio;
        this.arrowHeadSizeRatio = 0.0;
    }

    /**
     * Set a custom arrow scaling algorithm for rendering arrows. After custom
     * scaling is applied the size scaler will also be used to further scale the
     * arrow.
     * 
     * @param arrowScaler
     *            custom scaling algorithm
     * @see IArrowScaler
     * @see #setSizeScaler(double)
     */
    public void setArrowScaler(IArrowScaler arrowScaler) {
        this.arrowScaler = arrowScaler;
    }

    /**
     * Shortcut method to set the arrow scaler to a {@link LinearArrowScaler}.
     * This is equivalent to
     * <code>setArrowScaler(new LinearArrowScaler(scaleFactor))</code>
     * 
     * @param scaleFactor
     *            linear scale factor for arrow sizing.
     */
    public void setLinearArrowScaleFactor(double scaleFactor) {
        setArrowScaler(new LinearArrowScaler(scaleFactor));
    }

    /**
     * @return the base size in descriptor pixels
     * @see #setBaseSize(double)
     */
    public double getBaseSize() {
        return baseSize;
    }

    /**
     * @return the size scaler
     * @see #setSizeScaler(double)
     * @see #getScaledSize()
     */
    public double getSizeScaler() {
        return sizeScaler;
    }

    /**
     * Get the scaled size. This is the baseSize multiplied by the size scaler.
     * When a barb is painted this will be the length of the shaft. All size
     * ratios are multiplied by this value to get the actual size of the
     * rendered lines in descriptor pixels.
     * 
     * @return the scaled size.
     * @see #setBaseSize(double)
     * @see #setSizeScaler(double)
     */
    public double getScaledSize() {
        return baseSize * sizeScaler;
    }

    /**
     * @return the offset ratio
     * @see #setOffsetRatio(double)
     */
    public double getOffsetRatio() {
        return offsetRatio;
    }

    /**
     * @return the minimum magnitude
     * @see #setMinimumMagnitude(double)
     */
    public double getMinimumMagnitude() {
        return minimumMagnitude;
    }

    /**
     * @return the barb rotation angle in radians.
     * @see #setBarbRotationRadians(double)
     */
    public double getBarbRotationRadians() {
        return barbRotationRadians;
    }

    /**
     * @return the barb rotation angle in degrees.
     * @see #setBarbRotationDegrees(double)
     */
    public double getBarbRotationDegrees() {
        return Math.toDegrees(barbRotationRadians);
    }

    /**
     * @return the barb length ratio
     * @see #setBarbLengthRatio(double)
     */
    public double getBarbLengthRatio() {
        return barbLengthRatio;
    }

    /**
     * @return the barb spacing ratio
     * @see #setBarbSpacingRatio(double)
     */
    public double getBarbSpacingRatio() {
        return barbSpacingRatio;
    }

    /**
     * @return the barbFillFiftyTriangle
     * @see #setBarbFillFiftyTriangle()
     */
    public boolean isBarbFillFiftyTriangle() {
        return barbFillFiftyTriangle;
    }

    /**
     * @return the calmCircleMaximumMagnitude
     * @see #setCalmCircleMaximumMagnitude(double)
     */
    public double getCalmCircleMaximumMagnitude() {
        return calmCircleMaximumMagnitude;
    }

    /**
     * @return the calmCircleSizeRatio
     * @see #setCalmCircleSizeRatio(double)
     */
    public double getCalmCircleSizeRatio() {
        return calmCircleSizeRatio;
    }

    /**
     * @return the arrowHeadSizeRatio
     * @see #setArrowHeadSizeRatio(double)
     */
    public double getArrowHeadSizeRatio() {
        return arrowHeadSizeRatio;
    }

    /**
     * @return the arrowHeadStaffRatio
     * @see #setArrowHeadStaffRatio(double)
     */
    public double getArrowHeadStaffRatio() {
        return arrowHeadStaffRatio;
    }

    /**
     * @return the arrowScaler
     * @see #setArrowScaler(IArrowScaler)
     */
    public IArrowScaler getArrowScaler() {
        return arrowScaler;
    }

    @Override
    public VectorGraphicsConfig clone() {
        return new VectorGraphicsConfig(this);
    }

}
