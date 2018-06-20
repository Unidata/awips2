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
package com.raytheon.uf.common.monitor.scan.config;

/**
 * 
 * Storm Cell configuration class.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 3, 2009  #3039      lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */
public class StormCellConfig {
    /*
     * Storm cell configuration data.
     */
    private boolean symsCircleHigh = false;

    private boolean symsCircleMid = false;

    private boolean symsCircleLow = false;

    private boolean pastTracks = false;

    private boolean futureTracks = false;

    private boolean symsArrowHigh = false;

    private boolean symsArrowMid = false;

    private boolean symsArrowLow = false;

    private boolean symsIdsHigh = false;

    private boolean symsIdsMid = false;

    private boolean symsIdsLow = false;

    private int minRadius = 3;

    private int maxRadius = 7;

    private String radVar;

    private double radLow = Double.NaN;

    private double radHigh = Double.NaN;

    private boolean arrowMode = false;

    private int arrowConversion = 1;

    private String attrName;

    private double upperVal = Double.NaN;

    private double midVal = Double.NaN;

    private double lowerVal = Double.NaN;

    private boolean linkToFrame = false;

    /**
     * 
     * @param symsCircleHigh
     * @param symsCircleMid
     * @param symsCircleLow
     * @param pastTracks
     * @param futureTracks
     * @param symsArrow
     * @param symsIds
     * @param minRadius
     * @param maxRadius
     * @param radVar
     * @param radLow
     * @param radHigh
     * @param arrowMode
     * @param arrowConversion
     * @param attrName
     * @param upperVal
     * @param midVal
     * @param lowerVal
     * @param linkToFrame
     */
    public StormCellConfig(boolean symsCircleHigh, boolean symsCircleMid,
            boolean symsCircleLow, boolean pastTracks, boolean futureTracks,
            boolean symsArrowHigh, boolean symsArrowMid, boolean symsArrowLow,
            boolean symsIdsHigh, boolean symsIdsMid, boolean symsIdsLow,
            int minRadius, int maxRadius, String radVar, double radLow,
            double radHigh, boolean arrowMode, int arrowConversion,
            String attrName, double upperVal, double midVal, double lowerVal,
            boolean linkToFrame) {
        this.symsCircleHigh = symsCircleHigh;
        this.symsCircleMid = symsCircleMid;
        this.symsCircleLow = symsCircleLow;
        this.pastTracks = pastTracks;
        this.futureTracks = futureTracks;
        this.symsArrowHigh = symsArrowHigh;
        this.symsArrowMid = symsArrowMid;
        this.symsArrowLow = symsArrowLow;
        this.symsIdsHigh = symsIdsHigh;
        this.symsIdsMid = symsIdsMid;
        this.symsIdsLow = symsIdsLow;
        this.minRadius = minRadius;
        this.maxRadius = maxRadius;
        this.radVar = radVar;
        this.radLow = radLow;
        this.radHigh = radHigh;
        this.arrowMode = arrowMode;
        this.arrowConversion = arrowConversion;
        this.attrName = attrName;
        this.upperVal = upperVal;
        this.midVal = midVal;
        this.lowerVal = lowerVal;
        this.linkToFrame = linkToFrame;
    }

    /**
     * Get Syms Circle option.
     * 
     * @return True/False.
     */
    public boolean getSymsCircleHigh() {
        return symsCircleHigh;
    }

    /**
     * Get Syms Circle option.
     * 
     * @return True/False.
     */
    public boolean getSymsCircleMid() {
        return symsCircleMid;
    }

    /**
     * Get Syms Circle option.
     * 
     * @return True/False.
     */
    public boolean getSymsCircleLow() {
        return symsCircleLow;
    }

    /**
     * past tracks
     * 
     * @return True/False.
     */
    public boolean getPastTracks() {
        return pastTracks;
    }

    /**
     * past tracks
     * 
     * @return True/False.
     */
    public boolean getFutureTracks() {
        return futureTracks;
    }

    /**
     * Get Syms arrow option.
     * 
     * @return True/False.
     */
    public boolean getSymsArrowHigh() {
        return symsArrowHigh;
    }

    /**
     * Get Syms arrow option.
     * 
     * @return True/False.
     */
    public boolean getSymsArrowMid() {
        return symsArrowMid;
    }

    /**
     * Get Syms arrow option.
     * 
     * @return True/False.
     */
    public boolean getSymsArrowLow() {
        return symsArrowLow;
    }

    /**
     * Get Syms ID option.
     * 
     * @return True/False.
     */
    public boolean getSymsIdsHigh() {
        return symsIdsHigh;
    }

    /**
     * Get Syms ID option.
     * 
     * @return True/False.
     */
    public boolean getSymsIdsMid() {
        return symsIdsMid;
    }

    /**
     * Get Syms ID option.
     * 
     * @return True/False.
     */
    public boolean getSymsIdsLow() {
        return symsIdsLow;
    }

    /**
     * Get the minimum radius.
     * 
     * @return The minimum radius.
     */
    public int getMinRadius() {
        return minRadius;
    }

    /**
     * Get the maximum radius.
     * 
     * @return The maximum radius.
     */
    public int getMaxRadius() {
        return maxRadius;
    }

    /**
     * Get the radius interpolation.
     * 
     * @return The radius interpolation.
     */
    public String getRadVar() {
        return radVar;
    }

    /**
     * Get the radius lower value.
     * 
     * @return The radius lower value.
     */
    public double getRadLow() {
        return radLow;
    }

    /**
     * Get the radius high value.
     * 
     * @return The radius high value.
     */
    public double getRadHigh() {
        return radHigh;
    }

    /**
     * Get the arrow mode option.
     * 
     * @return True/False.
     */
    public boolean getArrowMode() {
        return arrowMode;
    }

    /**
     * Get the arrow conversion value.
     * 
     * @return The arrow conversion value.
     */
    public int getArrowConversion() {
        return arrowConversion;
    }

    /**
     * Get the attribute name.
     * 
     * @return The attribute name.
     */
    public String getAttrName() {
        return attrName;
    }

    /**
     * Get the upper value.
     * 
     * @return The upper value.
     */
    public double getUpperVal() {
        return upperVal;
    }

    /**
     * Get the mid value.
     * 
     * @return The mid value.
     */
    public double getMidVal() {
        return midVal;
    }

    /**
     * Get the lower value.
     * 
     * @return The lower value.
     */
    public double getLowerVal() {
        return lowerVal;
    }

    /**
     * Set the link to frame option.
     * 
     * @param linkToFrame
     *            True to link the table to the display, false otherwise.
     */
    public void setLinkToFrame(boolean linkToFrame) {
        this.linkToFrame = linkToFrame;
    }

    /**
     * Get the link to frame option.
     * 
     * @return True to link the table to the display, false otherwise.
     */
    public boolean isLinkToFrame() {
        return linkToFrame;
    }
}
