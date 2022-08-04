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
package com.raytheon.uf.common.dataplugin.radar.level3;

import java.util.Date;

import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Object representation of the alert message radar product(productCode 9).
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Aug 04, 2011           mnash       Initial creation
 * Jun 04, 2014  3232     bsteffen    Remove ISerializableObject
 * Dec 26, 2014  ASM#632  dhuffman    Edited AlertCategory to conform with
 *                                    Table IV of the "ICD for the RPG to Class 1 User"
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class AlertMessage {
    // Table IV in document: Interface Control Document for the RPG to Class 1 User
    public enum AlertCategory {
        UNUSED_Zero, 
        GRID_Velocity, 
        GRID_Composite_Reflectivity, 
        GRID_Echo_Tops, 
        GRID_Severe_Weather_Probability, 
        GRID_Spare, 
        GRID_Vertically_Integrated_Liquid, 
        VOLUME_Velocity_Azimuth_Display, 
        VOLUME_Maximum_Size_Hail, 
        VOLUME_Spare, 
        VOLUME_Tornado_Vortex_Signature, 
        VOLUME_Maximum_Storm_Reflectivity, 
        VOLUME_Probability_of_Hail, 
        VOLUME_Probability_of_Severe_Hail,
        VOLUME_Storm_Top, 
        VOLUME_Maximum_1_Hour_Rainfall_Accumulation, 
        VOLUME_MDA_Strength_Rank,
        UNUSED_Spare17,
        UNUSED_Spare18,
        UNUSED_Spare19,
        UNUSED_Spare20,
        UNUSED_Spare21,
        UNUSED_Spare22,
        UNUSED_Spare23,
        UNUSED_Spare24,
        FORECAST_Maximum_Hail_Size, 
        FORECAST_Spare, 
        FORECAST_Tornado_Vortex_Signature, 
        FORECAST_Maximum_Storm_Reflectivity, 
        FORECAST_Probability_of_Hail, 
        FORECAST_Probability_of_Severe_Hail, 
        FORECAST_Storm_Top, 
        FORECAST_MDA_Strength_Rank,
        UNUSED_Spare33,
        UNUSED_Spare34,
        UNUSED_Spare35,
        UNUSED_Spare36,
        UNUSED_Spare37,
        UNUSED_Spare38,
        UNUSED_Spare39,
        UNUSED_Spare40,
        UNUSED_Spare41
    }

    @DynamicSerializeElement
    private int status;

    @DynamicSerializeElement
    private int alertAreaNum;

    @DynamicSerializeElement
    private int alertCategory;

    @DynamicSerializeElement
    private int thresholdCode;

    @DynamicSerializeElement
    private int thresholdValue;

    @DynamicSerializeElement
    private int exceedingValue;

    @DynamicSerializeElement
    private int gridBoxAz;

    @DynamicSerializeElement
    private int gridBoxRange;

    @DynamicSerializeElement
    private String stormId;

    @DynamicSerializeElement
    private int volScan;

    @DynamicSerializeElement
    private Date volScanDate;

    /**
     * @return the status
     */
    public int getStatus() {
        return status;
    }

    /**
     * @param status
     *            the status to set
     */
    public void setStatus(int status) {
        this.status = status;
    }

    /**
     * @return the alertAreaNum
     */
    public int getAlertAreaNum() {
        return alertAreaNum;
    }

    /**
     * @param alertAreaNum
     *            the alertAreaNum to set
     */
    public void setAlertAreaNum(int alertAreaNum) {
        this.alertAreaNum = alertAreaNum;
    }

    /**
     * @return the alertCategory
     */
    public int getAlertCategory() {
        return alertCategory;
    }

    /**
     * @param alertCategory
     *            the alertCategory to set
     */
    public void setAlertCategory(int alertCategory) {
        this.alertCategory = alertCategory;
    }

    /**
     * @return the thresholdCode
     */
    public int getThresholdCode() {
        return thresholdCode;
    }

    /**
     * @param thresholdCode
     *            the thresholdCode to set
     */
    public void setThresholdCode(int thresholdCode) {
        this.thresholdCode = thresholdCode;
    }

    /**
     * @return the thresholdValue
     */
    public int getThresholdValue() {
        return thresholdValue;
    }

    /**
     * @param thresholdValue
     *            the thresholdValue to set
     */
    public void setThresholdValue(int thresholdValue) {
        this.thresholdValue = thresholdValue;
    }

    /**
     * @return the exceedingValue
     */
    public int getExceedingValue() {
        return exceedingValue;
    }

    /**
     * @param exceedingValue
     *            the exceedingValue to set
     */
    public void setExceedingValue(int exceedingValue) {
        this.exceedingValue = exceedingValue;
    }

    /**
     * @return the gridBoxAz
     */
    public int getGridBoxAz() {
        return gridBoxAz;
    }

    /**
     * @param gridBoxAz
     *            the gridBoxAz to set
     */
    public void setGridBoxAz(int gridBoxAz) {
        this.gridBoxAz = gridBoxAz;
    }

    /**
     * @return the gridBoxRange
     */
    public int getGridBoxRange() {
        return gridBoxRange;
    }

    /**
     * @param gridBoxRange
     *            the gridBoxRange to set
     */
    public void setGridBoxRange(int gridBoxRange) {
        this.gridBoxRange = gridBoxRange;
    }

    /**
     * @return the stormId
     */
    public String getStormId() {
        return stormId;
    }

    /**
     * @param stormId
     *            the stormId to set
     */
    public void setStormId(String stormId) {
        this.stormId = stormId;
    }

    /**
     * @return the volScan
     */
    public int getVolScan() {
        return volScan;
    }

    /**
     * @param volScan
     *            the volScan to set
     */
    public void setVolScan(int volScan) {
        this.volScan = volScan;
    }

    /**
     * @return the volScanDate
     */
    public Date getVolScanDate() {
        return volScanDate;
    }

    /**
     * @param volScanDate
     *            the volScanDate to set
     */
    public void setVolScanDate(Date volScanDate) {
        this.volScanDate = volScanDate;
    }
}
