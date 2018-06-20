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

package com.raytheon.viz.hydrocommon.data;

/**
 * Class for packaging the gage data in TimeStep mode.
 * 
 * <pre>
 * SOFTWARE HISTOR
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Sep 14, 2015  17978     lbousaidi   Initial Creation  
 * Dec 05, 2015  18357     xwei        Fixed error in opening Timeseries for Timesteps
 * May 03, 2016  5623      bkowal      Cleanup and formatting.
 * </pre>
 * 
 * @author lbousaidi
 * @version 1.0
 */

public class GageDataTimeStep extends GageData {

    private String PP_p = "";

    private String PC_p = "";

    private String Other_p = "";

    private String PP_v = "";

    private String PC_v = "";

    private String Other_v = "";

    /**
     * public constructor
     */
    public GageDataTimeStep() {
        super();

    }

    /**
     * public constructor
     * 
     * @param gage
     */
    public GageDataTimeStep(GageData gage) {
        setLid(gage.getLid());
        setName(gage.getName());
        setElevation(gage.getElevation());
        setValidtime(gage.getValidtime());
        setCoordinate(gage.getCoordinate());
        setDispClass(gage.getDispClass());

        setValue(gage.getValue());
        setThreatIndex(gage.getThreatIndex());

        setPe(gage.getPe());
        setTs(gage.getTs());
        setExtremum(gage.getExtremum());

        setP(gage);
        setV(gage);
    }

    /**
     * Update by a gage
     * 
     * @param gage
     */
    public void update(GageData gage) {
        if (getValue() < gage.getValue()) {
            setValue(gage.getValue());
            setThreatIndex(gage.getThreatIndex());
        }

        addParam(gage);
        addValue(gage);
    }

    /**
     * Get both PP and PC Parameter codes
     */
    public String getPpAndPcParam() {
        return combineString(PP_p, PC_p);
    }

    /**
     * Get both PP and PC Values
     */
    public String getPpAndPcValue() {
        return combineString(PP_v, PC_v);
    }

    /**
     * Get PP Parameter codes
     */
    public String getPpParam() {

        return PP_p;
    }

    /**
     * Get both PP values
     */
    public String getPpValue() {

        return PP_v;
    }

    /**
     * Get PC Parameter codes
     */
    public String getPcParam() {

        return PC_p;
    }

    /**
     * Get both PC values
     */
    public String getPcValue() {

        return PC_v;
    }

    /**
     * Get Parameter codes for rain
     */
    public String getRainParam(int a) {

        if (a == 0) {
            return getPpAndPcParam();
        }

        if (a == 1) {
            return getPcParam();
        }

        if (a == 2) {
            return getPpParam();
        }

        return "";
    }

    /**
     * Get Parameter values for rain
     */
    public String getRainValue(int a) {

        if (a == 0) {
            return getPpAndPcValue();
        }

        if (a == 1) {
            return getPcValue();
        }

        if (a == 2) {
            return getPpValue();
        }

        return "";
    }

    /**
     * Get both Parameter codes for other
     */
    public String getOtherParam() {

        return Other_p;
    }

    /**
     * Get both values for other
     */
    public String getOtherValue() {

        return Other_v;
    }

    /**
     * Set Parameter codes
     * 
     * @param gage
     * 
     */
    private void setP(GageData gage) {

        if (gage.getPe().equalsIgnoreCase("PP")) {

            PP_p = gage.getParamCode();
            return;

        }

        if (gage.getPe().equalsIgnoreCase("PC")) {

            PC_p = gage.getParamCode();
            return;
        }

        Other_p = gage.getParamCode();

    }

    /**
     * Set values
     * 
     * @param gage
     * 
     */
    private void setV(GageData gage) {

        if (gage.getPe().equalsIgnoreCase("PP")) {

            PP_v = formatValue(gage);
            return;
        }

        if (gage.getPe().equalsIgnoreCase("PC")) {

            PC_v = formatValue(gage);
            return;
        }

        Other_v = formatValue(gage);

    }

    /**
     * Add Parameter codes
     * 
     * @param gage
     * 
     */
    private void addParam(GageData gage) {

        if (gage.getPe().equalsIgnoreCase("PP")) {

            PP_p = combineString(PP_p, gage.getParamCode());
        }

        if (gage.getPe().equalsIgnoreCase("PC")) {

            PC_p = combineString(PC_p, gage.getParamCode());
        }

        Other_p = combineString(Other_p, gage.getParamCode());
    }

    /**
     * Add values
     * 
     * @param gage
     * 
     */
    private void addValue(GageData gage) {

        if (gage.getPe().equalsIgnoreCase("PP")) {

            PP_v = combineString(PP_v, formatValue(gage));
        }

        if (gage.getPe().equalsIgnoreCase("PC")) {

            PC_v = combineString(PC_v, formatValue(gage));
        }

        Other_v = combineString(Other_v, formatValue(gage));

    }

    /**
     * Combine two strings
     * 
     * @param strOne
     *            first string
     * 
     * @param strTwo
     *            second string
     */
    private String combineString(String strOne, String strTwo) {

        String combinedStr = "";

        if (strOne.equalsIgnoreCase("")) {
            combinedStr = strTwo;
        } else {
            if (!strTwo.equalsIgnoreCase("")) {
                combinedStr = strOne + "\n" + strTwo;
            } else {
                combinedStr = strOne;
            }
        }

        return combinedStr;
    }

    public static String formatValue(GageData pGage) {

        String valueLabel;
        String formatStr = null;

        formatStr = getDataFormat(pGage.getPe());

        if (pGage.getValue() == -9999) {
            valueLabel = "M";
        } else {
            valueLabel = String.format(formatStr, pGage.getValue());
        }

        return valueLabel;
    }

}
