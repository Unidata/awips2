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
package com.raytheon.viz.hydrocommon.util;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.localization.LocalizationManager;
import com.raytheon.viz.hydrocommon.HydroConstants;
import com.raytheon.viz.hydrocommon.data.FloodCategoryData;
import com.raytheon.viz.hydrocommon.data.FloodData;
import com.raytheon.viz.hydrocommon.data.ForecastData;
import com.raytheon.viz.hydrocommon.data.HydroData;
import com.raytheon.viz.hydrocommon.data.LocationData;
import com.raytheon.viz.hydrocommon.data.LowWaterData;
import com.raytheon.viz.hydrocommon.data.LowWaterStatementData;
import com.raytheon.viz.hydrocommon.data.PhysicalElementData;
import com.raytheon.viz.hydrocommon.data.RejectedData;
import com.raytheon.viz.hydrocommon.datamanager.HydroDBDataManager;
import com.raytheon.viz.hydrocommon.whfslib.GeoUtil;

/**
 * Hydro Data conversion Utilities
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date			Ticket#		Engineer	Description
 * ------------	----------	-----------	--------------------------
 * Nov 6, 2008	1661	    askripsky	Initial creation.
 * 12/16/2008   1782        grichard    Refreshed Product Viewer.
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

public class HydroDataUtils {

    protected static SimpleDateFormat dbFormat = HydroConstants.DATE_FORMAT;

    protected static SimpleDateFormat postingTimeFormat = new SimpleDateFormat(
    "yyyy-MM-dd HH:mm:ss.SSS");

    protected static SimpleDateFormat basisTimeFormat = new SimpleDateFormat(
    "yyyy-MM-dd HH:mm:ss.SSS");

    protected static SimpleDateFormat lwFormat = new SimpleDateFormat(
            "yyyy-MM-dd");

    static {
        lwFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        dbFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        postingTimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
        basisTimeFormat.setTimeZone(TimeZone.getTimeZone("UTC"));
    }

    public static void copyHydroData(HydroData source, HydroData destination) {

        destination.setLid(source.getLid());
        destination.setPe(source.getPe());
        destination.setDur(source.getDur());
        destination.setTs(source.getTs());
        destination.setExtremum(source.getExtremum());
        destination.setValue(source.getValue());
        destination.setRevision(source.getRevision());
        destination.setShefQualCode(source.getShefQualCode());
        destination.setProductID(source.getProductID());
        destination.setProductTime(source.getProductTime());
        destination.setQualityCode(source.getQualityCode());

        // Set Posting Time to current time
        Date d = Calendar.getInstance(TimeZone.getTimeZone("GMT")).getTime();        
        destination.setPostingTime(d);
    }

    public static RejectedData convertPhysicalElementToRejected(
            PhysicalElementData source) {
        RejectedData destination = new RejectedData();

        copyHydroData(source, destination);

        // Set probability to -1
        destination.setProbability(-1);

        /* set validtime and basistime to obstime for observed data */
        destination.setValidTime(source.getObstime());
        destination.setBasisTime(source.getObstime());

        /* set reject_type to M for Manual */
        destination.setRejectType("M");

        /* copy userid to rejectedData structure */
        destination.setUserID(LocalizationManager.getInstance()
                .getCurrentUser());

        return destination;
    }

    public static PhysicalElementData convertRejectedToPhysicalElement(
            RejectedData source) {
        PhysicalElementData destination = new PhysicalElementData();

        copyHydroData(source, destination);

        /* set obstime from validtime for observed data */
        destination.setObstime(source.getValidTime());

        return destination;
    }

    public static ForecastData convertRejectedToForecast(RejectedData source) {
        ForecastData destination = new ForecastData();

        copyHydroData(source, destination);

        destination.setProbability(source.getProbability());
        destination.setValidTime(source.getValidTime());
        destination.setBasisTime(source.getBasisTime());

        return destination;
    }

    /**
     * Constructs the clause that indicates the primary key for flood category
     * data tables.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(FloodCategoryData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("'");

        return rval.toString();
    }

    /**
     * Constructs the clause that indicates the primary key for flood data
     * tables.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(FloodData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND stage='");
        rval.append(data.getStage());
        rval.append("'");

        return rval.toString();
    }

    /**
     * Constructs the clause that indicates the primary key for forecast data
     * tables.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(ForecastData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND pe='");
        rval.append(data.getPe());
        rval.append("' AND dur='");
        rval.append(data.getDur());
        rval.append("' AND ts='");
        rval.append(data.getTs());
        rval.append("' AND extremum='");
        rval.append(data.getExtremum());
        rval.append("' AND probability='");
        rval.append(data.getProbability());
        rval.append("' AND validtime='");
        rval.append(dbFormat.format(data.getValidTime()));
        rval.append("' AND basistime='");
        rval.append(dbFormat.format(data.getBasisTime()));
        rval.append("'");

        return rval.toString();
    }

    /**
     * Returns the primary key string for a given record for any physical
     * element table
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(PhysicalElementData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND pe='");
        rval.append(data.getPe());
        rval.append("' AND dur='");
        rval.append(data.getDur());
        rval.append("' AND ts='");
        rval.append(data.getTs());
        rval.append("' AND extremum='");
        rval.append(data.getExtremum());
        rval.append("' AND obstime='");
        rval.append(dbFormat.format(data.getObstime()));
        rval.append("'");

        return rval.toString();
    }

    /**
     * Constructs the clause that indicates the primary key for the RejectedData
     * table.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(RejectedData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND pe='");
        rval.append(data.getPe());
        rval.append("' AND dur='");
        rval.append(data.getDur());
        rval.append("' AND ts='");
        rval.append(data.getTs());
        rval.append("' AND extremum='");
        rval.append(data.getExtremum());
        rval.append("' AND probability='");
        rval.append(data.getProbability());
        rval.append("' AND validtime='");
        rval.append(dbFormat.format(data.getValidTime()));
        rval.append("' AND basistime='");
        rval.append(basisTimeFormat.format(data.getBasisTime()));
        rval.append("' AND postingtime='");
        rval.append(postingTimeFormat.format(data.getPostingTime()));
        rval.append("'");

        return rval.toString();
    }

    /**
     * Constructs the clause that indicates the primary key for the lowwater
     * table.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(LowWaterData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND lwdat='");
        rval.append(lwFormat.format(data.getDate()));
        rval.append("'");

        return rval.toString();
    }

    /**
     * Constructs the clause that indicates the primary key for the lowwater
     * table.
     * 
     * @param data
     * @return
     */
    public static String getPKStatement(LowWaterStatementData data) {
        StringBuffer rval = new StringBuffer();

        rval.append("lid='");
        rval.append(data.getLid());
        rval.append("' AND pe='");
        rval.append(data.getPe());
        rval.append("' AND lower_value=");
        rval.append(data.getLowerValueString());
        rval.append(" AND criteria_rank='");
        rval.append(data.getCriteriaRank());
        rval.append("'");

        return rval.toString();
    }

    /**
     * Returns the display string for the date
     * 
     * @param date
     *            The date to display.
     * @param sdf
     *            The format of the date.
     * @return The formatted display string.
     */
    public static String getDisplayString(Date date, SimpleDateFormat sdf) {
        return (date != null) ? sdf.format(date) : "";
    }

    public static String getDisplayString(String finalFormat,
            String displayFormat, Double val) {
        String temp = (Double.compare(val, Double
                .valueOf(HydroConstants.MISSING_VALUE)) != 0) ? String.format(
                displayFormat, val) : "";

        return String.format(finalFormat, temp);
    }

    public static String getDisplayString(String finalFormat,
            String displayFormat, int val) {
        String temp = (val != HydroConstants.MISSING_VALUE) ? String.format(
                displayFormat, val) : "";

        return String.format(finalFormat, temp);
    }

    /**
     * Converts the value in a text field to a double.
     * 
     * @param currTF
     *            The text field to get the double value for.
     * @param tfName
     *            If there is an error in parsing the value, this name will be
     *            used to refer to the value in the error message displayed
     * @return The double value corresponding to the text field value
     */
    public static Double getDoubleFromTF(Shell shell, Text currTF, String tfName) {
        return getDoubleFromTF(shell, currTF, tfName, false);
    }

    /**
     * Converts the value in a text field to a double.
     * 
     * @param currTF
     *            The text field to get the double value for.
     * @param tfName
     *            If there is an error in parsing the value, this name will be
     *            used to refer to the value in the error message displayed
     * @param requiredField
     *            If True and the text field is blank, an error will be
     *            displayed telling the user the field is required
     * @return The double value corresponding to the text field value
     */
    public static Double getDoubleFromTF(Shell shell, Text currTF,
            String tfName, boolean requiredField) {
        Double rval = null;

        String tfVal = currTF.getText();

        if (!tfVal.equals("")) {
            try {
                rval = Double.parseDouble(tfVal);
            } catch (NumberFormatException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb.setMessage("Please enter a valid numeric value for "
                        + tfName);
                mb.open();
            }
        } else if (requiredField) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("A value is required for " + tfName);
            mb.open();
        } else {
            rval = Double.valueOf(HydroConstants.MISSING_VALUE);
        }

        return rval;
    }

    /**
     * Converts the value in a text field to an int.
     * 
     * @param currTF
     *            The text field to get the double value for.
     * @param tfName
     *            If there is an error in parsing the value, this name will be
     *            used to refer to the value in the error message displayed
     * @return The int value corresponding to the text field value
     */
    public static Integer getIntegerFromTF(Shell shell, Text currTF,
            String tfName) {
        return getIntegerFromTF(shell, currTF, tfName, false);
    }

    /**
     * Converts the value in a text field to an int.
     * 
     * @param currTF
     *            The text field to get the double value for.
     * @param tfName
     *            If there is an error in parsing the value, this name will be
     *            used to refer to the value in the error message displayed
     * @param requiredField
     *            If True and the text field is blank, an error will be
     *            displayed telling the user the field is required
     * @return The int value corresponding to the text field value
     */
    public static Integer getIntegerFromTF(Shell shell, Text currTF,
            String tfName, boolean requiredField) {
        Integer rval = null;

        String tfVal = currTF.getText();

        if (!tfVal.equals("")) {
            try {
                rval = Integer.parseInt(tfVal);
            } catch (NumberFormatException e) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb.setMessage("Please enter a valid numeric value for "
                        + tfName);
                mb.open();
            }
        } else if (requiredField) {
            MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
            mb.setText("Invalid Value");
            mb.setMessage("A value is required for " + tfName);
            mb.open();
        } else {
            rval = HydroConstants.MISSING_VALUE;
        }

        return rval;
    }

    /**
     * Returns the string corresponding to the DB value. Takes the MISSING_VALUE
     * into account.
     * 
     * @param val
     *            The double to get a display string for.
     * @return The corresponding string or "" if the value is MISSING_VALUE
     */
    public static String getDisplayString(Double val) {
        String temp = (Double.compare(val, Double
                .valueOf(HydroConstants.MISSING_VALUE)) != 0) ? Double
                .toString(val) : "";

        return temp;
    }

    /**
     * Returns the string corresponding to the DB value. Takes the MISSING_VALUE
     * into account.
     * 
     * @param val
     *            The int to get a display string for.
     * @return The corresponding string or "" if the value is MISSING_VALUE
     */
    public static String getDisplayString(int val) {
        String temp = (val != HydroConstants.MISSING_VALUE) ? Integer
                .toString(val) : "";

        return temp;
    }

    /**
     * Method that determines whether a location exists.
     * 
     * @param tf
     *            -- the text field widget
     * @return rval -- boolean indicator of whether location exists
     */
    public static boolean locationExists(Text tf) {
        boolean rval = false;
        String currLid = tf.getText();

        if (!currLid.equals("")) {
            LocationData locData = new LocationData();
            locData.setLid(currLid);
            try {
                rval = HydroDBDataManager.getInstance().checkData(locData) > 0;
            } catch (VizException e) {
                e.printStackTrace();
            }
        }

        return rval;
    }

    /**
     * Method that determines whether a product *potentially* exists.
     * 
     * @param tf
     *            -- the text field widget
     * @return rval -- boolean indicator of whether product *potentially* exists
     */
    public static boolean productExists(Text tf) {
        boolean rval = false;
        String currProd = tf.getText();

        if (!currProd.equals("")) {
            rval = true;
        }

        return rval;
    }

    /**
     * Converts the value in a text field to a double representation of the
     * latitude in the form DDD MM SS.
     * 
     * @param shell
     *            The shell used to display the error message.
     * @param currTF
     *            The text field to get the value for.
     * @return The double value for the latitude or NULL if invalid.
     */
    public static Double getLatitudeFromTF(Shell shell, Text currTF) {
        Double rval;

        String latTxt = currTF.getText();
        double lat = HydroConstants.MISSING_VALUE;
        if (!latTxt.equals("")) {
            boolean invalidLat = false;

            try {
                lat = GeoUtil.getInstance().cvt_spaced_format(latTxt, 0);
            } catch (Exception e) {
                invalidLat = true;
            }

            if ((lat < -90) || (lat > 90) || invalidLat) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-90 to 90) Latitude\nin the form: DD MM SS");
                mb.open();

                rval = null;
            } else {
                rval = lat;
            }
        } else {
            rval = Double.valueOf(HydroConstants.MISSING_VALUE);
        }

        return rval;
    }

    /**
     * Converts the value in a text field to a double representation of the
     * longitude in the form DDD MM SS.
     * 
     * @param shell
     *            The shell used to display the error message.
     * @param currTF
     *            The text field to get the value for.
     * @return The double value for the longitude or NULL if invalid
     */
    public static Double getLongitudeFromTF(Shell shell, Text currTF) {
        Double rval;

        String lonTxt = currTF.getText();
        double lon = HydroConstants.MISSING_VALUE;
        if (!lonTxt.equals("")) {
            boolean invalidLon = false;

            try {
                lon = GeoUtil.getInstance().cvt_spaced_format(lonTxt, 0);
            } catch (Exception e) {
                invalidLon = true;
                e.printStackTrace();
            }

            if ((lon > 180) || (lon < -180) || invalidLon) {
                MessageBox mb = new MessageBox(shell, SWT.ICON_ERROR | SWT.OK);
                mb.setText("Invalid Value");
                mb
                        .setMessage("Please enter a VALID (-180 to 180) Longitude\nin the form: DD MM SS");
                mb.open();

                rval = null;
            } else {
                rval = lon;
            }
        } else {
            rval = Double.valueOf(HydroConstants.MISSING_VALUE);
        }

        return rval;
    }

    public static String getLatLonDisplayString(double latlon) {
        return (latlon != HydroConstants.MISSING_VALUE) ? GeoUtil.getInstance()
                .cvt_latlon_from_double(latlon) : "";
    }
    
    /**
     * Create a copy of the object.
     * 
     * @param obj The object to copy
     * @return a copy of obj
     * @throws Exception
     */
    public static Object objCopy(Object obj) throws Exception {
        ObjectOutputStream oos = null;
        ObjectInputStream ois = null;
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            oos = new ObjectOutputStream(bos);
            // serialize and pass the object
            oos.writeObject(obj);
            oos.flush();
            ByteArrayInputStream bin = new ByteArrayInputStream(bos
                    .toByteArray());
            ois = new ObjectInputStream(bin);
            // return the new object
            return ois.readObject();
        } catch (Exception e) {
            System.out.println("Exception in ObjectCopier = " + e);
            throw (e);
        } finally {
            oos.close();
            ois.close();
        }
    }
}
