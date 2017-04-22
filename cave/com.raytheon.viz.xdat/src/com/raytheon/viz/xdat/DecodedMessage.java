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
package com.raytheon.viz.xdat;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.TimeZone;

/**
 * Decodes a line of data and puts it into data values.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * May 3, 2011            lvenable     Initial creation
 * 
 * </pre>
 * 
 * @author lvenable
 * @version 1.0
 */

public class DecodedMessage {

    /**
     * Identification
     */
    private String id;

    /**
     * Physical Element
     */
    private String pe;

    /**
     * Duration
     */
    private int dur;

    /**
     * Time series
     */
    private String TS;

    /**
     * Extremum
     */
    private String extremum;

    /**
     * Observe time
     */
    private Calendar obsTime;

    /**
     * Product
     */
    private String product;

    /**
     * Value
     */
    private double value;

    /**
     * Is the message a valid one.
     */
    private boolean validMessage = false;

    public DecodedMessage(String xdatMessage) {
        decodeMessage(xdatMessage);
    }

    /**
     * Decode the data line.
     * 
     * @param xdatMessage
     *            Data line.
     */
    private void decodeMessage(String xdatMessage) {
        if (xdatMessage == null) {
            validMessage = false;
        } else if (xdatMessage.contains("\n")
                & xdatMessage.indexOf("\n") != (xdatMessage.length() - 1)) {
            validMessage = false;
        } else {
            String splittedFields[] = xdatMessage.trim().split("[ ]++");

            if (splittedFields.length != 8 && splittedFields.length != 9
                    && splittedFields.length != 10) {
                validMessage = false;
            } else {
                id = splittedFields[0];
                pe = splittedFields[1];
                try {
                    dur = Integer.parseInt(splittedFields[2]);
                } catch (NumberFormatException nfe) {
                    validMessage = false;
                    return;
                }
                TS = splittedFields[3];
                extremum = splittedFields[4];
                obsTime = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
                SimpleDateFormat obsTimeFmt = new SimpleDateFormat(
                        "yyyy-MM-dd HH:mm:ss", Locale.US);
                try {
                    Date sDate = obsTimeFmt.parse(splittedFields[5] + " "
                            + splittedFields[6]);
                    obsTime.setTime(sDate);
                } catch (ParseException e) {
                    // use current date for gdate
                    validMessage = false;
                }

                String valueTxt;
                if (splittedFields.length == 10) {
                    product = splittedFields[7];
                    valueTxt = splittedFields[8];
                } else if (splittedFields.length == 8) {
                    product = "";
                    valueTxt = splittedFields[7];
                } else {
                    try {
                        Double.parseDouble(splittedFields[7]);
                        product = "";
                        valueTxt = splittedFields[7];
                    } catch (NumberFormatException nfe) {
                        product = splittedFields[7];
                        valueTxt = splittedFields[8];
                    }
                }

                try {
                    value = Double.parseDouble(valueTxt);
                    validMessage = true;
                } catch (NumberFormatException nfe) {
                    validMessage = false;
                }
            }
        }
    }

    public String getId() {
        return id;
    }

    public String getPe() {
        return pe;
    }

    public int getDur() {
        return dur;
    }

    public String getTS() {
        return TS;
    }

    public String getExtremum() {
        return extremum;
    }

    public Calendar getObsTime() {
        return obsTime;
    }

    public String getProduct() {
        return product;
    }

    public double getValue() {
        return value;
    }

    public boolean isValidMessage() {
        return validMessage;
    }

}
