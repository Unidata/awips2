/**
 * NcscatProcessing
 *
 * This class sets the raw data to  Arraylist of bytes
 * separated by fixed length
 *
 * <pre>
 * Uma Josyula                               11/2009         Creation
 * B. Hebbard                                07/2012         Handle OSCAT / OSCAT_HI
 * B. Hebbard        R4865/TTR984            10/2014         Tighten code that infers satellite type
 *                                                           (from date/hour field recurrence) and
 *                                                           date handling to prevent garbage from being
 *                                                           interpreted as dates decades in the future.
 * </pre>
 *
 * This code has been developed by the SIB for use in the AWIPS system.
 */

package gov.noaa.nws.ncep.edex.plugin.ncscat.util;

import gov.noaa.nws.ncep.common.dataplugin.ncscat.NcscatMode;
import gov.noaa.nws.ncep.common.dataplugin.ncscat.NcscatPoint;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.NoSuchElementException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.log4j.Logger;

public class NcscatProcessing {

    private final Logger log = Logger.getLogger(getClass().getName());

    private final Log theLogger = LogFactory.getLog(getClass());

    private List<NcscatPoint> item;

    List<Byte> newMessage = null;

    NcscatPoint sPointObj;

    private Calendar endTime;

    private int scatNumber;

    private Calendar startTime;

    private int recordLength;

    private String inputFileName;

    private NcscatMode ncscatMode;

    public NcscatProcessing() {
    }

    public byte[] separate(byte[] data) {
        doSeparate(data);
        try {
            if (newMessage == null) {
                return (byte[]) null;
            } else {
                byte[] tempBytes = listByteTobyteArray(newMessage);
                return tempBytes;
            }

        } catch (NoSuchElementException e) {
            return (byte[]) null;
        }

    }

    /**
     * @param message
     *            separate bulletins
     */
    private void doSeparate(byte[] message) {

        ByteBuffer byteBuffer = ByteBuffer.allocate(message.length);
        byteBuffer.put(message, 0, message.length);

        int tempLength = message.length;

        startTime = Calendar.getInstance();
        startTime.setLenient(false); // guard against wild values
        startTime.set(Calendar.MILLISECOND, 0);
        endTime = Calendar.getInstance();
        endTime.setLenient(false); // guard against wild values
        endTime.set(Calendar.MILLISECOND, 0);

        // Determine type of data (that is, which satellite and resolution)
        // by examining it for unique consistency with known characteristics

        NcscatMode matchedMode = NcscatMode.UNKNOWN;
        for (NcscatMode mode : NcscatMode.values()) {
            if (mode.consistentWith(byteBuffer)) {
                matchedMode = mode;
                break;
            }
        }

        if (matchedMode == NcscatMode.UNKNOWN) {
            // TODO: Log error/warning -- and quit?
        }

        ncscatMode = matchedMode;

        // Set for historical compatibility...
        scatNumber = ncscatMode.getPointsPerRow();
        // assert ncscatMode.getBytesPerRow() % 2 == 0;
        recordLength = ncscatMode.getBytesPerRow() / 2;

        if (ncscatMode.getFileHeaderLength() > 0) {
            // If there is a file header (separate from per-row headers), remove
            // it from the message array here, so we don't have to mess with it
            // separately later.
            tempLength = message.length - ncscatMode.getFileHeaderLength();
            byte[] xmessage = new byte[tempLength];
            for (int i = 0; i < tempLength; i++) {
                xmessage[i] = message[i + ncscatMode.getFileHeaderLength()];
            }
            message = xmessage;
            byteBuffer.clear();
            byteBuffer = null;
            byteBuffer = ByteBuffer.allocate(tempLength);
            byteBuffer.put(message, 0, tempLength);
        }

        // Endianess correction must be applied to the 4-short date/time field
        // on each line. Here we set the ByteOrder of the byteBuffer, so
        // that the 4 getShort(...) calls below will do the byte-flipping for
        // us.
        //
        ByteOrder byteOrder = ncscatMode.getByteOrder();
        byteBuffer.order(byteOrder);

        try {
            if (message != null) {
                int ji = 0;
                int dNcscatMult = 0;
                int doubleNcscat = scatNumber * 2;
                int scatXLen = doubleNcscat * 9 + 8;
                int byteNum = 0;

                // Make a pass through all rows in file, just to determine
                // earliest and latest times
                int day, hour, min, sec;
                while (ji < tempLength) {
                    day = byteBuffer.getShort(ji);
                    hour = byteBuffer.getShort(ji + 2);
                    min = byteBuffer.getShort(ji + 4);
                    sec = byteBuffer.getShort(ji + 6);

                    if (day < 1 || day > 366 || hour < 0 || hour > 23
                            || min < 0 || min > 59 || sec < 0 || sec > 60) {
                        // TODO log error?
                        break;// TODO continue?
                    }

                    if (ji < 8) { // first row
                        startTime.set(Calendar.DAY_OF_YEAR, day);
                        startTime.set(Calendar.HOUR_OF_DAY, hour);
                        startTime.set(Calendar.MINUTE, min);
                        startTime.set(Calendar.SECOND, sec);
                    }

                    // Don't know ahead of time which row will be last
                    // so set each time; last one will remain endTime
                    endTime.set(Calendar.DAY_OF_YEAR, day);
                    endTime.set(Calendar.HOUR_OF_DAY, hour);
                    endTime.set(Calendar.MINUTE, min);
                    endTime.set(Calendar.SECOND, sec);

                    ji = ji + scatXLen;

                }// for while

                // Time bounds scan done; now go back through the row data and
                // rearrange as needed.

                newMessage = new ArrayList<Byte>(tempLength);

                while (byteNum < tempLength) {
                    int consByteNum = byteNum;
                    while (byteNum < consByteNum + 8) {

                        // Here we again apply endianess correction to the
                        // 4-short date/time field on each row. Above (via the
                        // ByteBuffer) was just for determining startTime and
                        // endTime of the entire data set. Here we do it again
                        // as the 'message' array is moved to newMessage
                        // (ArrayList) for later return and eventual writing to
                        // the HDF5 data for each scan row. Note that the
                        // byte-swapping is done below for the data fields in
                        // each row following the leading date/time (in the
                        // process of regrouping the data so data for each point
                        // is together); here we do it for the date as well
                        // (where no other regrouping is required).
                        //
                        int offsetInDate = byteNum - consByteNum; // 0 thru 7
                        if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
                            // need to flip the 2 bytes of each short; map 0 1 2
                            // 3 4 5 6 7 to 1 0 3 2 5 4 7 6, respectively
                            offsetInDate = offsetInDate / 2 * 2
                                    + (1 - offsetInDate % 2);
                        }
                        newMessage.add((Byte) message[consByteNum
                                + offsetInDate]);
                        byteNum++;
                    }// date field 8 bytes

                    // Done with row header (date/time); now start on the data
                    // for points in that row. (consByteNum is index of first
                    // byte of such data .)

                    consByteNum = byteNum;

                    // In the incoming data for a row, all values for each
                    // single parameter are grouped together (for all points in
                    // the row). The following code "shuffles" things so that
                    // all values for a single point are grouped together
                    // (that is, all parameters for a given point). In the
                    // process, proper endianess order is set for the two bytes
                    // making up each data value.

                    while ((scatXLen * dNcscatMult + 7) <= byteNum
                            && byteNum < (scatXLen * (dNcscatMult + 1))) {
                        int calc = 0;
                        for (int qNoIndex = 0; qNoIndex < doubleNcscat; qNoIndex++) {
                            for (int rou = 0; rou <= 8; rou++) {
                                if (byteNum < tempLength) {
                                    calc = consByteNum + doubleNcscat * rou
                                            + qNoIndex;
                                    if (byteOrder == ByteOrder.LITTLE_ENDIAN) {
                                        // swap the two bytes making up the
                                        // short
                                        newMessage.add(message[calc + 1]);
                                        byteNum++;
                                        newMessage.add(message[calc]);
                                        byteNum++;
                                    } else { // ByteOrder.BIG_ENDIAN
                                        // preserve order of the two bytes
                                        // making up the short
                                        newMessage.add(message[calc]);
                                        byteNum++;
                                        newMessage.add(message[calc + 1]);
                                        byteNum++;
                                    }
                                } // end of if
                            } // end of for rou
                            qNoIndex++;
                        } // end of for qNoIndex
                        dNcscatMult++;
                    } // end of while

                } // while (each row)

            }// if message is not null
        }// end of try block
        catch (Exception e) {
            e.printStackTrace();
            if (log.isInfoEnabled()) {
                log.info("No valid records found!");
            }
            theLogger.warn("No valid records found!");
        }
        return;
    }

    public NcscatMode getNcscatMode() {
        return ncscatMode;
    }

    /**
     * processHDF5Data -- SHOULD NOT BE USED IN PRESENT FORM Please see
     * NcscatResource.processHDF5Data(...)
     * 
     * @return List<NcscatPoint>
     * 
     * @deprecated Please see NcscatResource.processHDF5Data(...)
     */
    @Deprecated
    public List<NcscatPoint> processHDF5Data(byte[] hdf5Msg) {
        int ji = 0, bitNum = 0;
        int day, hour, min, sec;
        item = new ArrayList<NcscatPoint>();
        // TODO - Caution! Separate startTime Calendar object needs to be
        // allocated for each point row, since will be shared by all points
        // in that row. See below.
        startTime = Calendar.getInstance();
        ByteBuffer byteBuffer = null;
        byteBuffer = ByteBuffer.allocate(hdf5Msg.length);
        byteBuffer.put(hdf5Msg, 0, hdf5Msg.length);

        while (ji < hdf5Msg.length) {

            day = byteBuffer.getShort(bitNum);
            hour = byteBuffer.getShort(bitNum + 2);
            min = byteBuffer.getShort(bitNum + 4);
            sec = byteBuffer.getShort(bitNum + 6);
            ji = ji + 8;
            bitNum = bitNum + 8;
            // TODO - Caution! Need to allocate new startTime here...
            startTime.set(Calendar.DAY_OF_YEAR, day);
            startTime.set(Calendar.HOUR_OF_DAY, hour);
            startTime.set(Calendar.MINUTE, min);
            startTime.set(Calendar.SECOND, sec);
            for (int j = ji; j < ji + scatNumber * 18
                    && bitNum < hdf5Msg.length; j = j + 18) {
                sPointObj = new NcscatPoint();
                // TODO - continued - otherwise all points in all rows get same
                // time here
                sPointObj.setStTime(startTime);
                sPointObj.setLat(byteBuffer.getShort(j));
                sPointObj.setLon(byteBuffer.getShort(j + 2));
                sPointObj.setIql(byteBuffer.getShort(j + 4));
                sPointObj.setIsp(byteBuffer.getShort(j + 6));
                sPointObj.setIdr(byteBuffer.getShort(j + 8));
                sPointObj.setIrn(byteBuffer.getShort(j + 10));
                sPointObj.setIb1(byteBuffer.getShort(j + 12));
                sPointObj.setIb2(byteBuffer.getShort(j + 14));
                sPointObj.setIb3(byteBuffer.getShort(j + 16));
                bitNum = bitNum + 18;
                item.add(sPointObj);

            }// for

            ji = bitNum;

        }// for while

        return item;

    }

    public Calendar getStartTime() {
        return startTime;
    }

    public void setStartTime(Calendar startTime) {
        this.startTime = startTime;
    }

    public Calendar getEndTime() {
        return endTime;
    }

    public void setEndTime(Calendar endTime) {
        this.endTime = endTime;
    }

    private static byte[] listByteTobyteArray(List<Byte> temp) {
        byte[] byteArray = new byte[temp.size()];
        int jkl = 0;
        for (Byte current : temp) {
            byteArray[jkl] = current;
            jkl++;
        }

        return byteArray;
    }

    public int getRecordLength() {
        return recordLength;
    }

    public void setRecordLength(int recordLength) {
        this.recordLength = recordLength;
    }

    public String getInputFileName() {
        return inputFileName;
    }

    public void setInputFileName(String inputFileName) {
        this.inputFileName = inputFileName;
    }

}
