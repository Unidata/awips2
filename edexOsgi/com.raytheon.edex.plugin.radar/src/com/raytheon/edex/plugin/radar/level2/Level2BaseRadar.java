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

package com.raytheon.edex.plugin.radar.level2;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import com.raytheon.uf.common.dataplugin.radar.RadarRecord;
import com.raytheon.uf.common.dataplugin.radar.util.TiltAngleBin;
import com.raytheon.uf.common.time.DataTime;

/**
 * BaseRadar is a class that will allow the user to parse a Level II formatted
 * radar image. This includes:
 * <ul>
 * <li>Decode a radar file</li>
 * <li>Parse a standard level II radar file header</li>
 * </ul>
 * <p>
 * This base radar class will parse the data common to all NEXRAD Level II data
 * files (based on the National Climatic Data Center Data Documentation DSI-6500
 * for NEXRAD Level II data <a href="
 * http://www1.ncdc.noaa.gov/pub/data/documentlibrary/tddoc/td6500.pdf"> located
 * here</a>) and make it available via common methods as described in this
 * JavaDoc.
 * </p>
 * <p>
 * All coordinates which appear as arguments to the methods of this Graphics
 * object are considered relative to the translation origin of this Graphics
 * object prior to the invocation of the method. All rendering operations modify
 * only pixels which lie within the area bounded by both the current clip of the
 * graphics context and the extents of the Component used to create the Graphics
 * object.
 * 
 * 
 * @author Bryan Rockwood
 * @version 1.0
 */
public class Level2BaseRadar {

    private static final int MAX_VELC_BINS = 920;

    private static final int MAX_REFL_BINS = 460;

    private ByteArrayInputStream theRawRadarData;

    private DataInputStream theRadarData;

    private Date radarDate;

    private long radarTimestamp;

    private ArrayList<RadarRecord> records;

    /**
     * A static value which represents reflectivity data.
     */
    public static final int REFL = 300;

    /**
     * A static value which represents velocity data.
     */
    public static final int VELC = 301;

    private int messageJump = 0;

    private String icao;

    private int messageSize;

    private int channelId;

    private int messageType;

    private int idSequence;

    private int numMsgSegments;

    private int msgSegmentNum;

    private double unAmbRange;

    private double azmAngle;

    private int radialNumb;

    private int radialStatus;

    private double elevationAngle;

    private int rngFirstGateRefl;

    private int rngFirstGateVelc;

    private int reflGateSize;

    private int velcGateSize;

    private int numReflGate;

    private int numVelcGate;

    private int sectNumCut;

    private int reflPointer;

    private int velcPointer;

    private int specPointer;

    private int velcResolution;

    private int volCovPattern;

    private int reflCounter = 0;

    private int velcCounter = 0;

    private byte dataValue;

    /**
     * This baseradar constructor accepts a radar file contained within a
     * java.io.File object.
     * 
     * @param aRadar
     *            A java.io.File object containing a raw radar file
     * @throws IOException
     *             If the radar head parsing fails, an IO exception is thrown
     */
    public Level2BaseRadar(File aRadar) throws IOException {
        super();
        int fileSize = (int) aRadar.length();
        byte[] theRawRadarByteArray = new byte[fileSize];
        records = new ArrayList<RadarRecord>();
        FileInputStream radarFile = null;

        try {
            radarFile = new FileInputStream(aRadar);
            radarFile.read(theRawRadarByteArray);
        } finally {
            if (radarFile != null) {
                radarFile.close();
            }
        }

        theRawRadarData = new ByteArrayInputStream(theRawRadarByteArray);
        theRadarData = new DataInputStream(theRawRadarData);
        theRadarData.skip(12);
        this.parseHeader();
        this.processRadarDataMessages();
    }

    /**
     * This baseradar constructor accepts a radar file already converted to a
     * byte array.
     * 
     * @param aRadar
     *            A byte array containing a raw radar file
     * @throws IOException
     *             If the radar head parsing fails, an IO exception is thrown
     */
    public Level2BaseRadar(byte[] aRadar) throws IOException {
        records = new ArrayList<RadarRecord>();
        theRawRadarData = new ByteArrayInputStream(aRadar);
        theRadarData = new DataInputStream(theRawRadarData);
        theRadarData.skip(12);
        this.parseHeader();
        this.processRadarDataMessages();
    }

    private void parseHeader() throws IOException {
        long days = theRadarData.readInt() - 1;
        long millisFromMid = theRadarData.readInt();
        long dayMillis = days * 24 * 60 * 60 * 1000;
        radarTimestamp = dayMillis + millisFromMid;
        byte[] stationBytes = new byte[4];
        theRadarData.read(stationBytes);
        icao = new String(stationBytes);

        Calendar date = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
        date.setTimeInMillis(radarTimestamp);
        radarDate = date.getTime();
        theRadarData.mark(4000);
    }

    /**
     * Gets the next message in the level II product. Returns true if there is
     * another message and false for the end of the file.
     * 
     * @return A boolean indicating the next message is available
     */
    public boolean getNextMessage() throws IOException {
        boolean nextMessage = false;
        theRadarData.reset();
        theRadarData.skip(messageJump);
        if (messageJump == 0) {
            messageJump = 2432;
        }
        theRadarData.mark(4000);
        theRadarData.skip(12);
        messageSize = theRadarData.readShort();
        channelId = theRadarData.readUnsignedByte();
        messageType = theRadarData.readUnsignedByte();
        idSequence = theRadarData.readShort();
        theRadarData.skip(6);
        numMsgSegments = theRadarData.readShort();
        msgSegmentNum = theRadarData.readShort();
        theRadarData.skip(6);
        unAmbRange = theRadarData.readShort();
        int azAngle = theRadarData.readUnsignedShort();
        // subtract half a degree to get to start of radial (not center)
        azmAngle = (azAngle >> 3) * (180.0 / 4096.0) - 0.5;
        if (azmAngle < 0.0) {
            azmAngle += 360.0;
        }
        radialNumb = theRadarData.readShort();
        radialStatus = theRadarData.readShort();
        int elvAngle = theRadarData.readUnsignedShort();
        elevationAngle = (elvAngle >> 3) * (180.0 / 4096.0);
        if (elevationAngle > 90.0) {
            elevationAngle -= 360.0;
        }
        theRadarData.skip(2);
        rngFirstGateRefl = theRadarData.readShort();
        rngFirstGateVelc = theRadarData.readShort();
        reflGateSize = theRadarData.readShort();
        velcGateSize = theRadarData.readShort();
        numReflGate = theRadarData.readShort();
        numVelcGate = theRadarData.readShort();
        sectNumCut = theRadarData.readShort();
        theRadarData.skip(4);
        reflPointer = theRadarData.readShort();
        velcPointer = theRadarData.readShort();
        specPointer = theRadarData.readShort();
        velcResolution = theRadarData.readShort();
        volCovPattern = theRadarData.readShort();

        nextMessage = true;

        return nextMessage;

    }

    /**
     * Gets the next reflectivity data in the level II message. Returns true if
     * there is another reflectivity data set.
     * 
     * @return A boolean indicating the next reflectivity set is available.
     */
    public boolean getNextReflData() throws IOException {
        if (reflCounter == 0) {
            theRadarData.reset();
            theRadarData.skip(reflPointer + 28);
        } else if (reflCounter == numReflGate) {
            reflCounter = 0;
            return false;
        }
        dataValue = theRadarData.readByte();
        reflCounter++;
        return true;
    }

    /**
     * Gets the next velocity data in the level II message. Returns true if
     * there is another velocity data set.
     * 
     * @return A boolean indicating the next velocity set is available.
     */
    public boolean getNextVelcData() throws IOException {
        if (velcCounter == 0) {
            theRadarData.reset();
            theRadarData.skip(velcPointer + 28);
        } else if (velcCounter == numVelcGate) {
            velcCounter = 0;
            return false;
        }
        dataValue = theRadarData.readByte();
        velcCounter++;
        return true;
    }

    /**
     * Returns the radar data's ICAO.
     * 
     * @return The ICAO
     */
    public String getIcao() {
        return icao;
    }

    /**
     * Returns the current raw value.
     * 
     * @return The raw value
     */
    public byte getDataValue() {
        return dataValue;
    }

    /**
     * Returns the current azimuth angle.
     * 
     * @return The azimuth angle
     */
    public double getAzmAngle() {
        return azmAngle;
    }

    /**
     * Returns the current channel id.
     * 
     * @return The channel id
     */
    public int getChannelId() {
        return channelId;
    }

    /**
     * Returns the current elevation angle.
     * 
     * @return The elevation angle
     */
    public double getElevationAngle() {
        return elevationAngle;
    }

    /**
     * Returns the current id sequence.
     * 
     * @return The id sequence
     */
    public int getIdSequence() {
        return idSequence;
    }

    /**
     * Returns the current message size.
     * 
     * @return The message size
     */
    public int getMessageSize() {
        return messageSize;
    }

    /**
     * Returns the current message type.
     * 
     * @return The message type
     */
    public int getMessageType() {
        return messageType;
    }

    /**
     * Returns the current message segment number.
     * 
     * @return The message segment number
     */
    public int getMsgSegmentNum() {
        return msgSegmentNum;
    }

    /**
     * Returns the number of message segments.
     * 
     * @return The number of messsage segments
     */
    public int getNumMsgSegments() {
        return numMsgSegments;
    }

    /**
     * Returns the number of reflectivity gates in the current radial.
     * 
     * @return The number of gates
     */
    public int getNumReflGate() {
        return numReflGate;
    }

    /**
     * Returns the number of velocity gates in the current radial.
     * 
     * @return The number of gates
     */
    public int getNumVelcGate() {
        return numVelcGate;
    }

    /**
     * Returns the radar data timestamp.
     * 
     * @return The date of the data
     */
    public Date getRadarDate() {
        return radarDate;
    }

    private void processRadarDataMessages() throws IOException {
        ArrayList<byte[]> reflData = null;
        ArrayList<Double> reflAngles = null;

        ArrayList<byte[]> velcData = null;
        ArrayList<Double> velcAngles = null;

        while (this.getNextMessage()) {
            if (this.getMessageType() != 1) {
                continue;
            }
            if (this.getMessageType() == 1) {
                if (this.getNumReflGate() > 0) {
                    if (reflData == null) {
                        reflData = new ArrayList<byte[]>();
                        reflAngles = new ArrayList<Double>();
                    }

                    byte[] radialData = new byte[MAX_REFL_BINS];
                    int bin = 0;
                    while (this.getNextReflData()) {
                        radialData[bin++] = this.getDataValue();
                    }
                    for (int i = bin; i < MAX_REFL_BINS; i++) {
                        radialData[i] = (byte) 0;
                    }
                    reflData.add(radialData);
                    reflAngles.add(this.azmAngle);
                }
                if (this.getNumVelcGate() > 0) {
                    if (velcData == null) {
                        velcData = new ArrayList<byte[]>();
                        velcAngles = new ArrayList<Double>();
                    }

                    byte[] radialData = new byte[MAX_VELC_BINS];
                    int bin = 0;
                    while (this.getNextVelcData()) {
                        radialData[bin++] = this.getDataValue();
                    }
                    for (int i = bin; i < MAX_VELC_BINS; i++) {
                        radialData[i] = (byte) 0;
                    }
                    velcData.add(radialData);
                    velcAngles.add(this.azmAngle);
                }
            }
            if (this.radialStatus == 2 || this.radialStatus == 4) {
                if (reflData != null) {
                    RadarRecord refl = new RadarRecord(reflData.size(),
                            MAX_REFL_BINS);
                    for (int i = 0; i < reflData.size(); i++) {
                        System.arraycopy(reflData.get(i), 0, refl.getRawData(),
                                i * MAX_REFL_BINS, MAX_REFL_BINS);
                        refl.setRadialAzimuth(i, reflAngles.get(i).floatValue());
                    }
                    refl.setProductCode(REFL);
                    refl.setNumLevels(256);
                    refl.setGateResolution(this.reflGateSize);
                    refl.setVolumeCoveragePattern(this.volCovPattern);
                    refl.setTrueElevationAngle((float) this.elevationAngle);
                    refl.setPrimaryElevationAngle(TiltAngleBin
                            .getPrimaryElevationAngle(refl
                                    .getTrueElevationAngle()));
                    Calendar timeStamp = Calendar.getInstance();
                    timeStamp.setTimeInMillis(this.radarTimestamp);
                    refl.setDataTime(new DataTime(timeStamp));
                    refl.setThreshold(0, (short) -320);
                    refl.setThreshold(1, (short) 5);
                    records.add(refl);
                    reflData = null;
                }
                if (velcData != null) {
                    RadarRecord velc = new RadarRecord(velcData.size(),
                            MAX_VELC_BINS);
                    for (int i = 0; i < velcData.size(); i++) {
                        System.arraycopy(velcData.get(i), 0, velc.getRawData(),
                                i * MAX_VELC_BINS, MAX_VELC_BINS);
                        velc.setRadialAzimuth(i, reflAngles.get(i).floatValue());
                    }
                    velc.setProductCode(VELC);
                    velc.setNumLevels(256);
                    velc.setGateResolution(this.velcGateSize);
                    velc.setVolumeCoveragePattern(this.volCovPattern);
                    velc.setTrueElevationAngle((float) this.elevationAngle);
                    velc.setPrimaryElevationAngle(TiltAngleBin
                            .getPrimaryElevationAngle(velc
                                    .getTrueElevationAngle()));
                    Calendar timeStamp = Calendar.getInstance();
                    timeStamp.setTimeInMillis(this.radarTimestamp);
                    velc.setDataTime(new DataTime(timeStamp));
                    velc.setThreshold(0,
                            (short) (this.velcResolution == 2 ? -635 : -1270));
                    velc.setThreshold(1, (short) (this.velcResolution == 2 ? 5
                            : 10));
                    records.add(velc);
                    velcData = null;
                }
            }
            if (this.radialStatus == 4) {
                break;
            }
        }
    }

    /**
     * Returns the number of radial angles for a particular elevation.
     * 
     * @return The number of radials for that elevation
     */
    public int getRadialNumb() {
        return radialNumb;
    }

    /**
     * Returns the current radial status.
     * 
     * @return The radial status
     */
    public int getRadialStatus() {
        return radialStatus;
    }

    /**
     * Returns the size of the current reflectivity gate.
     * 
     * @return The size
     */
    public int getReflGateSize() {
        return reflGateSize;
    }

    /**
     * Returns the location to the start of the reflectivity data in the current
     * message.
     * 
     * @return The location to the start of the data
     */
    public int getReflPointer() {
        return reflPointer;
    }

    /**
     * Returns the range to the first reflectivity gate.
     * 
     * @return The range
     */
    public int getRngFirstGateRefl() {
        return rngFirstGateRefl;
    }

    /**
     * Returns the range to the first velocity gate.
     * 
     * @return The range
     */
    public int getRngFirstGateVelc() {
        return rngFirstGateVelc;
    }

    /**
     * Returns the section number cuts.
     * 
     * @return The number of cuts.
     */
    public int getSectNumCut() {
        return sectNumCut;
    }

    /**
     * Returns the location to the start of the spectrum width data.
     * 
     * @return The location
     */
    public int getSpecPointer() {
        return specPointer;
    }

    /**
     * Returns the data input stream version of the raw data.
     * 
     * @return The data input stream
     */
    public DataInputStream getTheRadarData() {
        return theRadarData;
    }

    /**
     * Returns the raw radar data.
     * 
     * @return A byte array input stream of the raw data
     */
    public ByteArrayInputStream getTheRawRadarData() {
        return theRawRadarData;
    }

    /**
     * Returns the current unambiguous range.
     * 
     * @return The unambiguous range
     */
    public double getUnAmbRange() {
        return unAmbRange;
    }

    /**
     * Returns the velocity gate size.
     * 
     * @return The gate size
     */
    public int getVelcGateSize() {
        return velcGateSize;
    }

    /**
     * Returns the location to the start of the velocity data in the current
     * message.
     * 
     * @return The velocity data location
     */
    public int getVelcPointer() {
        return velcPointer;
    }

    /**
     * Returns the current velocity resolution.
     * 
     * @return The resolution
     */
    public double getVelcResolution() {
        return velcResolution;
    }

    /**
     * Returns the current volume coverage pattern.
     * 
     * @return The coverage pattern
     */
    public int getVolCovPattern() {
        return volCovPattern;
    }

    public ArrayList<RadarRecord> getRecords() {
        return records;
    }

}
