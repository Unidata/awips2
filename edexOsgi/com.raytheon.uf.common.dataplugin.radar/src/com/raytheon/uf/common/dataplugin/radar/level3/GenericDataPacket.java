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

import java.io.DataInputStream;
import java.io.IOException;
import java.util.List;

import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataComponent;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericDataParameter;
import com.raytheon.uf.common.dataplugin.radar.level3.generic.GenericUtil;
import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * this packet brings in generic data from a random source
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 4, 2009            mnash     Initial creation
 * 07/29/2013   2148       mnash     Refactor registering of packets to Spring
 * 
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class GenericDataPacket extends SymbologyPacket implements
        ISerializableObject {

    public GenericDataPacket(int packetId, DataInputStream in)
            throws IOException {
        super(packetId, in);
    }

    public GenericDataPacket() {

    }

    private static final int GENERIC_DATA29 = 29;

    @DynamicSerializeElement
    public String name;

    @DynamicSerializeElement
    public String description;

    @DynamicSerializeElement
    public int code;

    @DynamicSerializeElement
    public int type;

    @DynamicSerializeElement
    public long genTime;

    @DynamicSerializeElement
    public String radarName;

    @DynamicSerializeElement
    public float latitude;

    @DynamicSerializeElement
    public float longitude;

    @DynamicSerializeElement
    public float height;

    @DynamicSerializeElement
    public int volumeScanStartTime;

    @DynamicSerializeElement
    public int elevationScanStartTime;

    @DynamicSerializeElement
    public float elevationAngle;

    @DynamicSerializeElement
    public int volumeScanNum;

    @DynamicSerializeElement
    public int operationalMode;

    @DynamicSerializeElement
    public int volumeCoveragePattern;

    @DynamicSerializeElement
    public int elevationNum;

    @DynamicSerializeElement
    protected List<GenericDataParameter> parameters;

    @DynamicSerializeElement
    protected List<GenericDataComponent> components;

    /**
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * @param name
     *            the name to set
     */
    public void setName(String name) {
        this.name = name;
    }

    /**
     * @return the description
     */
    public String getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * @return the code
     */
    public int getCode() {
        return code;
    }

    /**
     * @param code
     *            the code to set
     */
    public void setCode(int code) {
        this.code = code;
    }

    /**
     * @return the type
     */
    public int getType() {
        return type;
    }

    /**
     * @param type
     *            the type to set
     */
    public void setType(int type) {
        this.type = type;
    }

    /**
     * @return the genTime
     */
    public long getGenTime() {
        return genTime;
    }

    /**
     * @param genTime
     *            the genTime to set
     */
    public void setGenTime(long genTime) {
        this.genTime = genTime;

        long timestamp = genTime * 1000; // msec
        java.util.Date d = new java.util.Date(timestamp);
    }

    /**
     * @return the radarName
     */
    public String getRadarName() {
        return radarName;
    }

    /**
     * @param radarName
     *            the radarName to set
     */
    public void setRadarName(String radarName) {
        this.radarName = radarName;
    }

    /**
     * @return the latitude
     */
    public float getLatitude() {
        return latitude;
    }

    /**
     * @param latitude
     *            the latitude to set
     */
    public void setLatitude(float latitude) {
        this.latitude = latitude;
    }

    /**
     * @return the longitude
     */
    public float getLongitude() {
        return longitude;
    }

    /**
     * @param longitude
     *            the longitude to set
     */
    public void setLongitude(float longitude) {
        this.longitude = longitude;
    }

    /**
     * @return the height
     */
    public float getHeight() {
        return height;
    }

    /**
     * @param height
     *            the height to set
     */
    public void setHeight(float height) {
        this.height = height;
    }

    /**
     * @return the volumeScanStartTime
     */
    public int getVolumeScanStartTime() {
        return volumeScanStartTime;
    }

    /**
     * @param volumeScanStartTime
     *            the volumeScanStartTime to set
     */
    public void setVolumeScanStartTime(int volumeScanStartTime) {
        this.volumeScanStartTime = volumeScanStartTime;
    }

    /**
     * @return the elevationScanStartTime
     */
    public int getElevationScanStartTime() {
        return elevationScanStartTime;
    }

    /**
     * @param elevationScanStartTime
     *            the elevationScanStartTime to set
     */
    public void setElevationScanStartTime(int elevationScanStartTime) {
        this.elevationScanStartTime = elevationScanStartTime;
    }

    /**
     * @return the elevationAngle
     */
    public float getElevationAngle() {
        return elevationAngle;
    }

    /**
     * @param elevationAngle
     *            the elevationAngle to set
     */
    public void setElevationAngle(float elevationAngle) {
        this.elevationAngle = elevationAngle;
    }

    /**
     * @return the volumeScanNum
     */
    public int getVolumeScanNum() {
        return volumeScanNum;
    }

    /**
     * @param volumeScanNum
     *            the volumeScanNum to set
     */
    public void setVolumeScanNum(int volumeScanNum) {
        this.volumeScanNum = volumeScanNum;
    }

    /**
     * @return the operationalMode
     */
    public int getOperationalMode() {
        return operationalMode;
    }

    /**
     * @param operationalMode
     *            the operationalMode to set
     */
    public void setOperationalMode(int operationalMode) {
        this.operationalMode = operationalMode;
    }

    /**
     * @return the volumeCoveragePattern
     */
    public int getVolumeCoveragePattern() {
        return volumeCoveragePattern;
    }

    /**
     * @param volumeCoveragePattern
     *            the volumeCoveragePattern to set
     */
    public void setVolumeCoveragePattern(int volumeCoveragePattern) {
        this.volumeCoveragePattern = volumeCoveragePattern;
    }

    /**
     * @return the elevationNum
     */
    public int getElevationNum() {
        return elevationNum;
    }

    /**
     * @param elevationNum
     *            the elevationNum to set
     */
    public void setElevationNum(int elevationNum) {
        this.elevationNum = elevationNum;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        // Skip the Reserved spare
        in.skipBytes(2);

        // read the length of data
        long dataLength = convertToLong(in.readUnsignedShort(),
                in.readUnsignedShort());

        // Parse the Data Description Structure
        parseDataDescriptionStructure(in);

        // Handle the parameters and components of the generic packet
        this.setParameters(GenericUtil.parseParameters(in));
        this.setComponents(GenericUtil.parseComponents(in));
    }

    private long convertToLong(int msw, int lsw) {
        return ((msw * 16) + lsw);
    }

    private void parseDataDescriptionStructure(DataInputStream in)
            throws IOException {
        this.setName(GenericUtil.readInString(in));
        this.setDescription(GenericUtil.readInString(in));

        this.setCode(in.readInt());
        this.setType(in.readInt());

        // this.setGenTime(convertToLong(in.readUnsignedShort(), in
        // .readUnsignedShort()));
        this.setGenTime(in.readInt());

        if (packetId == GENERIC_DATA29) {
            // Skip external data Description spares
            in.skip(10);
        } else {
            this.setRadarName(GenericUtil.readInString(in));
            this.setLatitude(in.readFloat());
            this.setLongitude(in.readFloat());
            this.setHeight(in.readFloat());
            this.setVolumeScanStartTime(in.readInt());
            this.setElevationScanStartTime(in.readInt());
            this.setElevationAngle(in.readFloat());
            this.setVolumeScanNum(in.readInt());
            this.setOperationalMode(in.readInt());
            this.setVolumeCoveragePattern(in.readInt());
            this.setElevationNum(in.readInt());
        }

        // Skip spares reserved for future implementation of compression
        in.skip(8);
    }

    /**
     * @return the parameters
     */
    public List<GenericDataParameter> getParameters() {
        return parameters;
    }

    /**
     * @param parameters
     *            the parameters to set
     */
    public void setParameters(List<GenericDataParameter> parameters) {
        this.parameters = parameters;
    }

    /**
     * @return the components
     */
    public List<GenericDataComponent> getComponents() {
        return components;
    }

    /**
     * @param components
     *            the components to set
     */
    public void setComponents(List<GenericDataComponent> components) {
        this.components = components;
    }

    @Override
    public String toString() {
        StringBuffer rval = new StringBuffer(super.toString()
                + " GenericDataPacket\n");

        rval.append("Name: ");
        rval.append(name);
        rval.append("\nDescription: ");
        rval.append(description);
        rval.append("\nCode: ");
        rval.append(code);
        rval.append("\nType: ");
        rval.append(type);
        rval.append("\nGenerationTime: ");
        rval.append(genTime);
        rval.append("\nRadarName: ");
        rval.append(radarName);
        rval.append("\nLat/Lon: ");
        rval.append(latitude);
        rval.append(", ");
        rval.append(longitude);
        rval.append("\nHeight: ");
        rval.append(height);
        rval.append("\nVolStartTime: ");
        rval.append(volumeScanStartTime);
        rval.append("\nElevStartTime: ");
        rval.append(elevationScanStartTime);
        rval.append("\nElevAngle: ");
        rval.append(elevationAngle);
        rval.append("\nVolScanNumber: ");
        rval.append(volumeScanNum);
        rval.append("\nOpMode: ");
        rval.append(operationalMode);
        rval.append("\nVCP: ");
        rval.append(volumeCoveragePattern);
        rval.append("\nElevNumber: ");
        rval.append(elevationNum);

        rval.append("\nParameters: ");
        for (GenericDataParameter currParam : this.parameters) {
            rval.append("\t" + currParam);
        }

        rval.append("\nComponents: ");
        for (GenericDataComponent currComp : this.components) {
            rval.append("\t" + currComp);
        }

        return rval.toString();
    }
}
