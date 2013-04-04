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

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decodes the general status message portion of the product
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * 1-26-2009               mnash       Initial creation
 * 1-20-2010     DR 4059   Zihou Wang  Decode more GSM status
 * 03/01/13      DR15496   zwang       Decode expanded GSM
 * </pre>
 * 
 * @author mnash
 * @version 1.0
 */

@DynamicSerialize
public class GSMBlock extends AbstractBlock implements ISerializableObject {

    private static final int BLOCK_ID = 2;

    public GSMBlock(DataInputStream in) throws IOException {
        super(in);
    }

    public GSMBlock() {

    }

    public static int getBlockId() {
        return BLOCK_ID;
    }

    @DynamicSerialize
    public static class GSMMessage implements ISerializableObject {

        @DynamicSerializeElement
        public int mode;

        @DynamicSerializeElement
        public int operabilityStatus;

        @DynamicSerializeElement
        public int volumeCoveragePattern;

        @DynamicSerializeElement
        public int numCuts;

        @DynamicSerializeElement
        double[] elevation;

        @DynamicSerializeElement
        public int rdaStatus;

        @DynamicSerializeElement
        public int rdaAlarms;

        @DynamicSerializeElement
        public int dataTransmissionEnable;

        @DynamicSerializeElement
        public int rpgOperability;

        @DynamicSerializeElement
        public int rpgAlarms;

        @DynamicSerializeElement
        public int rpgStatus;

        @DynamicSerializeElement
        public int rpgNarrowbandStatus;

        @DynamicSerializeElement
        public double reflectCalibCorr;

        @DynamicSerializeElement
        public int productAvail;

        @DynamicSerializeElement
        public int superResolutionCuts;

        @DynamicSerializeElement
        public int cmdStatus;

        @DynamicSerializeElement
        public double reflectCalibCorr2;

        @DynamicSerializeElement
        public int rdaBuildNum;

        @DynamicSerializeElement
        public int rdaChannelNum;

        @DynamicSerializeElement
        public int buildVersion;

        @DynamicSerializeElement
        public int vcpInfo;
        
        /**
         * @return the mode
         */
        public int getMode() {
            return mode;
        }

        /**
         * @param mode
         *            the mode to set
         */
        public void setMode(int mode) {
            this.mode = mode;
        }

        /**
         * @return the operabilityStatus
         */
        public int getOperabilityStatus() {
            return operabilityStatus;
        }

        /**
         * @param operabilityStatus
         *            the operabilityStatus to set
         */
        public void setOperabilityStatus(int operabilityStatus) {
            this.operabilityStatus = operabilityStatus;
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
         * @return the numCuts
         */
        public int getNumCuts() {
            return numCuts;
        }

        /**
         * @param numCuts
         *            the numCuts to set
         */
        public void setNumCuts(int numCuts) {
            this.numCuts = numCuts;
        }

        /**
         * @return the elevation
         */
        public double[] getElevation() {
            return elevation;
        }

        /**
         * @param elevation
         *            the elevation to set
         */
        public void setElevation(double[] elevation) {
            this.elevation = elevation;
        }

        /**
         * @return the rdaStatus
         */
        public int getRdaStatus() {
            return rdaStatus;
        }

        /**
         * @param rdaStatus
         *            the rdaStatus to set
         */
        public void setRdaStatus(int rdaStatus) {
            this.rdaStatus = rdaStatus;
        }

        /**
         * @return the rdaAlarms
         */
        public int getRdaAlarms() {
            return rdaAlarms;
        }

        /**
         * @param rdaAlarms
         *            the rdaAlarms to set
         */
        public void setRdaAlarms(int rdaAlarms) {
            this.rdaAlarms = rdaAlarms;
        }

        /**
         * @return the dataTransmissionEnable
         */
        public int getDataTransmissionEnable() {
            return dataTransmissionEnable;
        }

        /**
         * @param dataTransmissionEnable
         *            the dataTransmissionEnable to set
         */
        public void setDataTransmissionEnable(int dataTransmissionEnable) {
            this.dataTransmissionEnable = dataTransmissionEnable;
        }

        /**
         * @return the rpgOperability
         */
        public int getRpgOperability() {
            return rpgOperability;
        }

        /**
         * @param rpgOperability
         *            the rpgOperability to set
         */
        public void setRpgOperability(int rpgOperability) {
            this.rpgOperability = rpgOperability;
        }

        /**
         * @return the rpgAlarms
         */
        public int getRpgAlarms() {
            return rpgAlarms;
        }

        /**
         * @param rpgAlarms
         *            the rpgAlarms to set
         */
        public void setRpgAlarms(int rpgAlarms) {
            this.rpgAlarms = rpgAlarms;
        }

        /**
         * @return the rpgStatus
         */
        public int getRpgStatus() {
            return rpgStatus;
        }

        /**
         * @param rpgStatus
         *            the rpgStatus to set
         */
        public void setRpgStatus(int rpgStatus) {
            this.rpgStatus = rpgStatus;
        }

        /**
         * @return the rpgNarrowbandStatus
         */
        public int getRpgNarrowbandStatus() {
            return rpgNarrowbandStatus;
        }

        /**
         * @param rpgNarrowbandStatus
         *            the rpgNarrowbandStatus to set
         */
        public void setRpgNarrowbandStatus(int rpgNarrowbandStatus) {
            this.rpgNarrowbandStatus = rpgNarrowbandStatus;
        }

        /**
         * @return the reflectCalibCorr
         */
        public double getReflectCalibCorr() {
            return reflectCalibCorr;
        }

        /**
         * @param reflectCalibCorr
         *            the reflectCalibCorr to set
         */
        public void setReflectCalibCorr(double reflectCalibCorr) {
            this.reflectCalibCorr = reflectCalibCorr;
        }

        /**
         * @return the productAvail
         */
        public int getProductAvail() {
            return productAvail;
        }

        /**
         * @param productAvail
         *            the productAvail to set
         */
        public void setProductAvail(int productAvail) {
            this.productAvail = productAvail;
        }

        /**
         * @return the superResolutionCuts
         */
        public int getSuperResolutionCuts() {
            return superResolutionCuts;
        }

        /**
         * @param superResolutionCuts
         *            the superResolutionCuts to set
         */
        public void setSuperResolutionCuts(int superResolutionCuts) {
            this.superResolutionCuts = superResolutionCuts;
        }

        /**
         * @return the cmdStatus (Clutter Mitigation Decision Status)
         */
        public int getCmdStatus() {
            return cmdStatus;
        }

        /**
         * @param cmdStatus
         *            the cmdStatus to set
         */
        public void setCmdStatus(int cmdStatus) {
            this.cmdStatus = cmdStatus;
        }

        /**
         * @return the reflectCalibCorr2
         */
        public double getReflectCalibCorr2() {
            return reflectCalibCorr2;
        }

        /**
         * @param reflectCalibCorr2
         *            the reflectCalibCorr2 to set
         */
        public void setReflectCalibCorr2(double reflectCalibCorr2) {
            this.reflectCalibCorr2 = reflectCalibCorr2;
        }

        /**
         * @return the rdaBuildNum
         */
        public int getRdaBuildNum() {
            return rdaBuildNum;
        }

        /**
         * @param rdaBuildNum
         *            the rdaBuildNum to set
         */
        public void setRdaBuildNum(int rdaBuildNum) {
            this.rdaBuildNum = rdaBuildNum;
        }

        /**
         * @return the rdaChannelNum
         */
        public int getRdaChannelNum() {
            return rdaChannelNum;
        }

        /**
         * @param rdaChannelNum
         *            the rdaChannelNum to set
         */
        public void setRdaChannelNum(int rdaChannelNum) {
            this.rdaChannelNum = rdaChannelNum;
        }

        /**
         * @return the buildVersion
         */
        public int getBuildVersion() {
            return buildVersion;
        }

        /**
         * @param buildVersion
         *            the buildVersion to set
         */
        public void setBuildVersion(int buildVersion) {
            this.buildVersion = buildVersion;
        }
        
        /**
         * @return the vcpSupInfo
         */
        public int getVcpInfo() {
            return vcpInfo;
        }

        /**
         * @param vcpSupInfo
         *            the vcpSupInfo to set
         */
        public void setVcpInfo(int vcpInfo) {
            this.vcpInfo = vcpInfo;
        }
        
        @Override
        public String toString() {
            return String
                    .format("(%d,%d %s)", mode, buildVersion, productAvail);
        }
    }

    @DynamicSerializeElement
    GSMMessage message;

    /**
     * @return the points
     */
    public GSMMessage getMessage() {
        return message;
    }

    /**
     * @param points
     *            the points to set
     */
    public void setMessage(GSMMessage message) {
        this.message = message;
    }

    /*
     * (non-Javadoc)
     * 
     * @seecom.raytheon.uf.common.dataplugin.radar.level3.SymbologyPacket#init(java
     * .io. DataInputStream)
     */
    @Override
    protected void init(DataInputStream in) throws IOException {
        message = new GSMMessage();
        message.mode = in.readShort();
        message.operabilityStatus = in.readShort();
        message.volumeCoveragePattern = in.readShort();
        message.numCuts = in.readShort();
        message.elevation = new double[25];
        for (int j = 0; j < 20; j++) {
            message.elevation[j] = in.readShort();
        }
        message.rdaStatus = in.readShort();
        message.rdaAlarms = in.readShort();
        message.dataTransmissionEnable = in.readShort();
        message.rpgOperability = in.readShort();
        message.rpgAlarms = in.readShort();
        message.rpgStatus = in.readShort();
        message.rpgNarrowbandStatus = in.readShort();
        message.reflectCalibCorr = in.readShort() * 0.25;
        message.productAvail = in.readShort();
        message.superResolutionCuts = in.readShort();
        message.cmdStatus = in.readShort();
        message.reflectCalibCorr2 = in.readShort() * 0.25;
        message.rdaBuildNum = in.readShort();
        message.rdaChannelNum = in.readShort();
        in.skip(4);
        message.buildVersion = in.readShort();
        
        // GSM size is increased to 200 bytes since Build 14.0
        if (message.buildVersion >= 140) {
        	for (int j = 0; j < 5; j++) {
        		message.elevation[20+j] = in.readShort();
        	}
        	message.vcpInfo = in.readShort();
        }
        else {
        	message.vcpInfo = 0;
        }
    }

    @Override
    public String toString() {
        String s = super.toString() + " GSMPacket";
        s += "\n\t" + message;
        return s;
    }
}
