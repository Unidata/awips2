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
import java.util.ArrayList;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Decodes the Alert Adaption Parameters send via the Radar Server
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * July 31, 2009           askripsky   Initial creation
 * 
 * </pre>
 * 
 * @author askripsky
 * @version 1.0
 */

@DynamicSerialize
public class AlertAdaptationParameters implements ISerializableObject {
    @DynamicSerializeElement
    protected ArrayList<AAPCategory> categories;

    public AlertAdaptationParameters(DataInputStream in) throws IOException {
        init(in);
    }

    public AlertAdaptationParameters() {

    }

    @DynamicSerialize
    public static class AAPCategory implements ISerializableObject {

        @DynamicSerializeElement
        protected int alertGroup;

        @DynamicSerializeElement
        protected int alertCategory;

        @DynamicSerializeElement
        public int[] thresholds;

        @DynamicSerializeElement
        public int productCode;

        @DynamicSerializeElement
        public int numberOfThresholds;

        /**
         * @return the alertGroup
         */
        public int getAlertGroup() {
            return alertGroup;
        }

        /**
         * @param alertGroup
         *            the alertGroup to set
         */
        public void setAlertGroup(int alertGroup) {
            this.alertGroup = alertGroup;
        }

        /**
         * @return the thresholds
         */
        public int[] getThresholds() {
            return thresholds;
        }

        /**
         * @param thresholds
         *            the thresholds to set
         */
        public void setThresholds(int[] thresholds) {
            this.thresholds = thresholds;
        }

        /**
         * @return the productCode
         */
        public int getProductCode() {
            return productCode;
        }

        /**
         * @param productCode
         *            the productCode to set
         */
        public void setProductCode(int productCode) {
            this.productCode = productCode;
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
         * @return the numberOfThresholds
         */
        public int getNumberOfThresholds() {
            return numberOfThresholds;
        }

        /**
         * @param numberOfThresholds
         *            the numberOfThresholds to set
         */
        public void setNumberOfThresholds(int numberOfThresholds) {
            this.numberOfThresholds = numberOfThresholds;
        }

        @Override
        public String toString() {
            StringBuffer rval = new StringBuffer();

            rval.append("\nGroup: " + alertGroup);
            rval.append("\nCategory: " + alertCategory);
            rval.append("\nProduct Code: " + productCode);
            rval.append("\nThresholds:");
            for (int i = 0; i < numberOfThresholds; i++) {
                rval.append("\n" + i + ": " + thresholds[i]);
            }
            return rval.toString();
        }
    }

    /**
     * The AAP will always be a set size. If there are no thresholds for the
     * group/category, there will just be 40 bytes of 0x00.
     * 
     * @param in
     * @throws IOException
     */
    protected void init(DataInputStream in) throws IOException {
        // Get the block length
        int blockLen = in.readUnsignedShort();

        categories = new ArrayList<AAPCategory>();

        AAPCategory currCat;
        int[] thresholds;

        for (int k = 0; k < 41; k++) {
            currCat = new AAPCategory();
            currCat.setAlertGroup(in.readUnsignedShort());
            currCat.setAlertCategory(in.readUnsignedShort());

            // The number of REAL thresholds, there will always be 12 bytes
            currCat.setNumberOfThresholds(in.readUnsignedShort());

            thresholds = new int[6];
            for (int i = 0; i < 6; i++) {
                thresholds[i] = in.readUnsignedShort();
            }

            currCat.setThresholds(thresholds);

            currCat.setProductCode(in.readUnsignedShort());

            this.categories.add(currCat);
        }
    }

    /**
     * @return the categories
     */
    public ArrayList<AAPCategory> getCategories() {
        return categories;
    }

    /**
     * @param categories
     *            the categories to set
     */
    public void setCategories(ArrayList<AAPCategory> categories) {
        this.categories = categories;
    }

    @Override
    public String toString() {
        String s = "AlertAdaptationParameters";
        for (AAPCategory curr : categories) {
            s += curr;
        }
        return s;
    }
}
