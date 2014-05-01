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
package com.raytheon.uf.common.pypies.request;

import com.raytheon.uf.common.datastorage.StorageProperties.Compression;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 17, 2012            rjpeter     Initial creation
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */

@DynamicSerialize
public class CopyRequest extends AbstractRequest {
    @DynamicSerializeElement
    private boolean repack;

    @DynamicSerializeElement
    private Compression repackCompression;

    @DynamicSerializeElement
    private String outputDir;

    @DynamicSerializeElement
    private String timestampCheck;

    @DynamicSerializeElement
    private int minMillisSinceLastChange;

    @DynamicSerializeElement
    private int maxMillisSinceLastChange;

    /**
     * @return the repack
     */
    public boolean isRepack() {
        return repack;
    }

    /**
     * @param repack
     *            the repack to set
     */
    public void setRepack(boolean repack) {
        this.repack = repack;
    }

    /**
     * @return the repackCompression
     */
    public Compression getRepackCompression() {
        return repackCompression;
    }

    /**
     * @param repackCompression
     *            the repackCompression to set
     */
    public void setRepackCompression(Compression repackCompression) {
        this.repackCompression = repackCompression;
    }

    /**
     * @return the outputDir
     */
    public String getOutputDir() {
        return outputDir;
    }

    /**
     * @param outputDir
     *            the outputDir to set
     */
    public void setOutputDir(String outputDir) {
        this.outputDir = outputDir;
    }

    /**
     * @return the timestampCheck
     */
    public String getTimestampCheck() {
        return timestampCheck;
    }

    /**
     * @param timestampCheck
     *            the timestampCheck to set
     */
    public void setTimestampCheck(String timestampCheck) {
        this.timestampCheck = timestampCheck;
    }

    /**
     * @return the minMillisSinceLastChange
     */
    public int getMinMillisSinceLastChange() {
        return minMillisSinceLastChange;
    }

    /**
     * @param minMillisSinceLastChange
     *            the minMillisSinceLastChange to set
     */
    public void setMinMillisSinceLastChange(int minMillisSinceLastChange) {
        this.minMillisSinceLastChange = minMillisSinceLastChange;
    }

    /**
     * @return the maxMillisSinceLastChange
     */
    public int getMaxMillisSinceLastChange() {
        return maxMillisSinceLastChange;
    }

    /**
     * @param maxMillisSinceLastChange
     *            the maxMillisSinceLastChange to set
     */
    public void setMaxMillisSinceLastChange(int maxMillisSinceLastChange) {
        this.maxMillisSinceLastChange = maxMillisSinceLastChange;
    }
}
