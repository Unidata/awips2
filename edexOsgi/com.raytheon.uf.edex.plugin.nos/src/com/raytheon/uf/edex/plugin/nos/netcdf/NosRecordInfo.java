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
package com.raytheon.uf.edex.plugin.nos.netcdf;

import java.io.File;

import com.raytheon.uf.common.gridcoverage.GridCoverage;
import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.edex.plugin.nos.description.NosProductDescription;

/**
 * Holds the information needed to be able to decode a NOS grid record, allowing
 * an NOS file to be split and decoded without having to keep more in memory
 * than is necessary.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Aug 28, 2015 4696       nabowle     Initial creation
 * 
 * </pre>
 * 
 * @author nabowle
 * @version 1.0
 */
public class NosRecordInfo {

    /** The file on disk that is to be decoded. */
    private File file;

    /** The pre-computed grid coverage. */
    private GridCoverage location;

    /** The pre-computed dataTime. */
    private DataTime dataTime;

    /** The index into the level data for this record. */
    private int levelIdx;

    /** The description of this product. */
    private NosProductDescription description;


    /**
     * Constructor.
     */
    public NosRecordInfo() {
        super();
    }

    /**
     * @return the file
     */
    public File getFile() {
        return file;
    }

    /**
     * @param file
     *            the file to set
     */
    public void setFile(File file) {
        this.file = file;
    }

    /**
     * @return the location
     */
    public GridCoverage getLocation() {
        return location;
    }

    /**
     * @param location
     *            the location to set
     */
    public void setLocation(GridCoverage location) {
        this.location = location;
    }

    /**
     * @return the dataTime
     */
    public DataTime getDataTime() {
        return dataTime;
    }

    /**
     * @param dataTime
     *            the dataTime to set
     */
    public void setDataTime(DataTime dataTime) {
        this.dataTime = dataTime;
    }

    /**
     * @return the levelIdx
     */
    public int getLevelIdx() {
        return levelIdx;
    }

    /**
     * @param levelIdx
     *            the levelIdx to set
     */
    public void setLevelIdx(int levelIdx) {
        this.levelIdx = levelIdx;
    }

    /**
     * @return the description
     */
    public NosProductDescription getDescription() {
        return description;
    }

    /**
     * @param description
     *            the description to set
     */
    public void setDescription(NosProductDescription description) {
        this.description = description;
    }

    @Override
    public String toString() {
        return "NosRecordInfo [file=" + file + ", dataTime=" + dataTime
                + ", levelIdx=" + levelIdx + ", description=" + description
                + "]";
    }

}
