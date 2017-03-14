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
package com.raytheon.uf.viz.cloudheight.data;

import java.io.File;

import javax.xml.bind.JAXB;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.localization.PathManagerFactory;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;

/**
 * Port of awips 1 file HH_Parameters.txt
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date          Ticket#    Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Dec 16, 2009           mschenke    Initial creation
 * Jul 25, 2013  2190     mschenke    Moved sounding configurations into popup
 *                                    skewt plugin
 * Oct 24, 2013  2491     bsteffen    Remove ISerializableObject
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
public class CloudHeightData {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(CloudHeightData.class);

    public static final String CLOUDHEIGHT_DATA_DIR = "cloudheight";

    private static final String DATA_FILE = CLOUDHEIGHT_DATA_DIR
            + File.separator + "values.xml";

    private static CloudHeightData theData = null;

    public static enum DisplayOption {
        NONE, PEAK, PREDOMINANT, LOW;
    }

    @XmlElement
    private float nx;

    @XmlElement
    private float ny;

    @XmlElement
    private DisplayOption displayOption;

    public static synchronized CloudHeightData getCloudHeightData() {
        if (theData == null) {
            theData = new CloudHeightData();
            populateData(theData, PathManagerFactory.getPathManager()
                    .getStaticFile(DATA_FILE));
        }
        return theData;
    }

    /**
     * 
     */
    private static void populateData(CloudHeightData data, File dataFile) {
        try {
            CloudHeightData serializedData = JAXB.unmarshal(dataFile,
                    CloudHeightData.class);
            theData.displayOption = serializedData.displayOption;
            theData.nx = serializedData.nx;
            theData.ny = serializedData.ny;
            if ((int) Math.floor(theData.getNx() * theData.getNy()) > 6400) {
                statusHandler
                        .handle(Priority.VERBOSE,
                                "Nx*Ny for cloud height data totals more than 6400, defaulting to 25x25");
                theData.setNx(25.0f);
                theData.setNy(25.0f);
            }
        } catch (Exception e) {
            statusHandler.handle(Priority.PROBLEM,
                    "Error deserializing CloudHeightData xml", e);
        }
    }

    public float getNx() {
        return nx;
    }

    public void setNx(float nx) {
        this.nx = nx;
    }

    public float getNy() {
        return ny;
    }

    public void setNy(float ny) {
        this.ny = ny;
    }

    public DisplayOption getDisplayOption() {
        return displayOption;
    }

    public void setDisplayOption(DisplayOption displayOption) {
        this.displayOption = displayOption;
    }

}
