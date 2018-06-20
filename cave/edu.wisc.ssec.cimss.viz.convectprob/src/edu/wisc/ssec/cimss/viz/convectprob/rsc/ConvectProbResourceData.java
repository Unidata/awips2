package edu.wisc.ssec.cimss.viz.convectprob.rsc;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.status.IUFStatusHandler;
import com.raytheon.uf.common.status.UFStatus;
import com.raytheon.uf.common.status.UFStatus.Priority;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

import edu.wisc.ssec.cimss.common.dataplugin.convectprob.ConvectProbRecord;

/**
 * NOAA/CIMSS Prob Severe Model Visualization Resource Data
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ConvectProbResourceData extends AbstractRequestableResourceData {

    private static final transient IUFStatusHandler statusHandler = UFStatus
            .getHandler(ConvectProbResourceData.class);

    // This flag determines if we draw polygons
    @XmlAttribute
    private boolean displayShape = true;

    // This flag determines if object IDs are output to screen during inspect
    @XmlAttribute
    private boolean showObjectId = false;

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#equals(java.lang.Object)
     */
    @Override
    public boolean equals(Object obj) {
        if (!super.equals(obj)) {
            return false;
        }

        if (obj instanceof ConvectProbResourceData == false) {
            return false;
        }

        ConvectProbResourceData other = (ConvectProbResourceData) obj;

        if (other.displayShape != this.displayShape) {
            return false;
        }

        if (other.showObjectId != this.showObjectId) {
            return true;
        }

        return true;
    }

    /**
     * @see com.raytheon.uf.viz.core.rsc.AbstractRequestableResourceData#constructResource(com.raytheon.uf.viz.core.rsc.LoadProperties, com.raytheon.uf.common.dataplugin.PluginDataObject[])
     */
    @Override
    protected AbstractVizResource<?, ?> constructResource(
            LoadProperties loadProperties, PluginDataObject[] objects)
                    throws VizException {
        ConvectProbResource rsc = new ConvectProbResource(this, loadProperties);
        for (PluginDataObject o : objects) {
            if (o instanceof ConvectProbRecord) {
                ConvectProbRecord rec = (ConvectProbRecord) o;
                rsc.addRecord(rec);
            } else {
                statusHandler.handle(Priority.PROBLEM,
                        "Received wrong type of data.  Got: " + o.getClass()
                        + " Expected: " + ConvectProbRecord.class);
            }
        }
        return rsc;
    }

    /**
     * Flag to see if shape should be displayed
     *
     * @return boolean
     */
    public boolean isDisplayShape() {
        return displayShape;
    }

    /**
     * Set flag for displaying shape
     *
     * @param boolean
     */
    public void setDisplayShape(boolean displayShape) {
        this.displayShape = displayShape;
    }

    /**
     * Flag to see if object ID should be displayed
     *
     * @return boolean
     */
    public boolean isShowObjectId() {
        return showObjectId;
    }

    /**
     * Set flag for displaying shape
     *
     * @param boolean
     */
    public void setShowObjectId(boolean showObjectId) {
        this.showObjectId = showObjectId;
    }

}
