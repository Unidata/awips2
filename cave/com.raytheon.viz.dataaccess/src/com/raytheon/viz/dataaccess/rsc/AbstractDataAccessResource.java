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
package com.raytheon.viz.dataaccess.rsc;

import java.util.Arrays;

import org.apache.commons.lang.StringUtils;

import com.raytheon.uf.common.time.DataTime;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.map.MapDescriptor;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Abstracts the rendering of data retrieved using the Data Access Framework.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Jan 31, 2013            bkowal     Initial creation
 * Feb 14, 2013 1614       bsteffen    Refactor data access framework to use
 *                                     single request.
 * Feb 19, 2013 1552       mpduff     Handle empty legend text.
 * 
 * </pre>
 * 
 * @author bkowal
 * @version 1.0
 */

public abstract class AbstractDataAccessResource<T extends AbstractDataAccessResourceData<?>>
        extends AbstractVizResource<T, MapDescriptor> {

    protected static final String _SPACE_ = " ";

    private String genericLegendText = StringUtils.EMPTY;

    /**
     * Constructor
     * 
     * @param resourceData
     *            the data associated with the resource
     * @param loadProperties
     *            the load properties associated with the resource
     * @param genericLegendText
     *            the default legend text associated with the resource
     */
    protected AbstractDataAccessResource(T resourceData,
            LoadProperties loadProperties, String genericLegendText) {
        super(resourceData, loadProperties);
        this.genericLegendText = genericLegendText;
        if (resourceData.getDataTimes() == null) {
            this.dataTimes = TIME_AGNOSTIC;
        } else {
            this.dataTimes = Arrays.asList(resourceData.getDataTimes());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected void initInternal(IGraphicsTarget target) throws VizException {
        DataTime[] timesToLoad = descriptor.getFramesInfo().getTimeMap()
                .get(this);
        if (timesToLoad == null && descriptor.getTimeMatcher() != null) {
            timesToLoad = descriptor.getTimeMatcher().initialLoad(
                    getLoadProperties(), getDataTimes(), descriptor);
        }
        if (timesToLoad != null) {
            for (DataTime time : timesToLoad) {
                prepareData(target, time);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void remove(DataTime dataTime) {
        ;// for now never remove anything from dataTimes since there are no
         // updates on redoTimeMatching
    }

    /*
     * (non-Javadoc)
     * 
     * @see com.raytheon.uf.viz.core.rsc.AbstractVizResource#getName()
     */
    @Override
    public String getName() {
        return buildLegendText(genericLegendText);
    }

    /**
     * Extracts pertinent information from the retrieved data and stores it; the
     * extracted data will generally be used to render the resource
     * 
     * @param target
     * @throws VizException
     */
    protected abstract void prepareData(IGraphicsTarget target, DataTime time)
            throws VizException;

    /**
     * Adds request-type specific information to the default legend text
     * 
     * @return additions to the legend text
     */
    protected abstract String buildLegendTextInternal();

    /**
     * Initializes the legend text.
     * 
     * @param genericLegendText
     *            the request-type specific legend text
     */
    private final String buildLegendText(String genericLegendText) {
        StringBuilder stringBuilder = new StringBuilder();
        if (genericLegendText != null) {
            stringBuilder.append(genericLegendText);
        }
        stringBuilder.append(this.padWithSeparator(this
                .buildLegendTextInternal()));
        return stringBuilder.toString();
    }

    /**
     * Adds an additional space to the end of the specified string if there is
     * not already one present
     * 
     * @param text
     *            the string to potentially pad with a space
     * @return the string padded with a space or the original string
     */
    private String padWithSeparator(String text) {
        if (text.isEmpty() || text.endsWith(_SPACE_)) {
            return text;
        }

        return StringUtils.rightPad(text, text.length() + 1);
    }
}