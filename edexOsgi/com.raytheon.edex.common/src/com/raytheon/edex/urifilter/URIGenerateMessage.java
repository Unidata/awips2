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
package com.raytheon.edex.urifilter;

import java.util.Date;

import com.raytheon.uf.common.serialization.ISerializableObject;
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
 * Nov 11, 2010            rjpeter     Initial creation
 * Nov 30, 2012            dhladky     Added queue and dequeue times for stats
 * 
 * </pre>
 * 
 * @author rjpeter
 * @version 1.0
 */
@DynamicSerialize
public class URIGenerateMessage implements ISerializableObject {
    @DynamicSerializeElement
    protected String name = null;

    /** list of URI's you matched */
    @DynamicSerializeElement
    protected String[] uris = null;

    /** time used for matching comparisons */
    @DynamicSerializeElement
    protected Date validTime = null;

    /** time used for matching comparisons */
    @DynamicSerializeElement
    protected Date currentTime = null;

    /** logged time */
    @DynamicSerializeElement
    protected Long enQueuedTime = null;

    /** time used for matching comparisons */
    @DynamicSerializeElement
    protected Long deQueuedTime = null;

    public Long getEnQueuedTime() {
        return enQueuedTime;
    }

    public void setEnQueuedTime(Long enQueuedTime) {
        this.enQueuedTime = enQueuedTime;
    }

    public Long getDeQueuedTime() {
        return deQueuedTime;
    }

    public void setDeQueuedTime(Long deQueuedTime) {
        this.deQueuedTime = deQueuedTime;
    }

    public URIGenerateMessage() {
    }

    public URIGenerateMessage(URIFilter filter) {
        setName(filter.getName());
        setCurrentTime(filter.getCurrentTime());
        setUris(filter.getURIs());
        setValidTime(filter.getValidTime());
        setEnQueuedTime(System.currentTimeMillis());
    }

    /**
     * Find a URI that contains this match string
     * 
     * @param match
     * @return
     */
    public String getURI(String match) {
        for (String uri : uris) {
            String[] comps = uri.split(URIFilter.uriSeperator);
            for (String comp : comps) {
                if (comp.equals(match)) {
                    return uri;
                }
            }
        }

        return null;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String[] getUris() {
        return uris;
    }

    public Date getCurrentTime() {
        return currentTime;
    }

    public void setCurrentTime(Date currentTime) {
        this.currentTime = currentTime;
    }

    public void setUris(String[] uris) {
        this.uris = uris;
    }

    public Date getValidTime() {
        return validTime;
    }

    public void setValidTime(Date validTime) {
        this.validTime = validTime;
    }
}
