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
package com.raytheon.uf.edex.plugin.text;

import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.common.dataplugin.PluginDataObject;
import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * A TextRecord is a PluginDataObject representation of a StdTextProduct. Used
 * for notifications when following the dataURI pattern.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Dec 05, 2008            jkorman     Initial creation
 * May 07, 2013 1869       bsteffen    Remove dataURI column from
 *                                     PluginDataObject.
 * Aug 30, 2013 2298       rjpeter     Make getPluginName abstract
 * May 12, 2014 2536       bclement    removed IDecoderGettable
 * Oct 10, 2014 3549       njensen     Remove unnecessary Column annotations
 * Mar  4, 2016 4716       rferrel     Added AWIPS product Id.
 * 
 * </pre>
 * 
 * @author jkorman
 * @version 1.0
 */
@DynamicSerialize
public class TextRecord extends PluginDataObject {

    private static final long serialVersionUID = 1L;

    // Correction indicator from wmo header
    @DataURI(position = 1)
    @DynamicSerializeElement
    @XmlElement
    private String productId;

    // Correction indicator from wmo header
    @DataURI(position = 2)
    @DynamicSerializeElement
    @XmlElement
    private String awipsProductId;

    /**
     * 
     */
    public TextRecord() {
    }

    /**
     * Constructor for DataURI construction through base class. This is used by
     * the notification service.
     * 
     * @param uri
     *            A data uri applicable to this class.
     */
    public TextRecord(String uri) {
        super(uri);
    }

    /**
     * @return the productId
     */
    public String getProductId() {
        return productId;
    }

    /**
     * @param productId
     *            the productId to set
     */
    public void setProductId(String productId) {
        this.productId = productId;
    }

    public String getAwipsProductId() {
        return this.awipsProductId;
    }

    public void setAwipsProductId(String awipsProductId) {
        this.awipsProductId = awipsProductId;
    }

    @Override
    public String getPluginName() {
        return "text";
    }
}
