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
package com.raytheon.viz.spi;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Implements SPI metadata and construction factory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 23, 2009            chammack     Initial creation
 * May 31, 2018 6562       tgurney     extends AbstractMapResourceData
 *
 * </pre>
 *
 * @author chammack
 */
@XmlAccessorType(XmlAccessType.NONE)
public class SPIResourceData extends AbstractMapResourceData {

    /** The filename */
    @XmlElement
    private String filename = null;

    /** The "size" of the plot, used when calculating progressive disclosure */
    @XmlAttribute
    private int pixelSizeHint = 90;

    public SPIResourceData() {
        super();
        this.nameGenerator = new AbstractNameGenerator() {

            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                if (mapName != null) {
                    return mapName;
                }

                return filename;
            }

        };
    }

    public SPIResourceData(final String filename) {
        this();
        this.filename = filename;

    }

    @Override
    public SPIResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        SPIResource rsc = new SPIResource(this, loadProperties);
        rsc.setPixelSizeHint(pixelSizeHint);
        return rsc;
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub

    }

    /**
     * @return the filename
     */
    public String getFilename() {
        return filename;
    }

    /**
     * @param filename
     *            the filename to set
     */
    public void setFilename(String filename) {
        this.filename = filename;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (obj.getClass() != getClass()) {
            return false;
        }
        SPIResourceData other = (SPIResourceData) obj;

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && !this.mapName.equals(other.mapName)) {
            return false;
        }

        if (this.filename != null && other.filename == null) {
            return false;
        } else if (this.filename == null && other.filename != null) {
            return false;
        } else if (this.filename != null
                && !this.filename.equals(other.filename)) {
            return false;
        }

        return true;
    }

}
