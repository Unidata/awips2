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
package com.raytheon.viz.bcd;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.maps.rsc.AbstractMapResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Implements BCD metadata and construction factory
 *
 * <pre>
 *
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            njensen     Initial creation
 * May 31, 2018 6562       tgurney     extends AbstractMapResourceData
 *
 * </pre>
 *
 * @author njensen
 */

@XmlAccessorType(XmlAccessType.NONE)
public class BCDResourceData extends AbstractMapResourceData {

    /** The filename */
    @XmlElement
    private String filename = null;

    public BCDResourceData() {
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

    public BCDResourceData(final String filename) {
        this();
        this.filename = filename;

    }

    @Override
    public BCDResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new BCDResource(this, loadProperties);
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
        BCDResourceData other = (BCDResourceData) obj;

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
