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
package com.raytheon.viz.geotiff.rsc;

import java.io.IOException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 18, 2009            bgonzale     Initial creation
 * 
 * </pre>
 * 
 * @author bgonzale
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class GeoTiffResourceData extends AbstractResourceData {

    /** The file where the image is stored */
    @XmlElement
    private String fileName;

    /** The name of the layer */
    @XmlElement
    private String name;

    public GeoTiffResourceData() {
        this.nameGenerator = new AbstractNameGenerator() {
            @Override
            public String getName(AbstractVizResource<?, ?> resource) {
                return (name != null) ? name : fileName;
            }
        };
    }

    public GeoTiffResourceData(final String filename) {
        this();
        this.fileName = filename;
    }

    @Override
    public GeoTiffResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        try {
            return new GeoTiffResource(this, loadProperties);
        } catch (IOException e) {
            throw new VizException(
                    "Failed to initialize a GeoTiffResource for file "
                            + fileName, e);
        }
    }

    @Override
    public void update(Object updateData) {
        // TODO Auto-generated method stub
    }

    /**
     * @return the filename
     */
    public String getFilename() {
        return fileName;
    }

    /**
     * @param filename
     *            the filename to set
     */
    public void setFilename(String filename) {
        this.fileName = filename;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof GeoTiffResourceData == false) {
            return false;
        }
        GeoTiffResourceData other = (GeoTiffResourceData) obj;

        if (this.fileName != null && other.fileName == null) {
            return false;
        } else if (this.fileName == null && other.fileName != null) {
            return false;
        } else if (this.fileName != null
                && this.fileName.equals(other.fileName) == false) {
            return false;
        }

        if (this.name != null && other.name == null) {
            return false;
        } else if (this.name == null && other.name != null) {
            return false;
        } else if (this.name != null && this.name.equals(other.name) == false) {
            return false;
        }

        return true;
    }

}
