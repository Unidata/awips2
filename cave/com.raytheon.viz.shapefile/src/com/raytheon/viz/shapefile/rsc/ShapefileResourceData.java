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
package com.raytheon.viz.shapefile.rsc;

import java.io.File;
import java.io.FileNotFoundException;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;

import org.eclipse.swt.graphics.RGB;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractNameGenerator;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.AbstractVizResource;
import com.raytheon.uf.viz.core.rsc.LoadProperties;
import com.raytheon.uf.viz.core.rsc.capabilities.ColorableCapability;

/**
 * TODO Add Description
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 11, 2009            njensen     Initial creation
 * 
 * </pre>
 * 
 * @author njensen
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
public class ShapefileResourceData extends AbstractResourceData {

    private static final RGB COLOR = new RGB(155, 155, 155);

    @XmlElement
    private String filename;

    @XmlElement
    protected String[] labelFields;

    /** The human readable name */
    @XmlElement
    private String mapName = null;

    private File shapeFile;

    public ShapefileResourceData() {
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

    public ShapefileResourceData(String filename, String[] labelFields) {
        this();
        this.filename = filename;
        this.labelFields = labelFields;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#construct(com.raytheon
     * .uf.viz.core.comm.LoadProperties,
     * com.raytheon.uf.viz.core.drawables.IDescriptor)
     */
    @Override
    public ShapefileResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        shapeFile = new File(filename);
        if (shapeFile == null || shapeFile.exists() == false) {
            throw new VizException("Could not find shapefile",
                    new FileNotFoundException(String.valueOf(shapeFile)));
        }
        ShapefileResource rsc = new ShapefileResource(this, loadProperties);
        rsc.getCapability(ColorableCapability.class).setColor(COLOR);

        return rsc;
    }

    protected File getShapeFile() {
        return shapeFile;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * com.raytheon.uf.viz.core.rsc.AbstractResourceData#update(java.lang.Object
     * )
     */
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

    /**
     * @return the labelFields
     */
    public String[] getLabelFields() {
        return labelFields;
    }

    /**
     * @param labelFields
     *            the labelFields to set
     */
    public void setLabelFields(String[] labelFields) {
        this.labelFields = labelFields;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null || obj instanceof ShapefileResourceData == false) {
            return false;
        }
        ShapefileResourceData other = (ShapefileResourceData) obj;

        if (this.mapName != null && other.mapName == null) {
            return false;
        } else if (this.mapName == null && other.mapName != null) {
            return false;
        } else if (this.mapName != null
                && this.mapName.equals(other.mapName) == false) {
            return false;
        }

        if (this.filename != null && other.filename == null) {
            return false;
        } else if (this.filename == null && other.filename != null) {
            return false;
        } else if (this.filename != null
                && this.filename.equals(other.filename) == false) {
            return false;
        }

        return true;
    }

}
