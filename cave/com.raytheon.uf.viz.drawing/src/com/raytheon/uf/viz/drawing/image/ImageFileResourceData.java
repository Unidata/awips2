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
package com.raytheon.uf.viz.drawing.image;

import java.io.File;

import javax.annotation.Generated;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;

import com.raytheon.uf.viz.core.drawables.IDescriptor;
import com.raytheon.uf.viz.core.exception.VizException;
import com.raytheon.uf.viz.core.rsc.AbstractResourceData;
import com.raytheon.uf.viz.core.rsc.LoadProperties;

/**
 * Resource data for displaying a non geolocated image, contains an image file
 * name and position.
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * 
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Jan 15, 2014  2313     bsteffen    Initial creation
 * 
 * </pre>
 * 
 * @author bsteffen
 * @version 1.0
 */
@XmlAccessorType(XmlAccessType.NONE)
public class ImageFileResourceData extends AbstractResourceData {

    @XmlAttribute
    private String fileName;

    @XmlAttribute
    private double north = 0;

    @XmlAttribute
    private double east = 100;

    @XmlAttribute
    private double south = 100;

    @XmlAttribute
    private double west = 0;


    public ImageFileResourceData() {
        super();
    }

    public ImageFileResourceData(String fileName) {
        this();
        this.fileName = fileName;
    }

    @Override
    public ImageFileResource construct(LoadProperties loadProperties,
            IDescriptor descriptor) throws VizException {
        return new ImageFileResource(this, loadProperties);
    }

    public String getFileName() {
        return fileName;
    }

    public void setFileName(String fileName) {
        this.fileName = fileName;
    }

    public double getNorth() {
        return north;
    }

    public void setNorth(double north) {
        this.north = north;
    }

    public double getEast() {
        return east;
    }

    public void setEast(double east) {
        this.east = east;
    }

    public double getSouth() {
        return south;
    }

    public void setSouth(double south) {
        this.south = south;
    }

    public double getWest() {
        return west;
    }

    public void setWest(double west) {
        this.west = west;
    }

    public File getFile() {
        return new File(fileName);
    }

    @Override
    public void update(Object updateData) {
        // Nothing updates this resource
    }

    @Override
    @Generated(value = "org.eclipse.jdt.ui.actions.GenerateHashCodeEqualsAction", comments = "3.8.2.v20130107-165834")
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        long temp;
        temp = Double.doubleToLongBits(east);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        result = prime * result
                + ((fileName == null) ? 0 : fileName.hashCode());
        temp = Double.doubleToLongBits(north);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(south);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        temp = Double.doubleToLongBits(west);
        result = prime * result + (int) (temp ^ (temp >>> 32));
        return result;
    }

    @Override
    @Generated(value = "org.eclipse.jdt.ui.actions.GenerateHashCodeEqualsAction", comments = "3.8.2.v20130107-165834")
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        ImageFileResourceData other = (ImageFileResourceData) obj;
        if (Double.doubleToLongBits(east) != Double
                .doubleToLongBits(other.east))
            return false;
        if (fileName == null) {
            if (other.fileName != null)
                return false;
        } else if (!fileName.equals(other.fileName))
            return false;
        if (Double.doubleToLongBits(north) != Double
                .doubleToLongBits(other.north))
            return false;
        if (Double.doubleToLongBits(south) != Double
                .doubleToLongBits(other.south))
            return false;
        if (Double.doubleToLongBits(west) != Double
                .doubleToLongBits(other.west))
            return false;
        return true;
    }


}
