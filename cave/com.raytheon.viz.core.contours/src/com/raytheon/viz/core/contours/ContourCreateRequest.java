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
package com.raytheon.viz.core.contours;

import org.geotools.coverage.grid.GeneralGridGeometry;

import com.raytheon.uf.common.numeric.source.DataSource;
import com.raytheon.uf.common.style.contour.ContourPreferences;
import com.raytheon.uf.viz.core.IExtent;
import com.raytheon.uf.viz.core.IGraphicsTarget;
import com.raytheon.uf.viz.core.map.IMapDescriptor;
import com.raytheon.viz.core.contours.ContourSupport.ContourGroup;

/**
 * ContourCreateRequest
 * 
 * A request containing all the data needed for a ContourManagerJob to create
 * contours. The created contours are placed back into this object so keep a
 * reference around to check for finished contours.
 * 
 * <pre>
 * 
 *    SOFTWARE HISTORY
 *   
 * Date          Ticket#  Engineer    Description
 * ------------- -------- ----------- --------------------------
 * Feb 25, 2011           ekladstrup  Initial creation
 * Feb 27, 2014  2791     bsteffen    Switch from IDataRecord to DataSource
 * 
 * 
 * </pre>
 * 
 * @author ekladstrup
 * @version 1
 */
public class ContourCreateRequest {

    public ContourCreateRequest(String identifier, DataSource[] source,
            float level, IExtent pixelExtent, double currentDensity,
            double currentMagnification, GeneralGridGeometry imageGridGeometry,
            IGraphicsTarget target, IMapDescriptor descriptor,
            ContourPreferences prefs, float zoom) {
        super();
        this.identifier = identifier;
        this.source = source;
        this.level = level;
        this.pixelExtent = pixelExtent;
        this.imageGridGeometry = imageGridGeometry;
        this.target = target;
        this.descriptor = descriptor;
        this.prefs = prefs;
        this.currentDensity = currentDensity;
        this.currentMagnification = currentMagnification;
        this.zoom = zoom;
        this.canceled = false;
        this.contourGroup = null;
    }

    public String getIdentifier() {
        return identifier;
    }

    public void setIdentifier(String identifier) {
        this.identifier = identifier;
    }

    public DataSource[] getSource() {
        return source;
    }

    public void setSource(DataSource[] source) {
        this.source = source;
    }

    public float getLevel() {
        return level;
    }

    public void setLevel(float level) {
        this.level = level;
    }

    public IExtent getPixelExtent() {
        return pixelExtent;
    }

    public void setPixelExtent(IExtent pixelExtent) {
        this.pixelExtent = pixelExtent;
    }

    public double getCurrentDensity() {
        return currentDensity;
    }

    public void setCurrentDensity(double currentDensity) {
        this.currentDensity = currentDensity;
    }

    public double getCurrentMagnification() {
        return currentMagnification;
    }

    public void setCurrentMagnification(double currentMagnification) {
        this.currentMagnification = currentMagnification;
    }

    public GeneralGridGeometry getImageGridGeometry() {
        return imageGridGeometry;
    }

    public void setImageGridGeometry(GeneralGridGeometry imageGridGeometry) {
        this.imageGridGeometry = imageGridGeometry;
    }

    public IGraphicsTarget getTarget() {
        return target;
    }

    public void setTarget(IGraphicsTarget target) {
        this.target = target;
    }

    public IMapDescriptor getDescriptor() {
        return descriptor;
    }

    public void setDescriptor(IMapDescriptor descriptor) {
        this.descriptor = descriptor;
    }

    public ContourPreferences getPrefs() {
        return prefs;
    }

    public void setPrefs(ContourPreferences prefs) {
        this.prefs = prefs;
    }

    public float getZoom() {
        return zoom;
    }

    public void setZoom(float zoom) {
        this.zoom = zoom;
    }

    public boolean isCanceled() {
        return canceled;
    }

    public void setCanceled(boolean cancel) {
        this.canceled = cancel;
    }

    public ContourGroup getContourGroup() {
        return contourGroup;
    }

    public synchronized void setContourGroup(ContourGroup contourGroup) {
        // synchronized so disposed can't get set after it is checked
        if (!this.disposed) {
            this.contourGroup = contourGroup;
        } else {
            contourGroup.negValueShape.dispose();
            contourGroup.posValueShape.dispose();
        }
    }

    private String identifier;

    private DataSource[] source;

    private float level;

    private IExtent pixelExtent;

    private double currentDensity;

    private double currentMagnification;

    private GeneralGridGeometry imageGridGeometry;

    private IGraphicsTarget target;

    private IMapDescriptor descriptor;

    private ContourPreferences prefs;

    private float zoom;

    private boolean canceled;

    private ContourGroup contourGroup;

    private boolean disposed = false;

    public boolean equals(Object arg) {
        boolean rval = true;

        if (!(arg instanceof ContourCreateRequest)) {
            return false;
        }

        ContourCreateRequest rhs = (ContourCreateRequest) arg;

        // right now comparing:
        // identifier, level, pixelExtent, currentDensity, currentMagnification,
        // prefs, zoom, imageGridGeometry, mapGridGeometry
        // NOT COMPARING:
        // record, worldGridToCRSTransform, target, descriptor
        // Should never use canceled or contourGroup to check equality
        if (identifier != null && !identifier.equals(rhs.getIdentifier())) {
            rval = false;
        } else if (!(level == rhs.getLevel())) {
            rval = false;
        } else if (pixelExtent != null
                && !(pixelExtent.equals(rhs.getPixelExtent()))) {
            rval = false;
        } else if (!(currentDensity == rhs.getCurrentDensity())) {
            rval = false;
        } else if (!(currentMagnification == rhs.getCurrentMagnification())) {
            rval = false;
        } else if (prefs != null && !(this.prefs.equals(rhs.getPrefs()))) {
            rval = false;
        } else if (!(zoom == rhs.getZoom())) {
            rval = false;
        } else if (imageGridGeometry != null
                && !(this.imageGridGeometry.equals(rhs.getImageGridGeometry()))) {
            rval = false;
        }

        // if nothing set rval to false (not equal)
        // check that references to null are the same
        // if this instances references are not null then
        // rhs with null references should be caught above
        // so only worry about this.* == null and rhs.* != null
        if (rval == true) {
            if (identifier == null && rhs.getIdentifier() != null) {
                rval = false;
            } else if (pixelExtent == null && rhs.getPixelExtent() != null) {
                rval = false;
            } else if (prefs == null && rhs.getPrefs() != null) {
                rval = false;
            } else if (imageGridGeometry == null
                    && rhs.getImageGridGeometry() != null) {
                rval = false;
            }
        }

        return rval;
    }

    public synchronized void dispose() {
        // synchronized to avoid leaking memory because of a missed dispose
        this.disposed = true;
        if (contourGroup != null) {
            this.contourGroup.posValueShape.dispose();
            this.contourGroup.negValueShape.dispose();
            this.contourGroup = null;
        }
    }
}
