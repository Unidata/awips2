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

package com.raytheon.uf.common.dataplugin.ccfp;

import javax.persistence.Column;
import javax.persistence.Embeddable;

import org.hibernate.annotations.Type;
import org.opengis.referencing.crs.CoordinateReferenceSystem;

import com.raytheon.uf.common.dataplugin.annotations.DataURI;
import com.raytheon.uf.common.geospatial.ISpatialObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;
import com.vividsolutions.jts.geom.Geometry;

/**
 * 
 * CCFP Location
 * 
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Sep 25, 2008 1532        bphillip    initial creation
 * Sep 16, 2009 3027        njensen     Moved dataURI off geometry
 * Sep 17, 2009 3072        bsteffen    Fixed type of geometry
 * Jul 16, 2013 2181        bsteffen    Convert geometry types to use hibernate-
 *                                      spatial
 * Nov 01, 2013 2361        njensen     Remove XML annotations
 * Apr 15, 2014 3001        bgonzale    Refactored to common package,
 *                                      com.raytheon.uf.common.dataplugin.ccfp.
 * 
 * 
 * </pre>
 * 
 * @author bwoodle
 * @version 1
 */
@Embeddable
@DynamicSerialize
public class CcfpLocation implements ISpatialObject {

    private static final long serialVersionUID = 8890315829188793187L;

    @Column(name = "location")
    @Type(type = "org.hibernatespatial.GeometryUserType")
    @DynamicSerializeElement
    private Geometry geometry;

    @DataURI(position = 0)
    @Column
    @DynamicSerializeElement
    private double boxLat;

    @DataURI(position = 1)
    @Column
    @DynamicSerializeElement
    private double boxLong;

    @Override
    public CoordinateReferenceSystem getCrs() {
        return null;
    }

    @Override
    public Geometry getGeometry() {
        return geometry;
    }

    @Override
    public Integer getNx() {
        return 0;
    }

    @Override
    public Integer getNy() {
        return 0;
    }

    public double getBoxLat() {
        return boxLat;
    }

    public void setBoxLat(double boxLat) {
        this.boxLat = boxLat;
    }

    public double getBoxLong() {
        return boxLong;
    }

    public void setBoxLong(double boxLong) {
        this.boxLong = boxLong;
    }

    public void setGeometry(Geometry geometry) {
        this.geometry = geometry;
    }
}
