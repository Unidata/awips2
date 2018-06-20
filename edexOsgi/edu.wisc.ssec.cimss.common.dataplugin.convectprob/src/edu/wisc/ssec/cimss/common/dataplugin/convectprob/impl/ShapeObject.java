package edu.wisc.ssec.cimss.common.dataplugin.convectprob.impl;

/**
 * NOAA/CIMSS Prob Severe Model Shape Object Definition
 *
 * Data object that defines attributes of a NOAA/CIMSS Prob Severe Model shape
 *
 * <pre>
 * SOFTWARE HISTORY
 * Date         Ticket#     Engineer    Description
 * ------------ ----------  ----------- --------------------------
 * Mar 27, 2014 DCS 15298   lcronce     Initial Creation.
 *
 * </pre
 *
 * @author Lee Cronce
 * @version 1.0
 *
 */
public class ShapeObject {

    private String type;

    private int probability;

    private String mucape;

    private String ebshear;

    private String mesh;

    private String rcemiss;

    private String rcicecf;

    private String polygon;

    private String objectid;

    /**
     * Default empty constructor
     */
    public ShapeObject() {
    }

    /**
     * Retrieve type of a shape
     *
     * @return shape type
     */
    public String getType() {
        return type;
    }

    /**
     * Retrieve shape probability of severe
     *
     * @return shape probability of severe
     */
    public int getProbability() {
        return probability;
    }

    /**
     * Retrieve MUCAPE associated with a shape
     *
     * @return shape MUCAPE
     */
    public String getMucape() {
        return mucape;
    }

    /**
     * Retrieve EB Shear associated with a shape
     *
     * @return shape EB Shear
     */
    public String getEbshear() {
        return ebshear;
    }

    /**
     * Retrieve MESH associated with a shape
     *
     * @return shape MESH
     */
    public String getMesh() {
        return mesh;
    }

    /**
     * Retrieve rate of change of emissivity of a shape
     *
     * @return shape emissivity rate of change
     */
    public String getRcemiss() {
        return rcemiss;
    }

    /**
     * Retrieve rate of change of ice cloud fraction of a shape
     *
     * @return shape ice cloud fraction rate of change
     */
    public String getRcicecf() {
        return rcicecf;
    }

    /**
     * Retrieve the polygon defining a shape
     *
     * @return shape polygon
     */
    public String getPolygon() {
        return polygon;
    }

    /**
     * Retried the object ID of a shape
     *
     * @return shape objectID
     */
    public String getObjectid() {
        return objectid;
    }

    /**
     * Set type of a shape
     *
     * @param shape type
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Set shape probability of severe
     *
     * @param shape probability of severe
     */
    public void setProbability(int probability) {
        this.probability = probability;
    }

    /**
     * Set MUCAPE associated with a shape
     *
     * @param shape MUCAPE
     */
    public void setMucape(String mucape) {
        this.mucape = mucape;
    }

    /**
     * Set EB Shear associated with a shape
     *
     * @param shape EB Shear
     */
    public void setEbshear(String ebshear) {
        this.ebshear = ebshear;
    }

    /**
     * Set MESH associated with a shape
     *
     * @param shape MESH
     */
    public void setMesh(String mesh) {
        this.mesh = mesh;
    }

    /**
     * Set rate of change of emissivity of a shape
     *
     * @param shape emissivity rate of change
     */
    public void setRcemiss(String rcemiss) {
        this.rcemiss = rcemiss;
    }

    /**
     * Set rate of change of ice cloud fraction of a shape
     *
     * @param shape ice cloud fraction rate of change
     */
    public void setRcicecf(String rcicecf) {
        this.rcicecf = rcicecf;
    }

    /**
     * Set the polygon defining a shape
     *
     * @param shape polygon
     */
    public void setPolygon(String polygon) {
        this.polygon = polygon;
    }

    /**
     * Set the object ID of a shape
     *
     * @param shape objectID
     */
    public void setObjectid(String objectid) {
        this.objectid = objectid;
    }

}
