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
package com.raytheon.uf.viz.remote.graphics.events.wireframe;

import java.util.LinkedList;
import java.util.List;

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
 * Mar 26, 2012            mschenke     Initial creation
 * 
 * </pre>
 * 
 * @author mschenke
 * @version 1.0
 */
@DynamicSerialize
public class WireframeShapeData {

    @DynamicSerialize
    public static class Label {

        @DynamicSerializeElement
        private String text;

        @DynamicSerializeElement
        private double[] point;

        private Label(String text, double[] point) {
            this.text = text;
            this.point = point;
        }

        public Label() {

        }

        /**
         * @return the text
         */
        public String getText() {
            return text;
        }

        /**
         * @param text
         *            the text to set
         */
        public void setText(String text) {
            this.text = text;
        }

        /**
         * @return the point
         */
        public double[] getPoint() {
            return point;
        }

        /**
         * @param point
         *            the point to set
         */
        public void setPoint(double[] point) {
            this.point = point;
        }
    }

    @DynamicSerializeElement
    private List<double[][]> coordinates = new LinkedList<double[][]>();

    @DynamicSerializeElement
    private List<Label> labels = new LinkedList<Label>();

    /**
     * @return the labels
     */
    public List<Label> getLabels() {
        return labels;
    }

    /**
     * @param labels
     *            the labels to set
     */
    public void setLabels(List<Label> labels) {
        this.labels = labels;
    }

    /**
     * @return the coordinates
     */
    public List<double[][]> getCoordinates() {
        return coordinates;
    }

    /**
     * @param coordinates
     *            the coordinates to set
     */
    public void setCoordinates(List<double[][]> coordinates) {
        this.coordinates = coordinates;
    }

    public void addLabel(String text, double[] point) {
        labels.add(new Label(text, point));
    }

    public void addCoordinates(double[][] points) {
        coordinates.add(points);
    }
}
