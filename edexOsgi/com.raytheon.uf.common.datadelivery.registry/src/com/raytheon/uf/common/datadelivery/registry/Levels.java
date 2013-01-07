package com.raytheon.uf.common.datadelivery.registry;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlElements;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Levels in Retrieval
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Feb 08, 2011    191      dhladky     Initial creation
 * Jul 24, 2012    955      djohnson    Use List instead of ArrayList.
 * Nov 19, 2012 1166       djohnson     Clean up JAXB representation of registry objects.
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Levels implements ISerializableObject, Serializable {

    private static final long serialVersionUID = -7287908380344547929L;

    public Levels() {

    }

    @XmlAttribute
    @DynamicSerializeElement
    private String name;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer levelType;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer requestLevelEnd;

    @XmlAttribute
    @DynamicSerializeElement
    private Integer requestLevelStart;

    @XmlElements({ @XmlElement(name = "level") })
    @DynamicSerializeElement
    private List<Double> level;

    @XmlAttribute
    @DynamicSerializeElement
    private Double dz;

    @XmlElements({ @XmlElement(name = "selectedLevelIndices", type = Integer.class) })
    @DynamicSerializeElement
    private List<Integer> selectedLevelIndices;

    public Integer getLevelType() {
        return levelType;
    }

    public void setLevelType(Integer levelType) {
        this.levelType = levelType;
    }

    public Double getLevelStart() {
        return level.get(0);
    }

    public Double getLevelEnd() {
        return level.get(getLevel().size());
    }

    public Integer getRequestLevelEnd() {
        return requestLevelEnd;
    }

    public void setRequestLevelEnd(Integer requestLevelEnd) {
        this.requestLevelEnd = requestLevelEnd;
    }

    public Integer getRequestLevelStart() {
        return requestLevelStart;
    }

    public void setRequestLevelStart(Integer requestLevelStart) {
        this.requestLevelStart = requestLevelStart;
    }

    public Double getRequestLevelStartValue() {
        return level.get(requestLevelStart);
    }

    public Double getRequestLevelEndValue() {
        return level.get(requestLevelEnd);
    }

    public List<Double> getLevel() {
        return level;
    }

    public Double getLevelAt(int i) {
        return level.get(i);
    }

    public void setLevel(List<Double> level) {
        this.level = level;
    }

    public void addLevel(Double lev) {
        if (level == null) {
            level = new ArrayList<Double>();
        }
        level.add(lev);
    }

    public void removeLevel(Double lev) {
        level.remove(lev);
    }

    public Double getDz() {
        return dz;
    }

    public void setDz(Double dz) {
        this.dz = dz;
    }

    public int size() {
        return level.size();
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }



    /**
     * @return the selectedLevelIndices
     */
    public List<Integer> getSelectedLevelIndices() {
        return selectedLevelIndices;
    }

    /**
     * @param selectedLevelIndices
     *            the selectedLevelIndices to set
     */
    public void setSelectedLevelIndices(List<Integer> selectedLevelIndices) {
        this.selectedLevelIndices = selectedLevelIndices;
    }

	/**
	 * Gets the level breakups needed to split retrievals for a subscription
	 * 
	 * @return
	 */
    public List<List<Integer>> getLevelSequences(int sfactor) {
		
        List<List<Integer>> sequences = new ArrayList<List<Integer>>();
        List<Integer> al = new ArrayList<Integer>();
		
		if (selectedLevelIndices.size() > 0) {
			int previous = selectedLevelIndices.get(0);
			al.add(previous);
			for (int i = 1; i < selectedLevelIndices.size(); i++) {
				int next = selectedLevelIndices.get(i);
				if (next - previous == 1 && al.size() <= sfactor) {
					al.add(next);
					previous = next;
				} else {
					sequences.add(al);
					al = new ArrayList<Integer>();
					al.add(next);
					previous = next;
				}
			}

			sequences.add(al);
		}

		return sequences;
    }
}
