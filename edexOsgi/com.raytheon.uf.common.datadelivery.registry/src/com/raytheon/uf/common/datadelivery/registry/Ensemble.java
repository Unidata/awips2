package com.raytheon.uf.common.datadelivery.registry;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlAttribute;
import javax.xml.bind.annotation.XmlRootElement;

import com.raytheon.uf.common.serialization.ISerializableObject;
import com.raytheon.uf.common.serialization.annotations.DynamicSerialize;
import com.raytheon.uf.common.serialization.annotations.DynamicSerializeElement;

/**
 * Handle Ensemble models
 * 
 * <pre>
 * 
 * SOFTWARE HISTORY
 * Date         Ticket#    Engineer    Description
 * ------------ ---------- ----------- --------------------------
 * Mar 20, 2011    357      dhladky     Initial creation
 * 
 * </pre>
 * 
 * @author dhladky
 * @version 1.0
 */

@XmlRootElement
@XmlAccessorType(XmlAccessType.NONE)
@DynamicSerialize
public class Ensemble implements ISerializableObject {

    /**
     * names of the various ensemble members
     */
    @DynamicSerializeElement
    @XmlAttribute
    private List<String> members;

    /**
     * which members to request.
     */
    @DynamicSerializeElement
    @XmlAttribute
    private List<String> selectedMembers;

    public Ensemble() {

    }

    public Ensemble(Ensemble other) {
        if (other.members != null) {
            members = new ArrayList<String>(other.members);
        }
        if (other.selectedMembers != null) {
            selectedMembers = new ArrayList<String>(other.selectedMembers);
        }
    }

    public List<String> getMembers() {
        return members;
    }

    public void setMembers(List<String> members) {
        this.members = members;
    }

    public List<String> getSelectedMembers() {
        return selectedMembers;
    }

    public void setSelectedMembers(List<String> selectedMembers) {
        this.selectedMembers = selectedMembers;
    }

    public int getMemberCount() {
        if (members == null) {
            // if no members are defined then assume a single unnamed member.
            return 1;
        } else {
            return members.size();
        }
    }

    public int getSelectedMemberCount() {
        if (members == null) {
            // if no members are defined then assume a single unnamed member
            // which is automatically selected
            return 1;
        } else if (selectedMembers == null) {
            return 0;
        } else {
            return selectedMembers.size();
        }
    }

    public boolean hasSelection() {
        return selectedMembers != null && !selectedMembers.isEmpty();
    }

    /**
     * 
     * Get the range of indices of the selected members, inclusively. This
     * method should usually be used after a split operation to guarantee that
     * selected ensembles are consecutive
     * 
     * @return a int[2] representing the start and end indices of the ensemble
     *         members
     * @throws IllegalStateException
     *             if the selected member are non consecutive.
     */
    public int[] getSelectedRange() {
        int[] result = new int[2];
        int[] indexes = getSortedIndexes();
        if (indexes == null) {
            return result;
        }
        result[0] = indexes[0];
        result[1] = result[0] - 1;
        for (int index : indexes) {
            result[1] += 1;
            if (result[1] != index) {
                throw new IllegalStateException(
                        "Cannot get selected range for nonconsecutive ensemble members\nMembers "
                                + members + "\nSelected" + selectedMembers);
            }
        }
        return result;
    }

    /**
     * Split this ensemble into multiple Ensembles each with consecutive
     * members. Used in cases where multiple ensembles can be requested at once
     * if they are consecutive.
     * 
     * @param maxEnsembles
     *            The mazximum number of consecutive members in any Ensemble
     * @return
     */
    public List<Ensemble> split(int maxEnsembles) {
        int[] indexes = getSortedIndexes();
        if (indexes == null) {
            return Arrays.asList(new Ensemble(this));
        }
        List<Ensemble> result = new ArrayList<Ensemble>();
        List<String> selected = new ArrayList<String>(selectedMembers.size());
        int start = indexes[0];
        int end = start - 1;
        for (int index : indexes) {
            end += 1;
            if (index != end || (end - start) >= maxEnsembles) {
                // Either we have run out of consecutive indexes or we have the
                // max consecutive. so split off and start the next one.
                Ensemble e = new Ensemble(this);
                e.setSelectedMembers(new ArrayList<String>(selected));
                selected.clear();
                result.add(e);
                start = index;
                end = start;
            }
            selected.add(members.get(index));
        }
        Ensemble e = new Ensemble(this);
        e.setSelectedMembers(selected);
        result.add(e);
        return result;
    }

    private int[] getSortedIndexes() {
        if (!hasSelection()) {
            return null;
        }
        int[] indexes = new int[selectedMembers.size()];
        int c = 0;
        for (String selected : selectedMembers) {
            int index = members.indexOf(selected);
            if (index >= 0) {
                indexes[c++] = index;
            }
        }
        if (c == 0) {
            return null;
        } else if (c < indexes.length) {
            indexes = Arrays.copyOf(indexes, c);
        }
        Arrays.sort(indexes);
        return indexes;
    }

}
